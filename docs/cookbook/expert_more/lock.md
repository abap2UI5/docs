---
outline: [2, 4]
samples:
  - z2ui5_cl_smps_app_485
  - z2ui5_cl_smps_app_490
description: Eight ways to keep two users from overwriting each other in a stateless app - from no lock and an enqueue at save to an optimistic check, a stateful session and RAP drafts.
---
# Lock

In classic SAP GUI, a transaction like `VA02` calls `ENQUEUE_EVVBAK` and the lock lives as long as the dialog session. Web apps are different: each roundtrip is a fresh ABAP session by default, so a lock set in one roundtrip is gone by the next. Locking in abap2UI5 means picking a deliberate strategy for two questions:

| Phase | Question |
|---|---|
| **Edit** | What happens while the user is thinking and typing? |
| **Save** | What happens the moment they hit save? |

The patterns below combine in different ways. The examples use the sales order header table `VBAK` and its standard enqueue object `EVVBAK`, but the same shapes apply to any table. For patterns 1 to 6 each section ends with a link to the full class on [Lock: Full Source](/cookbook/expert_more/lock_sources). Patterns 7 and 8 point at the RAP draft chapter and at the add-on that implement them.

![Two users open the same record and read timestamp T1; user A saves and the record is at T2; user B saves still holding T1 - without a check B overwrites A, with the optimistic check the save is refused](/cookbook/expert_more/lock-timeline.svg)

## Which Strategy?

Eight ways to keep two users from overwriting each other, from none at all to
a framework that does it for you. Pick from the table, then read that one
strategy below; every full class is folded under its section.

| # | Strategy | "Locked by X" while editing | Catches external writes | Stateless | Best fit |
|---|---|---|---|---|---|
| 1 | No Locking | no | no | yes | Demos, sandboxes |
| 2 | Lock at Save | no | partial (race window) | yes | Single-app writers, low contention |
| 3 | Optimistic Check | no | yes (timestamp) | yes | Many concurrent users |
| 4 | Combined | no | yes | yes | **Production default** |
| 5 | Stateful Session | yes (enqueue) | yes | no (pins work process) | GUI-like feel, few users |
| 6 | Soft Lock | yes (Z table) | only via underlying guard | yes | "Locked by Alice" UX |
| 7 | RAP Drafts | yes (RAP-managed) | yes | yes | A released draft-enabled BO exists |
| 8 | Lock-Manager Add-on | yes | yes | yes | Skip the boilerplate |

Start with **(4) Combined** unless one of these tips the balance:
- The app is read-only → no lock needed
- A released, draft-enabled SAP BO already covers your object → **(7) RAP Drafts**
- You need a "locked by X since…" message at open → add **(6) Soft Lock** on top of (4), or use **(5) Stateful Session** for few users with a GUI-like feel
- A lock-manager add-on exists for your platform → **(8) Lock-Manager Add-on**

For the underlying concepts and trade-offs of statefulness, see [Statefulness](/cookbook/expert_more/statefulness).

## 1. No Locking
The minimal starting point — the user edits and saves, no lock and no conflict check. Last save wins, silently. Fine for personal sandboxes and throwaway demos, but rarely what you want in production.

→ Full source: [`ZCL_SAMPLE_LOCK_1`](/cookbook/expert_more/lock_sources#lock-1)

## 2. Lock at Save
Do not hold a lock while the user thinks. Acquire it the moment they save, write, commit, and release — all in one short roundtrip:
```abap
METHOD on_event_save.

  CALL FUNCTION `ENQUEUE_EVVBAK`
    EXPORTING
      mode_vbak      = `E`
      mandt          = sy-mandt
      vbeln          = vbeln
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    client->message_box_display( `Locked by another user.` ).
    RETURN.
  ENDIF.

  UPDATE vbak SET auart = @auart WHERE vbeln = @vbeln.
  COMMIT WORK.

  CALL FUNCTION `DEQUEUE_EVVBAK`
    EXPORTING
      mode_vbak = `E`
      mandt     = sy-mandt
      vbeln     = vbeln.

ENDMETHOD.
```

The lock exists for milliseconds, so this scales. But it protects only the write, not the edit. The enqueue is taken after the user read the record. A second user may have read the same record, saved first and released the lock again, and nobody notices. The first user's save then overwrites that change with values based on the old read. That lost update is what the timestamp of pattern 3 catches.

→ Full source: [`ZCL_SAMPLE_LOCK_2`](/cookbook/expert_more/lock_sources#lock-2)

## 3. Optimistic Check
On read, remember the record's change timestamp. On save, re-read and reject if it shifted in the meantime — the same idea as an HTTP ETag:
```abap
METHOD data_read.
  SELECT SINGLE aedat, UPD_TMSTMP FROM vbak
    WHERE vbeln = @vbeln
    INTO ( @token_aedat, @token_tmstmp ).
ENDMETHOD.

METHOD on_event_save.

  SELECT SINGLE aedat, UPD_TMSTMP FROM vbak
    WHERE vbeln = @vbeln
    INTO ( @DATA(now_aedat), @DATA(now_tmstmp) ).

  IF now_aedat <> token_aedat OR now_tmstmp <> token_tmstmp.
    client->message_box_display( `Record changed — please reload.` ).
    RETURN.
  ENDIF.

  " ...write through...

ENDMETHOD.
```

No lock is held during the edit phase, so this scales to many concurrent users and catches conflicts even from outside your app (SE16, batch jobs, RFC).

Pick a column that *always* updates on writes. If anyone writes the table bypassing `AEDAT` / `UPD_TMSTMP`, the check silently misses real conflicts.

→ Full source: [`ZCL_SAMPLE_LOCK_3`](/cookbook/expert_more/lock_sources#lock-3)

## 4. Combined (recommended default)
**Lock at Save** plus the **Optimistic Check** is the safest stateless pattern and the sensible production default. The enqueue makes parallel saves of *this* app wait for each other, and the timestamp check catches everyone else. The save handler does both, in this order:
```abap
METHOD on_event_save.

  " 1. lock, so that no second save runs at the same time
  CALL FUNCTION 'ENQUEUE_EVVBAK'
    EXPORTING
      vbeln        = vbeln
    EXCEPTIONS
      foreign_lock = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    client->message_box_display( `Another user is saving this order. Try again.` ).
    RETURN.
  ENDIF.

  " 2. compare the timestamp with the one remembered on read
  SELECT SINGLE aedat, UPD_TMSTMP FROM vbak
    WHERE vbeln = @vbeln
    INTO ( @DATA(now_aedat), @DATA(now_tmstmp) ).

  IF now_aedat <> token_aedat OR now_tmstmp <> token_tmstmp.
    CALL FUNCTION 'DEQUEUE_EVVBAK' EXPORTING vbeln = vbeln.
    client->message_box_display( `Record changed — please reload.` ).
    RETURN.
  ENDIF.

  " 3. write, then release the lock
  " ...write through...
  CALL FUNCTION 'DEQUEUE_EVVBAK' EXPORTING vbeln = vbeln.

ENDMETHOD.
```

→ Full source: [`ZCL_SAMPLE_LOCK_4`](/cookbook/expert_more/lock_sources#lock-4)

## 5. Stateful Session
For classic SAP GUI-like behavior, switch the session to stateful and call the lock function module on init. The lock survives subsequent roundtrips as long as the session stays alive:
```abap
IF client->check_on_navigated( ).

  CALL FUNCTION `ENQUEUE_EVVBAK`
    EXPORTING
      mode_vbak      = `E`
      mandt          = sy-mandt
      vbeln          = vbeln
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc = 0.
    client->set_session_stateful( ).
  ELSE.
    client->message_box_display( `Locked by another user.` ).
    client->nav_app_leave( ).
    RETURN.
  ENDIF.

ENDIF.
```

While the app is open, an entry for `EVVBAK` / `0000004711` is visible in SM12. The user can navigate, type, wait — the lock stays. On exit, release it explicitly:
```abap
CALL FUNCTION `DEQUEUE_EVVBAK`
  EXPORTING
    mode_vbak = `E`
    mandt     = sy-mandt
    vbeln     = vbeln.
client->set_session_stateful( abap_false ).
```

Each active user pins a work process, and a leaked enqueue blocks the record until the session times out — the cost and the rules for paying it are on [Statefulness → Trade-offs](/cookbook/expert_more/statefulness#trade-offs).

→ Full source: [`ZCL_SAMPLE_LOCK_5`](/cookbook/expert_more/lock_sources#lock-5)

## 6. Soft Lock
A soft lock is a row in a custom Z table marking *"user X is editing object Y"*. It is **not** enforced by the SAP kernel — only your app code respects it — so use it for UX feedback ("locked by Alice since 09:32") and always layer it on top of a real save-time guard. A minimal schema (table `Z2UI5_SAMPLE_01` in the example below):

| Field      | Type        | Description           |
|------------|-------------|-----------------------|
| MANDT      | MANDT       | Client (key)          |
| VBELN      | VBELN_VA    | Object (key)          |
| USERNAME   | SYUNAME     | Editing user          |
| LOCKED_AT  | TIMESTAMPL  | When the lock started |

A user closing the browser without pressing *Release* leaves the row behind, so add a cleanup job that deletes entries older than, say, 30 minutes. The example below uses a matching `Z2UI5_SAMPLE_01` table.

→ Full source: [`ZCL_SAMPLE_LOCK_6`](/cookbook/expert_more/lock_sources#lock-6)

## 7. RAP Drafts
On modern releases, RAP draft-enabled business objects manage locking for you: the draft holds an exclusive lock for its owner while the user keeps editing — no stateful session, no `ENQUEUE_*` call. If a released SAP BO already covers your object, this is usually the simplest path. See [Draft Handling](/cookbook/eml_cds_sql/draft_handling).

## 8. Lock-Manager Add-on
The community add-on [**lock-manager**](https://github.com/abap2UI5-addons/lock-manager) wraps the lock logic in a reusable class — including stale-lock cleanup and a "locked by X since…" message for the user. Install it like any other [add-on](/resources/addons) and call it instead of writing the boilerplate yourself.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Stateful Sessions — Locks — ENQUEUE_E_TABLE and ENQUEUE_READ, end and restart the session | [`Z2UI5_CL_SMPS_APP_485`](https://github.com/abap2UI5/samples-stack/blob/main/src/06/z2ui5_cl_smps_app_485.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `06-stateful-locks` |
| Stateful Sessions — Navigation and Locks — every Next Lock View takes the next VARKEY, going back releases it | [`Z2UI5_CL_SMPS_APP_490`](https://github.com/abap2UI5/samples-stack/blob/main/src/06/z2ui5_cl_smps_app_490.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `06-stateful-locks` |

<!-- samples:end -->
