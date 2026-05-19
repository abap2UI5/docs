---
outline: [2, 4]
---
# Locks

In classic SAP GUI, a transaction like `VA02` calls `ENQUEUE_EVVBAK` and the lock lives as long as the dialog session. Web apps are different: each roundtrip is a fresh ABAP session by default, so a lock set in one roundtrip is gone by the next. Locking in abap2UI5 means picking a deliberate strategy for two questions:

| Phase | Question |
|---|---|
| **Edit** | What happens while the user is thinking and typing? |
| **Save** | What happens the moment they hit save? |

The patterns below combine in different ways. The examples use the sales order header table `VBAK` and its standard enqueue object `EVVBAK`, but the same shapes apply to any table. All snippets have a complete, copy-paste-ready demo class — sample numbers are linked next to each section.

#### 1. No Locking
The minimal starting point — the user edits and saves, no lock and no conflict check. Last save wins, silently. Fine for personal sandboxes and throwaway demos, but rarely what you want in production. See sample `Z2UI5_CL_DEMO_APP_S_07`.

#### 2. Lock at Save
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

The lock exists for milliseconds, so this scales — but two parallel saves can still race if the timestamps line up, and the second one silently overwrites the first. See sample `Z2UI5_CL_DEMO_APP_S_08`.

#### 3. Optimistic Check
On read, remember the record's change timestamp. On save, re-read and reject if it shifted in the meantime — the same idea as an HTTP ETag:
```abap
METHOD data_read.
  SELECT SINGLE aedat, UPD_TMSTMP FROM vbak
    INTO ( @token_aedat, @token_aezet )
    WHERE vbeln = @vbeln.
ENDMETHOD.

METHOD on_event_save.

  SELECT SINGLE aedat, UPD_TMSTMP FROM vbak
    INTO ( @DATA(now_aedat), @DATA(now_aezet) )
    WHERE vbeln = @vbeln.

  IF now_aedat <> token_aedat OR now_aezet <> token_aezet.
    client->message_box_display( `Record changed — please reload.` ).
    RETURN.
  ENDIF.

  " ...write through...

ENDMETHOD.
```

No lock is held during the edit phase, so this scales to many concurrent users and catches conflicts even from outside your app (SE16, batch jobs, RFC). See sample `Z2UI5_CL_DEMO_APP_S_09`.

Pick a column that *always* updates on writes. If anyone writes the table bypassing `AEDAT` / `UPD_TMSTMP`, the check silently misses real conflicts.

#### 4. Combined (recommended default)
**Lock at Save** plus the **Optimistic Check** is the safest stateless pattern and the sensible production default — the enqueue serializes parallel saves of *this* app, the timestamp check catches everyone else. See sample `Z2UI5_CL_DEMO_APP_S_10`.

#### 5. Stateful Session
For classic SAP GUI-like behaviour, switch the session to stateful and call the lock function module on init. The lock survives subsequent roundtrips as long as the session stays alive:
```abap
IF client->check_on_init( ).

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

See samples `Z2UI5_CL_DEMO_APP_S_11` and `Z2UI5_CL_DEMO_APP_350` for complete examples.

::: warning
Each active user pins a work process. Use stateful sessions only for low-traffic, internal apps. Always pair `set_session_stateful( )` with an explicit `set_session_stateful( abap_false )` on every exit path — otherwise a leaked enqueue blocks future users until the session times out.
:::

#### 6. Soft Lock
A soft lock is a row in a custom Z table marking *"user X is editing object Y"*. It is **not** enforced by the SAP kernel — only your app code respects it — so use it for UX feedback ("locked by Alice since 09:32") and always layer it on top of a real save-time guard. A minimal schema:

| Field      | Type        | Description           |
|------------|-------------|-----------------------|
| MANDT      | MANDT       | Client (key)          |
| VBELN      | VBELN_VA    | Object (key)          |
| USERNAME   | SYUNAME     | Editing user          |
| LOCKED_AT  | TIMESTAMPL  | When the lock started |

A user closing the browser without pressing *Release* leaves the row behind, so add a cleanup job that deletes entries older than, say, 30 minutes. See sample `Z2UI5_CL_DEMO_APP_S_12` (with the matching `Z2UI5_SAMPLE_01` table).

#### 7. RAP Drafts
On modern releases, RAP draft-enabled business objects manage locking for you: the draft holds an exclusive lock for its owner while the user keeps editing — no stateful session, no `ENQUEUE_*` call. If a released SAP BO already covers your object, this is usually the simplest path. See [Draft Handling](./draft.md).

#### 8. Lock-Manager Add-on
The community add-on [**lock-manager**](https://github.com/abap2UI5-addons/lock-manager) wraps the lock logic in a reusable class — including stale-lock cleanup and a "locked by X since…" message for the user. Install it like any other [add-on](../../resources/addons.md) and call it instead of writing the boilerplate yourself.

#### Overview

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

For the underlying concepts and trade-offs of statefulness, see [Statefulness](./statefulness.md).
