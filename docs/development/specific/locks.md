---
outline: [2, 4]
---
# Locks

Business logic often needs to prevent two users from editing the same object at the same time. Because abap2UI5 runs stateless by default, locks held with `ENQUEUE` / `DEQUEUE` are released at the end of each roundtrip — so locking needs a deliberate approach.

#### Lock at Save
The simplest pattern: do not hold a lock while the user thinks. Acquire it the moment the user saves, write, commit, and release — all in one short roundtrip. Combine with an optimistic check (e.g. a last-changed timestamp) to catch conflicts from outside your app.

#### Lock with a Stateful Session
For classic SAP GUI-like behaviour, switch the session to stateful and call the lock function module. The lock survives subsequent roundtrips as long as the session stays alive:
```abap
CLASS z2ui5_cl_sample_lock DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA varkey TYPE char120.

ENDCLASS.

CLASS z2ui5_cl_sample_lock IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      CALL FUNCTION `ENQUEUE_E_TABLE`
        EXPORTING
          tabname        = `ZTEST`
          varkey         = varkey
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc = 0.
        client->set_session_stateful( ).
      ELSE.
        client->set_session_stateful( abap_false ).
        client->nav_app_leave( ).
      ENDIF.

    ENDIF.

    "release the lock on exit
    IF client->check_on_event( `BACK` ).
      client->set_session_stateful( abap_false ).
      client->nav_app_leave( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

See sample `Z2UI5_CL_DEMO_APP_350` for a complete example.

::: warning
A stateful session pins a work process per active user. Use it only for low-traffic, internal apps. For background and the wider lock discussion, see [Statefulness, Locks](../../advanced/stateful.md).
:::

#### RAP Drafts
On modern releases, RAP draft-enabled business objects manage locking for you: a draft holds an exclusive lock for its owner while the user keeps editing — no stateful session, no `ENQUEUE_*` call. If a released SAP BO already covers your object, this is usually the simplest path. See [Draft Handling](./draft.md).

#### Lock-Manager Add-on
The community add-on [**lock-manager**](https://github.com/abap2UI5-addons/lock-manager) wraps the lock logic in a reusable class — including stale-lock cleanup and a "locked by X since…" message for the user. Install it like any other [add-on](../../resources/addons.md) and call it instead of writing the boilerplate yourself.
