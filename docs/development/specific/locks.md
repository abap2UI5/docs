---
outline: [2, 4]
---
# Locks

Business logic often needs to prevent two users from editing the same object at the same time. Because abap2UI5 runs stateless by default, locks held with `ENQUEUE` / `DEQUEUE` are released at the end of each roundtrip — so locking needs a deliberate approach.

### Lock with a Stateful Session
Switch the session to stateful and call the lock function module. The lock survives subsequent roundtrips as long as the session stays stateful:
```abap
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
ENDIF.
```

Release the lock by ending the stateful session — for example on navigation away from the edit view:
```abap
client->set_session_stateful( abap_false ).
client->nav_app_leave( ).
```

A complete example is in sample `Z2UI5_CL_DEMO_APP_350`.

### Other Options
Stateful sessions are only available on private and on-premise systems. For other deployments, infinite backend sessions can hold locks while the UI5 app keeps talking statelessly. See [Statefulness, Locks](../../advanced/stateful.md) for the alternatives and trade-offs.
