---
outline: [2, 5]
---
# Messages, Errors

Showing messages and errors is an everyday task for ABAP developers. The functions below cover the most common cases.

#### Message Toast

For brief notifications like success confirmations, use the message toast:

```abap
METHOD z2ui5_if_app~main.
  client->message_toast_display( `this is a message` ).
ENDMETHOD.
```

#### Message Box

When the user needs to acknowledge the message, show a message box they must close:

```abap
METHOD z2ui5_if_app~main.
  client->message_box_display( `this is a message` ).
ENDMETHOD.
```

For error messages, change the type:

```abap
METHOD z2ui5_if_app~main.
  client->message_box_display(
    text = `This is an error message`
    type = `error` ).
ENDMETHOD.
```

#### SY, BAPIRET, CX_ROOT
You can pass common message structures, objects, and variables directly to these functions:

##### SY
```abap
METHOD z2ui5_if_app~main.

  MESSAGE ID `NET` TYPE `I` NUMBER `001` INTO DATA(lv_dummy).
  client->message_box_display( sy ).

ENDMETHOD.
```
##### BAPIRET
```abap
METHOD z2ui5_if_app~main.

  DATA lt_bapiret TYPE STANDARD TABLE OF bapiret2.
  CALL FUNCTION `BAPI_USER_GET_DETAIL`
    EXPORTING
      username = sy-uname
    TABLES
      return   = lt_bapiret.
  client->message_box_display( lt_bapiret ).

ENDMETHOD.
```
##### CX_ROOT
```abap
METHOD z2ui5_if_app~main.

  TRY.
    DATA(lv_val) = 1 / 0.
  CATCH cx_root INTO DATA(lx).
    client->message_box_display( lx ).
  ENDTRY.

ENDMETHOD.
```
The framework accepts other inputs too — pass your message structure and the message box shows it.

#### Multi-Message Popup
The message box gives you basic output. For richer detail, use the popup `Z2UI5_CL_POP_MESSAGES`:
```abap
METHOD z2ui5_if_app~main.

  DATA(lt_msg) = VALUE bapirettab(
    ( type = `E` id = `MSG1` number = `001` message = `An empty Report field causes an empty XML Message to be sent` )
    ( type = `I` id = `MSG2` number = `002` message = `Product already in use` ) ).

  client->nav_app_call( z2ui5_cl_pop_messages=>factory( lt_msg ) ).

ENDMETHOD.
```
#### Error Popup
To display full details of your exception:
```abap
METHOD z2ui5_if_app~main.

  TRY.
    DATA(lv_val) = 1 / 0.
  CATCH cx_root INTO DATA(lx).
    client->nav_app_call( z2ui5_cl_pop_error=>factory( lx ) ).
  ENDTRY.

ENDMETHOD.
```

#### Uncaught Errors
When your code doesn't catch exceptions, the framework catches them and displays the standard error popup. Try this:

```abap
METHOD z2ui5_if_app~main.

    RAISE EXCEPTION NEW cx_sy_itab_line_not_found( ).

ENDMETHOD.
```

#### Uncatchable Exceptions / Short Dumps
What happens if your code raises uncatchable exceptions? The default HTTP handler exception output appears. Processing stops, and the user has to reload the browser. Reserve this for unexpected cases:

```abap
METHOD z2ui5_if_app~main.

    ASSERT 1 = `This is an error message!`.

ENDMETHOD.
```

::: tip **Improvements**
These message functions evolve all the time. Open an issue if you hit errors or incompatibilities, or submit a PR to extend them.
:::
