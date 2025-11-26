---
outline: [2, 4]
---

# Messages, Errors

Displaying messages and errors is an everyday requirement for ABAP developers. The following functions handle the most common scenarios.

#### Message Toast

For short-duration messages, such as success notifications, you can use the message toast:

```abap
METHOD z2ui5_if_app~main.

  client->message_toast_display( `this is a message` ).

ENDMETHOD.
```

#### Message Box

Want the user to acknowledge the message? You can display a message box that requires manual closure:

```abap
METHOD z2ui5_if_app~main.

  client->message_box_display( `this is a message` ).

ENDMETHOD.
```

For error messages, simply change the type:

```abap
METHOD z2ui5_if_app~main.

  client->message_box_display(
    text = `This is an error message`
    type = `error` ).

ENDMETHOD.
```

#### SY, BAPIRET, CX_ROOT
You can directly pass common message structures, objects, and variables to the functions:
###### SY
```abap
METHOD z2ui5_if_app~main.
  
  MESSAGE ID 'NET' TYPE 'I' NUMBER '001' into data(lv_dummy).
  client->message_box_display( sy ).

ENDMETHOD.
```
###### BAPIRET
```abap
METHOD z2ui5_if_app~main.

  DATA lt_bapiret TYPE STANDARD TABLE OF bapiret2.
  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username = sy-uname
    TABLES
      return   = lt_bapiret.
  IF sy-subrc <> 0.
    client->message_box_display( lt_bapiret ).
  ENDIF.

ENDMETHOD.
```
###### CX_ROOT
```abap
METHOD z2ui5_if_app~main.

  TRY.
    DATA(lv_val) = 1 / 0.
  CATCH cx_root INTO DATA(lx).
    client->message_box_display( lx ).
  ENDTRY.

ENDMETHOD. 
```
Other imports are supported as well. Just import your message structure, and the message box will display it.

#### Popup Multi Message 
The message box provides basic output. For a more detailed output, use the popup `z2ui5_cl_pop_messages`:
```abap
METHOD z2ui5_if_app~main.

  data(lt_msg) = value bapirettab(
    ( type = `E` id = `MSG1` number = `001` message = `An empty Report field causes an empty XML Message to be sent` )
    ( type = `I` id = `MSG2` number = `002` message = `Product already in use` ) ).

  client->nav_app_call( z2ui5_cl_pop_messages=>factory( lt_msg ) ).

ENDMETHOD.
```
#### Popup Error
To show a detailed view of your exception, use the following code:
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
What happens if errors are uncaught? In this case, the default HTTP handler exception output is used. The processing is interrupted, and the user will need to refresh the browser. Use this only for unexpected behavior:
```abap
METHOD z2ui5_if_app~main.

    ASSERT 1 = `This is an error message!`.

ENDMETHOD.
```
Alternatively, achieve the same behavior with an uncaught exception:
```abap
METHOD z2ui5_if_app~main.

    RAISE EXCEPTION NEW cx_sy_itab_line_not_found( ).

ENDMETHOD.
```
::: tip **Improvements**
These message functions are continually being improved. Feel free to open an issue if you encounter errors or incompatibilities, or submit a PR to extend the functionality.
:::
