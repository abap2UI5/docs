---
outline: [2, 4]
---

# Messages & Errors

Outputting messages and errors is an everyday requirement for ABAP developers. abap2UI5 provides functions for the most common situations.

#### Message Toast

Messages that are displayed for a short period, such as success notifications, can be shown with:

```abap
METHOD z2ui5_if_app~main.

  client->message_toast_display( `this is a message` ).

ENDMETHOD.
```

#### Message Box

If you want to ensure that the user acknowledges the message, you can display a message box that requires a manual close:

```abap
METHOD z2ui5_if_app~main.

  client->message_box_display( `this is a message` ).

ENDMETHOD.
```

For error messages, simply change the type:

```abap
METHOD z2ui5_if_app~main.

  client->message_box_display( 
    text = 'This is an error message' 
    type = 'error' ).

ENDMETHOD.
```

#### SY, BAPIRET, CX_ROOT
abap2UI5 contains a few functions to automatically generate a message popup in certain situations:
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
A lot more imports are possible, just import your message structure, and the message box will display the content.

#### Popup Multi Message 
The message box provides basic output, if you want to generate a detailed output, use the popup `z2ui5_cl_pop_messages`:
```abap
METHOD z2ui5_if_app~main.

  data(lt_msg) = value bapirettab(
    ( type = 'E' id = 'MSG1' number = '001' message = 'An empty Report field causes an empty XML Message to be sent' )
    ( type = 'I' id = 'MSG2' number = '002' message = 'Product already in use' ) ).

  client->nav_app_call( z2ui5_cl_pop_messages=>factory( lt_msg ) ).

ENDMETHOD.
```
#### Popup Error
To output a detailed view of your exception:
```abap
METHOD z2ui5_if_app~main.

  TRY.
    DATA(lv_val) = 1 / 0.
  CATCH cx_root INTO DATA(lx).
    client->nav_app_call( z2ui5_cl_pop_error=>factory( lx ) ).
  ENDTRY.

ENDMETHOD.
```
#### Popup Confirm
If interaction is required, the `z2ui5_cl_pop_to_confirm` can be used:
```abap
METHOD z2ui5_if_app~main.

  IF client->check_on_init( ).
   client->nav_app_call( z2ui5_cl_pop_to_confirm=>factory( `Can you confirm this?` ) ).
 ENDIF.

  CASE client->get( )-event.
    WHEN z2ui5_cl_pop_to_confirm=>cs_event-confirmed.
      client->message_box_display( `the result is confirmed` ).
    WHEN z2ui5_cl_pop_to_confirm=>cs_event-canceled.
      client->message_box_display( `the result is rejected` ).
  ENDCASE.

ENDMETHOD.
```
#### Uncatched Errors
What happens if errors are uncaught? In this case, the SAP handler exception output is used. The processing is interrupted and the user need to restart the browser. So only use this for unexpected behaviour:
```abap
METHOD z2ui5_if_app~main.

    ASSERT 1 = `This is an error message!`.

ENDMETHOD.
```
Or achieve the same behavior with an uncaught exception:
```abap
METHOD z2ui5_if_app~main.

    RAISE EXCEPTION NEW lcx_error( ).

ENDMETHOD.
```
::: tip **Improvements**
All these message functions are continually improved. Feel free to open an issue if you encounter errors or incompatibilities, or submit a PR to extend the functionality.
:::