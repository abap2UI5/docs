# App State

abap2UI5 supports saving the current app state so you can return to it later -- similar to how standard UI5 applications handle state management.

### Usage
Each state is saved as a draft with a unique ID, you can display this ID in the URL using the `client->set_app_state_active` method. This command generates a URL that you can copy and paste into another browser window. An example URL looks like this: <br>
`.../sap/bc/z2ui5?sap-client=001&app_start=z2ui5_cl_demo_app_000#/z2ui5-xapp-state=024251849E5A1EDFB1DAE2C97C8CE8C2`

### Sample Code
Below is an implementation of the app state functionality:
```abap
CLASS z2ui5_cl_sample_app_state DEFINITION PUBLIC.
 
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_quantity TYPE string.
 
ENDCLASS.
 
CLASS z2ui5_cl_sample_app_state IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
 
    IF client->check_on_navigated( ).
      DATA(view) = z2ui5_cl_xml_view=>factory( ).
      client->view_display(
        view->label( `quantity`
            )->input( client->_bind_edit( mv_quantity )
            )->button(
                text  = `post with state`
                press = client->_event( `BUTTON_POST` )
              )->stringify( ) ).
    ENDIF.
 
    CASE client->get( )-event.
      WHEN `BUTTON_POST`.
        client->message_toast_display( `data updated and url adjusted` ).
        client->set_app_state_active( ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

A working implementation of this code can be found in sample `z2ui5_cl_demo_app_321`.
