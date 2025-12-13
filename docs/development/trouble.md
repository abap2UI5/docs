# Troubleshooting

#### Hide Soft Keyboard

For UI5 input fields, the soft keyboard is automatically shown when focusing an input field. In some use cases, e.g. in the context of warehouses with small devices, this is not always wanted. To change this behavior, we have to adjust the HTML input element and switch the input type to `none`. The following snippet demonstrates how to activate/deactivate the soft keyboard:

```abap

  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
    
    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->_generic( name = `script`
                    ns   = `html` )->_cc_plain_xml( `z2ui5.afterBE = (id , mode) => { ` &&
                       `debugger;` &&
                        `var input = z2ui5.oView.byId(id).getDomRef();` &&
                        `input = input.childNodes[0].childNodes[0];` &&
                        `input.setAttribute("inputmode" , mode);` &&
                        ` alert("inputmode changed to" + mode); }` ).

    DATA(page) =   view->shell(
             )->page( title          = 'abap2UI5 - Softkeyboard on/off'
                      navbuttonpress = client->_event( 'BACK' )
                      shownavbutton  = client->check_app_prev_stack( )
                      )->_z2ui5( )->focus( focusid = `ZINPUT`
      )->simple_form( editable = abap_true
                 )->content( 'form'
                     )->title( 'Keyboard on/off'
                     )->label( 'Input'
                     )->input( id               = `ZINPUT`
                               value            = client->_bind_edit( input )
                               showvaluehelp    = abap_true
                               valuehelprequest = client->_event( 'CALL_KEYBOARD' )
                               valuehelpiconsrc = 'sap-icon://keyboard-and-mouse' ).

    client->view_display( page->stringify( ) ).

    ENDIF.


    CASE client->get( )-event.
      WHEN 'CALL_KEYBOARD'.
        client->follow_up_action( `z2ui5.afterBE("ZINPUT", "none");` ).
      WHEN 'BACK'.
        client->nav_app_leave( ).
    ENDCASE.

  ENDMETHOD.
```