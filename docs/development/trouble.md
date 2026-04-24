---
outline: [2, 4]
---
# Troubleshooting

#### Hide Soft Keyboard

For UI5 input fields, the soft keyboard appears automatically when you focus an input. In some cases — for example, warehouses with small devices — you don't want this behavior. To change it, adjust the HTML input element and switch the input type to `none`. The snippet below toggles the soft keyboard.

This example uses the **`z2ui5.afterBE` hook** — a JavaScript callback that the abap2UI5 frontend framework runs after every backend roundtrip. Assign a custom function to `z2ui5.afterBE` to run JavaScript in the browser after the backend responds. This is useful for DOM manipulations that UI5 doesn't support natively.

The pattern has two steps:
1. **Define** the JavaScript function in a `<html:script>` tag (rendered once on init)
2. **Run** it after a backend event through `client->follow_up_action( )` with a raw JavaScript string

```abap
METHOD z2ui5_if_app~main.

    DATA input TYPE string.

    IF client->check_on_init( ).

    DATA(view) = z2ui5_cl_xml_view=>factory( ).

    "Step 1: Define a JavaScript function inside an inline <script> tag.
    "z2ui5.afterBE is a hook that runs after the backend responds.
    "This function takes a UI5 control ID and an inputmode value,
    "then modifies the HTML input element's inputmode attribute.
    view->_generic( name = `script`
                    ns   = `html` )->_cc_plain_xml( `z2ui5.afterBE = (id , mode) => { ` &&
                        `var input = z2ui5.oView.byId(id).getDomRef();` &&
                        `input = input.childNodes[0].childNodes[0];` &&
                        `input.setAttribute("inputmode" , mode);` &&
                        ` alert("inputmode changed to" + mode); }` ).

    DATA(page) =   view->shell(
             )->page( title          = `abap2UI5 - Softkeyboard on/off`
                      navbuttonpress = client->_event( `BACK` )
                      shownavbutton  = client->check_app_prev_stack( )
                      )->_z2ui5( )->focus( focusid = `ZINPUT`
      )->simple_form( editable = abap_true
                 )->content( `form`
                     )->title( `Keyboard on/off`
                     )->label( `Input`
                     )->input( id               = `ZINPUT`
                               value            = client->_bind_edit( input )
                               showvaluehelp    = abap_true
                               valuehelprequest = client->_event( `CALL_KEYBOARD` )
                               valuehelpiconsrc = `sap-icon://keyboard-and-mouse` ).

    client->view_display( page->stringify( ) ).

    ENDIF.

    CASE client->get( )-event.
      WHEN `CALL_KEYBOARD`.
        "Step 2: After the backend processes the event, run the JavaScript
        "function as a follow-up action. This sets inputmode="none" on the
        "HTML element, which hides the soft keyboard on mobile devices.
        client->follow_up_action( `z2ui5.afterBE("ZINPUT", "none");` ).
      WHEN `BACK`.
        client->nav_app_leave( ).
    ENDCASE.

ENDMETHOD.
```
