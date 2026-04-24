---
outline: [2, 4]
---
# Custom JS

If the standard UI5 framework doesn't cover all your needs, define and call your own JavaScript functions. [Barcode Scanning](/development/specific/barcodes), for example, uses this approach to play a sound after scanning.

The idea: send the JavaScript function along with the view to the frontend and call it later when an event fires.

Below is a working example to use as a starting point. The `_generic` method creates an arbitrary XML/HTML element — here an HTML `<script>` tag (namespace `html`). The `_cc_plain_xml` method injects raw content into that element — in this case, the JavaScript function definition. On the backend, `client->follow_up_action` then runs the function by name on the frontend:

```abap
  METHOD z2ui5_if_app~main.

  IF client->check_on_init( ).
      DATA(view) = z2ui5_cl_xml_view=>factory( ).
      view->_generic( name = `script` ns = `html`
        )->_cc_plain_xml(
          |function myFunction() \{ console.log( `Hello World` ); \}|
        ).
      view->page(
        )->button( text  = `call custom JS`
                   press = client->_event( `CUSTOM_JS` ) ).
      client->view_display( view->stringify( ) ).
  ENDIF.

  IF client->get( )-event = `CUSTOM_JS`.
      client->follow_up_action( `myFunction()` ).
  ENDIF.

ENDMETHOD.
```
