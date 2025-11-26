---
outline: [2, 6]
---
# Custom JS

If the standard UI5 framework functionalities do not fulfill all your requirements, you can define and call your own custom JavaScript functions. For example, this approach is used in the scanner section to play a sound after scanning.

The idea is to send the custom JavaScript function along with the view to the frontend and invoke it later when an event is triggered.

Below is a working example that you can use as a starting point:

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
                   press = client->_event( |CUSTOM_JS| ) ).
      client->view_display( view->stringify( ) ).
  ENDIF.

  IF client->get( )-event = |CUSTOM_JS|.
      client->follow_up_action( `myFunction()` ).
  ENDIF.

ENDMETHOD.
```