---
outline: [2, 4]
---
# Nested Views

A nested view is a view that is built from several independent parts. In abap2UI5 there are two main ways to achieve this: composing a single view tree from helper methods, and splitting the app into sub-apps that each own a slice of the view.

#### Composing with Helper Methods

The fluent builder returns each node as an object reference, so you can pass a node into a helper method and let it add children there:

```abap
METHOD z2ui5_if_app~main.
  IF client->check_on_init( ).
    DATA(xml)  = z2ui5_cl_xml_view=>factory( ).
    DATA(page) = xml->shell( )->page( 'My App' ).

    build_header( page ).
    build_content( page ).
    build_footer( page ).

    client->view_display( xml->stringify( ) ).
  ENDIF.
ENDMETHOD.

METHOD build_header.
  io_page->toolbar( )->title( 'Header' ).
ENDMETHOD.

METHOD build_content.
  io_page->vbox(
      )->text( 'Line 1'
      )->text( 'Line 2' ).
ENDMETHOD.

METHOD build_footer.
  io_page->footer( )->button(
    text  = 'Save'
    press = client->_event( 'SAVE' ) ).
ENDMETHOD.
```

Each helper receives the parent node by reference and attaches its controls to it. The final `stringify( )` call serialises the entire tree — including all nested children — into one XML string.

#### Sub-Apps

For larger apps it is useful to split responsibility across separate ABAP classes. Each sub-app implements `z2ui5_if_app` and manages its own piece of the view. The parent app instantiates the sub-app, passes the client, and merges the returned view fragment into its own view tree.

**Parent app:**
```abap
METHOD z2ui5_if_app~main.
  IF client->check_on_init( ).
    sub_app = NEW z2ui5_cl_my_sub_app( ).
  ENDIF.

  DATA(xml)  = z2ui5_cl_xml_view=>factory( ).
  DATA(page) = xml->shell( )->page( 'Parent' ).

  " Let the sub-app render into a container on the page
  DATA(container) = page->vbox( ).
  sub_app->render( client = client io_node = container ).

  client->view_display( xml->stringify( ) ).
ENDMETHOD.
```

**Sub-app:**
```abap
METHOD render.
  io_node->text( 'I am the sub-app' ).
  io_node->button(
    text  = 'Sub Action'
    press = io_client->_event( 'SUB_ACTION' ) ).
ENDMETHOD.
```

The sub-app appends its controls to whatever node the parent provides. Events fired inside the sub-app are handled by the sub-app's own `main` method on the next roundtrip.

#### Navigation Between Views

When the nested content changes completely — for example, a detail page replacing a list — use `view_display` to swap the entire view rather than nesting:

```abap
CASE client->get_event( ).
  WHEN 'OPEN_DETAIL'.
    client->view_display( detail_view->stringify( ) ).
  WHEN 'BACK'.
    client->view_display( list_view->stringify( ) ).
ENDCASE.
```

Reserve true nesting for content that appears side-by-side or in a stable layout shell. Use `view_display` for sequential navigation where one screen fully replaces another.

#### Tips

- Keep each helper or sub-app focused on one logical section. A helper that grows beyond ~30 lines is a candidate for its own sub-app class.
- Sub-apps can hold their own instance attributes for local state. Because the whole app tree is reconstructed on every roundtrip, each sub-app re-renders from its current state automatically.
- Pass only the node the sub-app needs, not the whole view root. This limits the sub-app's scope and makes the boundary explicit.

See `Z2UI5_CL_DEMO_APP_160` for an end-to-end example of parent and sub-app composition.
