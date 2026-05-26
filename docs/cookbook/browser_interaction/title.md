---
outline: [2, 4]
---
# Title

Set the text the browser shows in the tab and window title bar.

#### Standalone

To change the title after the app is running — for example, to reflect the current record — call the `set_title` frontend event from the backend:

```abap
METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).
        client->view_display( z2ui5_cl_xml_view=>factory(
            )->page(
                )->button(
                    text  = `change title`
                    press = client->_event( `RENAME` )
            )->stringify( ) ).

      WHEN client->check_on_event( `RENAME` ).
        client->action(
            val   = client->cs_event-set_title
            t_arg = VALUE #( ( `Invoice 4711` ) ) ).

    ENDCASE.

  ENDMETHOD.
```

#### Launchpad

Inside an SAP Fiori Launchpad shell the title is forwarded to the `ShellUIService`; standalone the framework falls back to `document.title`.
