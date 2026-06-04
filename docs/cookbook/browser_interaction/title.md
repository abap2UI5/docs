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

When the app runs inside an SAP Fiori Launchpad shell, use the dedicated `set_title_launchpad` event instead. It forwards the title to the shell's `ShellUIService` rather than setting `document.title`:

```abap
client->action(
    val   = client->cs_event-set_title_launchpad
    t_arg = VALUE #( ( `Invoice 4711` ) ) ).
```

Use `set_title` for the browser tab/window title (standalone) and `set_title_launchpad` for the launchpad shell title.
