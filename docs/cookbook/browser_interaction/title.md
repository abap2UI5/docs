---
outline: [2, 4]
---
# Title

::: warning This page still shows the previous view builder
The examples below build views with `z2ui5_cl_xml_view`. That class is frozen:
it still runs, and your existing apps keep working — but it is no longer the
one to write new code against. The current builder is
`z2ui5_cl_ui5_view_builder`, and it has four verbs instead of a control per
method, which makes every UI5 control available rather than the curated set.

See [View → Definition](/cookbook/view/definition) for what the chain looks
like, and [Deprecations](/resources/deprecations) for the translation.
:::

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
        client->follow_up_action(
            val   = client->cs_event-set_title
            t_arg = VALUE #( ( `Invoice 4711` ) ) ).

    ENDCASE.

  ENDMETHOD.
```

#### Launchpad

When the app runs inside an SAP Fiori Launchpad shell, use the dedicated `set_title_launchpad` event instead. It forwards the title to the shell's `ShellUIService` rather than setting `document.title`:

```abap
client->follow_up_action(
    val   = client->cs_event-set_title_launchpad
    t_arg = VALUE #( ( `Invoice 4711` ) ) ).
```

Use `set_title` for the browser tab/window title (standalone) and `set_title_launchpad` for the launchpad shell title.
