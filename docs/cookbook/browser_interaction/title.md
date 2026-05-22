---
outline: [2, 4]
---
# Title

Set the text the browser shows in the tab and window title bar. abap2UI5 offers a static option via the user-exit configuration, and a dynamic option via a follow-up JavaScript action.

#### Static — via User Exit

The initial tab title comes from `cs_config-title` in the HTTP GET user exit. Implement [`Z2UI5_IF_EXIT`](../../advanced/extensibility/user_exits.md) and set the field once on page load:

```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-title = `my title`.

ENDMETHOD.
```

#### Dynamic — at Runtime

To change the title after the app is running — for example, to reflect the current record — push a follow-up action that updates `document.title` directly:

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
        client->follow_up_action( `document.title = "Invoice 4711";` ).

    ENDCASE.

  ENDMETHOD.
```
