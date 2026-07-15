---
outline: [2, 4]
---
# Clipboard

Copy arbitrary text to the user's clipboard with the frontend event `client->cs_event-clipboard_copy`. The text travels as the first `t_arg` entry and is passed to the browser's `navigator.clipboard.writeText` API, which performs the copy entirely in the browser.

```abap
CLASS z2ui5_cl_sample_clipboard DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_text TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_clipboard IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).
        mv_text = `Hello from abap2UI5`.

        client->view_display( z2ui5_cl_xml_view=>factory(
            )->page(
                )->input( client->_bind_edit( mv_text )
                )->button(
                    text  = `copy to clipboard`
                    press = client->_event( `COPY` )
            )->stringify( ) ).

      WHEN client->check_on_event( `COPY` ).
        client->action(
            val   = client->cs_event-clipboard_copy
            t_arg = VALUE #( ( mv_text ) ) ).
        client->message_toast_display( `copied` ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

#### Copy the App State URL

To share the current app state instead of a custom string, use `clipboard_app_state` — see [App State, Share, Bookmark](../expert_more/app_state_share.md).

::: warning
The browser's Clipboard API requires HTTPS (or `localhost`). On plain HTTP the call is silently ignored.
:::
