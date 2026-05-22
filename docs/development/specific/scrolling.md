---
outline: [2, 4]
---
# Scrolling

abap2UI5 offers the custom control `_z2ui5( )->scrolling( )` to read and set the scroll position of any container from the backend. Bind a name/value table of element IDs to scroll positions — the framework reports current positions back into that table on every roundtrip, and applies new positions to the DOM whenever the update flag is set.

This is useful for jump-to-top buttons, restoring scroll positions after navigation, or scrolling to a specific row after a backend search.

#### Basic Usage

Bind two variables to the control: a flag `setupdate` that triggers a write to the DOM, and an `items` table mapping element `id` to scroll position. Update the entry and call `view_model_update( )` — the browser scrolls the matching element:

```abap
CLASS z2ui5_cl_sample_scrolling DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_scrollupdate TYPE abap_bool.
    DATA mt_scroll       TYPE z2ui5_if_types=>ty_t_name_value.

ENDCLASS.

CLASS z2ui5_cl_sample_scrolling IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
      INSERT VALUE #( n = `id_page` ) INTO TABLE mt_scroll.

      DATA(page) = z2ui5_cl_xml_view=>factory( )->page( id = `id_page` ).
      page->_z2ui5( )->scrolling(
            setupdate = client->_bind_edit( mv_scrollupdate )
            items     = client->_bind_edit( mt_scroll ) ).

      page->footer( )->overflow_toolbar(
         )->button( text  = `Top`
                    press = client->_event( `SCROLL_TOP` )
         )->button( text  = `Bottom`
                    press = client->_event( `SCROLL_BOTTOM` ) ).

      client->view_display( page->stringify( ) ).
      RETURN.
    ENDIF.

    CASE client->get( )-event.
      WHEN `SCROLL_TOP`.
        mt_scroll[ n = `id_page` ]-v = `0`.
        mv_scrollupdate = abap_true.
        client->view_model_update( ).

      WHEN `SCROLL_BOTTOM`.
        mt_scroll[ n = `id_page` ]-v = `99999`.
        mv_scrollupdate = abap_true.
        client->view_model_update( ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

On every roundtrip the framework also writes the current scroll position of each tracked element back into `mt_scroll`, so the backend always knows where the user is. Setting `mv_scrollupdate = abap_true` switches the next update from read to write — the value in `mt_scroll` is applied to the DOM and the flag is reset by the frontend.

#### Notes

- Track multiple containers by inserting one row per element `id` into `mt_scroll`.
- For relative scrolling (e.g. "500 pixels down"), read the current value from `mt_scroll`, adjust it in ABAP, and set `mv_scrollupdate = abap_true`.
- See [sample 134](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_demo_app_134.clas.abap) for a full demo with top/up/down/bottom buttons.
