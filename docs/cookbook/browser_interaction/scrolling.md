---
outline: [2, 4]
---
# Scrolling

Read the current scroll positions from the backend, or scroll a control programmatically from the backend with two frontend events:

- **`cs_event-scroll_to`** — scroll a container to a specific pixel position.
- **`cs_event-scroll_into_view`** — bring a control into the viewport, wherever the surrounding scroll container currently sits.

Useful for jump-to-top buttons, restoring positions after navigation, or revealing a row after a backend search.

#### Read the Scroll Position

`client->get( )-s_scroll` reports the scroll positions of the page and any open dialogs at the moment the event was fired. Each container exposes the id of the scrollable element and its `x` / `y` offsets in pixels. `main` is the page's own scroll container, `nest` / `nest2` are the first and second [nested views](/cookbook/view/nested_views), and `popup` / `popover` are open dialogs.

```abap
DATA(scroll) = client->get( )-s_scroll.

DATA(main_y)    = scroll-main-y.       " main page
DATA(nest_y)    = scroll-nest-y.       " first nested view
DATA(nest2_y)   = scroll-nest2-y.      " second nested view
DATA(popup_y)   = scroll-popup-y.      " open popup
DATA(popover_y) = scroll-popover-y.    " open popover
```

#### Scroll to a Position

Pass the control id and the vertical position. Optionally also a horizontal position and a scroll behavior (`auto`, `smooth`, or `instant`):

```abap
CLASS z2ui5_cl_sample_scrolling DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

ENDCLASS.

CLASS z2ui5_cl_sample_scrolling IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
      DATA(page) = z2ui5_cl_xml_view=>factory( )->page( id = `id_page` ).
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
        client->action( val   = client->cs_event-scroll_to
                        t_arg = VALUE #( ( `id_page` ) ( `0` ) ) ).
      WHEN `SCROLL_BOTTOM`.
        client->action( val   = client->cs_event-scroll_to
                        t_arg = VALUE #( ( `id_page` ) ( `99999` ) ) ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

The arguments for `scroll_to` are `id`, `scrollTop` (y, px), `scrollLeft` (x, px, optional), and `behavior` (optional):

```abap
client->action( val   = client->cs_event-scroll_to
                t_arg = VALUE #( ( `id_page` ) ( `500` ) ( `0` ) ( `smooth` ) ) ).
```

#### Scroll an Element into View

To reveal a specific control — e.g. a row after a search — use `scroll_into_view` with the target control's id:

```abap
client->action( val   = client->cs_event-scroll_into_view
                t_arg = VALUE #( ( `id_row_42` ) ) ).
```

Additional optional arguments mirror the browser's `scrollIntoView` options: `behavior` (`smooth` | `auto` | `instant`), `block` (`start` | `center` | `end` | `nearest`), `inline` (`nearest` | `start` | `center` | `end`):

```abap
client->action( val   = client->cs_event-scroll_into_view
                t_arg = VALUE #( ( `id_row_42` ) ( `smooth` ) ( `center` ) ) ).
```
