---
outline: [2, 4]
description: Windowed tables in abap2UI5 - client->window_bind_tab( ) binds the rows the app has loaded and the total, and the list binding asks the app for more rows, or for a new sort or filter, the way an OData service is asked. In this shape from the next release on; 1.149.0 and 1.150.0 had a server-side window.
---
# Windowed Tables

A table bound with `client->_bind( )` travels whole: every row is in the model
of every response that sends it, and the browser holds all of them. That is
right for a few hundred rows. For ten thousand it is not. The browser parses
rows nobody looks at, and the
[size limit](/cookbook/model/size_limit) has to be raised before it shows them
at all.

`client->window_bind_tab( )` binds the rows **your app has loaded so far** -
the first 100, say - together with the number of rows there are in all. When
the list grows or scrolls past them, or the user sorts or filters it, the
list binding asks your app for rows, and your app reads them: the way an OData
service answers `$skip`, `$top`, `$orderby` and `$filter`. The framework
neither sorts nor filters nor counts. *In this shape from 1.151.0
on - 1.149.0 and 1.150.0 had a server-side window, see
[Coming from 1.150.0](#coming-from-1-150-0).*

| I want to… | Section |
| --- | --- |
| know whether I need it | [When to Use It](#when-to-use-it) |
| bind a table this way | [The Call](#the-call) |
| answer the browser's requests | [The Request](#the-request) |
| see a whole app | [A Complete Example](#a-complete-example) |
| use a `sap.ui.table.Table` | [Grid Table](#grid-table) |
| know which row an event means | [Which Row Was Pressed](#which-row-was-pressed) |
| know what does not work | [Limits](#limits) |
| move an app from 1.149.0 or 1.150.0 | [Coming from 1.150.0](#coming-from-1-150-0) |

## When to Use It

Use it when a table is too large to send whole: thousands of rows that the
user scrolls through, sorts and filters, but never reads all at once - above
all rows that come from the database, where your app can read exactly the
window it needs. Keep `_bind( )` for a table the user sees in full, a table of
a few dozen rows, and a table inside the rows of another table.

## The Call

`window_bind_tab( )` replaces `_bind( )` in the `items` of a list or the
`rows` of a grid table, and returns the same kind of binding string. The row
template binds relatively, exactly as with `_bind( )`:

```abap
)->ele( `Table`
    )->a( n = `items`            v = client->window_bind_tab( val    = mt_rows
                                                              total  = mv_total
                                                              event  = `ROWS`
                                                              window = 50 )
    )->a( n = `growing`          v = `true`
    )->a( n = `growingThreshold` v = `20`
```

| Parameter | What it is |
| --- | --- |
| `val` | the rows loaded so far, in the order the browser shows them: a public `STANDARD` table attribute of the app, passed like `_bind( )`. A data reference to one works too - `REF #( mt_rows )`, or an attribute `TYPE REF TO data` pointing to a table |
| `total` | the rows there are in all - what the list shows as its length. Pass an attribute of the app: the framework reads it anew with every response, so an event that loads rows only sets it |
| `event` | the event of your app the browser's requests arrive as |
| `window` | the rows the browser asks for at a time; default 100, at least 1 |

A growing `sap.m.Table` then shows *More [50 / 5000]*, and a
`sap.ui.table.Table` a scroll bar for all rows. To show the count, bind the
total like any value: `client->_bind( mv_total )`.

## The Request

When the browser needs rows it does not hold, your `main( )` runs with the
event you named, and `client->get( )-r_event_data` points to a
`z2ui5_if_client=>ty_s_window_request`:

| Component | What it is |
| --- | --- |
| `reason` | why the browser asks, a value of `z2ui5_if_client=>cs_window_reason` |
| `skip` | the rows the browser holds - for `MORE`, where to go on |
| `top` | the rows it wants now |
| `t_sort` | the sorters of the binding, type `ty_t_bind_tab_sort`: `path` (the upper-case column) and `descending` - for `SORT` and `FILTER` |
| `t_filter` | the filter of the binding, type `ty_t_bind_tab_filter`: `group`, `path`, `operator`, `value1`, `value2` - for `SORT` and `FILTER` |
| `path` | the model path of the table - which table asks, when several share one event |

The three reasons:

- **`MORE`** - the list grew or scrolled past the rows it holds. Append `top`
  rows to the table, after the ones it has.
- **`SORT`** - a column was sorted. Read the first `top` rows again, in the
  order of `t_sort`.
- **`FILTER`** - a column was filtered or a search ran. Read the first `top`
  rows again, of the rows `t_filter` lets through, and set the total.

`SORT` and `FILTER` both carry the binding's sorters **and** its filter, so
your app always gets the whole state of the columns in one request. A row
passes a filter group when it meets one of the group's conditions, and the
filter when it passes every group. The operators are the names of
`sap.ui.model.FilterOperator`: `EQ`, `NE`, `LT`, `LE`, `GT`, `GE`, `BT`, `NB`,
`Contains`, `NotContains`, `StartsWith`, `NotStartsWith`, `EndsWith` and
`NotEndsWith`; `value2` is the upper bound of `BT` and `NB`. What a sort or a
filter means is your app's to decide - an `ORDER BY` and a `WHERE`, a CDS
view, an RFC.

Nothing is rendered again: change the table and the total, and the response
pushes both. The request shows no busy indicator for `MORE`.

A search field and a sort button that ask the app, with
`cs_event-binding_call` ([Frontend Actions](/cookbook/event_navigation/frontend#calling-control-methods-on-the-frontend)).
The table needs an `id` for the call to find it:

```abap
)->tag( `SearchField`
    )->a( n = `search` v = client->follow_up_action( val   = client->cs_event-binding_call
                                                     t_arg = VALUE #( ( `tab` )
                                                                      ( `items` )
                                                                      ( `filter` )
                                                                      ( `NAME` )
                                                                      ( `Contains` )
                                                                      ( `${$parameters>/query}` ) ) )
)->tag( `Button`
    )->a( n = `text`  v = `Price descending`
    )->a( n = `press` v = client->follow_up_action( val   = client->cs_event-binding_call
                                                    t_arg = VALUE #( ( `tab` )
                                                                     ( `items` )
                                                                     ( `sort` )
                                                                     ( `PRICE` )
                                                                     ( `X` ) ) )
```

On a table bound with `_bind( )` the same two calls sort and filter in the
browser ([Tables](/cookbook/model/tables#in-the-browser)). On a windowed table
they become a request to your app. The view does not change.

## A Complete Example

Five thousand rows in a growing `sap.m.Table`, fifty at a time. The rows come
from `mt_db`, which stands in for a database table; the app reads windows of
it for the event `ROWS`. A row can be pressed, and the search field filters
by name:

```abap
CLASS zcl_sample_window DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_row,
        id    TYPE i,
        name  TYPE string,
        price TYPE p LENGTH 8 DECIMALS 2,
      END OF ty_s_row.
    TYPES ty_t_row TYPE STANDARD TABLE OF ty_s_row WITH EMPTY KEY.

    DATA mt_rows   TYPE ty_t_row.
    DATA mv_total  TYPE i.
    DATA mv_result TYPE string.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.
    " stands in for a database table
    DATA mt_db TYPE ty_t_row.
    DATA mt_sort TYPE z2ui5_if_client=>ty_t_bind_tab_sort.
    DATA mt_filter TYPE z2ui5_if_client=>ty_t_bind_tab_filter.

    METHODS view_display.
    METHODS on_rows.
    METHODS load
      IMPORTING
        skip TYPE i
        top  TYPE i.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_sample_window IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ).
      DO 5000 TIMES.
        INSERT VALUE #( id    = sy-index
                        name  = |Item { sy-index }|
                        price = ( sy-index * 37 ) MOD 251 ) INTO TABLE mt_db.
      ENDDO.
      load( skip = 0
            top  = 50 ).
      view_display( ).

    ELSEIF client->check_on_navigated( ).
      view_display( ).

    ELSEIF client->check_on_event( `ROWS` ).
      on_rows( ).

    ELSEIF client->check_on_event( `ROW` ).
      " the argument is the row's index in mt_rows, from 0
      mv_result = |{ mt_rows[ CONV i( client->get_event_arg( ) ) + 1 ]-name } pressed|.
    ENDIF.

  ENDMETHOD.

  METHOD on_rows.

    FIELD-SYMBOLS <request> TYPE z2ui5_if_client=>ty_s_window_request.

    DATA(lr_request) = client->get( )-r_event_data.
    ASSIGN lr_request->* TO <request>.

    IF <request>-reason = z2ui5_if_client=>cs_window_reason-more.
      load( skip = lines( mt_rows )
            top  = <request>-top ).
      RETURN.
    ENDIF.

    " SORT or FILTER: the new state of the columns, from the first row on
    mt_sort   = <request>-t_sort.
    mt_filter = <request>-t_filter.
    load( skip = 0
          top  = <request>-top ).

  ENDMETHOD.

  METHOD load.

    DATA lt_hit TYPE ty_t_row.

    " WHERE - the search field sends NAME Contains the query
    LOOP AT mt_db INTO DATA(ls_row).
      DATA(lv_match) = abap_true.
      LOOP AT mt_filter INTO DATA(ls_filter) WHERE path = `NAME`.
        IF to_upper( ls_row-name ) NS to_upper( ls_filter-value1 ).
          lv_match = abap_false.
        ENDIF.
      ENDLOOP.
      IF lv_match = abap_true.
        APPEND ls_row TO lt_hit.
      ENDIF.
    ENDLOOP.
    mv_total = lines( lt_hit ).

    " ORDER BY
    DATA(ls_sort) = VALUE #( mt_sort[ 1 ] OPTIONAL ).
    IF ls_sort-path IS NOT INITIAL.
      IF ls_sort-descending = abap_true.
        SORT lt_hit BY (ls_sort-path) DESCENDING.
      ELSE.
        SORT lt_hit BY (ls_sort-path) ASCENDING.
      ENDIF.
    ENDIF.

    " OFFSET skip, UP TO top ROWS
    IF skip = 0.
      CLEAR mt_rows.
    ENDIF.
    LOOP AT lt_hit INTO ls_row FROM skip + 1.
      IF lines( mt_rows ) >= skip + top.
        EXIT.
      ENDIF.
      APPEND ls_row TO mt_rows.
    ENDLOOP.

  ENDMETHOD.

  METHOD view_display.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->a( n = `title` v = `Windowed Table`

                )->ele( `Table`
                    )->a( n = `id`               v = `tab`
                    )->a( n = `items`            v = client->window_bind_tab( val    = mt_rows
                                                                              total  = mv_total
                                                                              event  = `ROWS`
                                                                              window = 50 )
                    )->a( n = `growing`          v = `true`
                    )->a( n = `growingThreshold` v = `20`

                    )->ele( `headerToolbar`
                        )->ele( `OverflowToolbar`
                            )->tag( `ObjectNumber`
                                )->a( n = `number` v = client->_bind( mv_total )
                                )->a( n = `unit`   v = `rows`
                            )->tag( `ToolbarSpacer`
                            )->tag( `Text`
                                )->a( n = `text` v = client->_bind( mv_result )
                            )->tag( `SearchField`
                                )->a( n = `width`  v = `12rem`
                                )->a( n = `search` v = client->follow_up_action( val   = client->cs_event-binding_call
                                                                                 t_arg = VALUE #( ( `tab` )
                                                                                                  ( `items` )
                                                                                                  ( `filter` )
                                                                                                  ( `NAME` )
                                                                                                  ( `Contains` )
                                                                                                  ( `${$parameters>/query}` ) ) )
                        )->end(
                    )->end(

                    )->ele( `columns`
                        )->ele( `Column`
                            )->tag( `Text`
                                )->a( n = `text` v = `Name`
                        )->end(
                        )->ele( `Column`
                            )->tag( `Text`
                                )->a( n = `text` v = `Price`
                        )->end(
                    )->end(

                    )->ele( `items`
                        )->ele( `ColumnListItem`
                            )->a( n = `type`  v = `Active`
                            )->a( n = `press` v = client->_event( val = `ROW`
                                                                  arg = `$event.getSource().getBindingContext().getPath().split('/').pop()` )
                            )->ele( `cells`
                                )->tag( `Text`
                                    )->a( n = `text` v = `{NAME}`
                                )->tag( `Text`
                                    )->a( n = `text` v = `{PRICE}` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

What to try:

- Press *More* twice. The first press stays inside the 50 rows the browser
  holds; the second runs `main( )` with `ROWS` and `MORE`, and the app
  appends the next 50.
- Search for `item 12`. The app filters, counts and sends the first rows of
  the result; the count in the toolbar follows.
- Press a row. Its index is the row of `mt_rows` - the table is exactly what
  the browser shows.

In a real app `load( )` is one `SELECT` with `WHERE`, `ORDER BY`,
`OFFSET skip` and `UP TO top ROWS`, plus a `SELECT COUNT(*)` for the total.

## Grid Table

A `sap.ui.table.Table` takes the same binding in `rows`. It asks for more rows
as the user scrolls. A column with `sortProperty` and `filterProperty` gets
the column menu, and what the user picks there arrives as `SORT` or `FILTER`:

```abap
)->ele( n = `Table` ns = `t`
    )->a( n = `rows`            v = client->window_bind_tab( val    = mt_rows
                                                             total  = mv_total
                                                             event  = `ROWS`
                                                             window = 30 )
    )->a( n = `visibleRowCount` v = `10`
    )->ele( n = `columns` ns = `t`
        )->ele( n = `Column` ns = `t`
            )->a( n = `sortProperty`   v = `NAME`
            )->a( n = `filterProperty` v = `NAME`
            )->ele( n = `label` ns = `t`
                )->tag( `Label`
                    )->a( n = `text` v = `Name`
            )->end(
            )->ele( n = `template` ns = `t`
                )->tag( `Text`
                    )->a( n = `text` v = `{NAME}` ).
```

The view declares `xmlns:t` as `sap.ui.table`. The column menu's filter sends
`Contains` for a text column.

## Which Row Was Pressed

The table bound is exactly what the browser shows, in that order. So the
index in a binding path that an event hands over - `/MT_ROWS/5` - is the row
of the table: `mt_rows[ 5 + 1 ]`. Send it from the row with
`$event.getSource().getBindingContext().getPath().split('/').pop()`, as in the
example above. A cell edited or a row ticked lands in the same row. When the
handler needs the data of the row rather than its position, sending a key
field (`${ID}`) works as for any table
([Tables](/cookbook/model/tables#which-row-was-clicked)).

## Limits

- **Only a `STANDARD` table, bound whole.** A sorted or hashed table is
  refused with an error that names the attribute, and so is a table bound
  elsewhere with `json`, `omit_initial` or a custom mapper. A list bound
  relative to a context - a table inside the rows of another table - is not
  windowed.
- **Sorters and filters in the view are ignored.** A `sorter` or `filters`
  written into the binding info of the view is not applied; the rows arrive
  in the order your app gives.
- **Not every filter has a wire form.** A filter with a test function, or a
  nesting deeper than an AND of ORs, is not sent; the browser logs
  `WINDOW: a filter of this shape is not sent to the server`.
- **The control has to ask by range.** A growing `sap.m.Table` or
  `sap.m.List` and a `sap.ui.table.Table` do. A list that does not grow asks
  for no range and shows the rows it holds.
- **`__WINDOW` is reserved.** The model carries the state of the window under
  `/__WINDOW/<path>`; do not name an attribute of the app that way.

## Coming from 1.150.0

In 1.149.0 and 1.150.0 the framework held the whole table, sorted and filtered
it on the server and answered the window requests without the app. That is gone:

- `window_bind_tab( )` takes `total` and `event` instead of `t_sort` and
  `t_filter`. `val` holds the loaded rows, not all of them.
- `window_get_row( )` is removed: the index of a row on screen is its index
  in the table.
- An order of the app's own is the order it loads in - set it before the
  first `load( )`, as sample
  [`z2ui5_cl_smp_app_547`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_547.clas.abap)
  does.

The full contract - what travels, in which order, and what a backend has to
do - is the specification
[Windowed tables](https://github.com/abap2UI5/protocol/blob/main/spec/windowed-tables.md)
of the abap2UI5 protocol.
