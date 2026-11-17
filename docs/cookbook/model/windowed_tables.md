---
outline: [2, 4]
description: Windowed tables in abap2UI5 - client->window_bind_tab( ) sends a large table to the browser a window at a time, sorts and filters it on the server, and window_get_row( ) maps a position the browser shows back to the row of the table. From 1.149.0 on.
---
# Windowed Tables

A table bound with `client->_bind( )` travels whole: every row is in the model
of every response that sends it, and the browser holds all of them. That is
right for a few hundred rows. For ten thousand it is not. The browser parses
rows nobody looks at, and the
[size limit](/cookbook/model/size_limit) has to be raised before it shows them
at all.

`client->window_bind_tab( )` binds the table so that the browser gets a
**window** of it - the first 100 rows, say - and asks for the next window when
the list grows or scrolls. The rows stay in the attribute of your app.
Sorting and filtering run on the server, over all rows. *From abap2UI5
1.149.0 on.*

| I want to… | Section |
| --- | --- |
| know whether I need it | [When to Use It](#when-to-use-it) |
| bind a table this way | [The Call](#the-call) |
| see a whole app | [A Complete Example](#a-complete-example) |
| use a `sap.ui.table.Table` | [Grid Table](#grid-table) |
| know which row an event means | [Which Row Was Pressed](#which-row-was-pressed) |
| sort or filter from the app | [The App's Order and the User's](#the-app-s-order-and-the-user-s) |
| know what does not work | [Limits](#limits) |

## When to Use It

Use it when a table is too large to send whole: thousands of rows that the
user scrolls through, sorts and filters, but never reads all at once. Keep
`_bind( )` for a table the user sees in full, a table of a few dozen rows, and
a table inside the rows of another table.

A windowed table changes three things at once:

- **What travels.** The response carries the rows of the window, not the
  table.
- **Who sorts and filters.** The server, over all rows. The browser holds
  only a part of them, so it cannot.
- **What the draft costs.** The table is kept next to the app in the draft,
  not inside its serialization. While its rows stay the same from one
  roundtrip to the next, it is not serialized again
  ([Performance](/configuration/performance#large-tables)).

## The Call

`window_bind_tab( )` replaces `_bind( )` in the `items` of a list or the
`rows` of a grid table, and returns the same kind of binding string. The row
template binds relatively, exactly as with `_bind( )`:

```abap
)->ele( `Table`
    )->a( n = `items`            v = client->window_bind_tab( val    = mt_rows
                                                              window = 50 )
    )->a( n = `growing`          v = `true`
    )->a( n = `growingThreshold` v = `20`
```

| Parameter | What it is |
| --- | --- |
| `val` | the table: a public `STANDARD` table attribute of the app, passed like `_bind( )`. A data reference to one works too - `REF #( mt_rows )`, or an attribute `TYPE REF TO data` pointing to a table |
| `window` | the rows the browser gets at a time; default 100, at least 1 |
| `t_sort` | the order of the rows, type `z2ui5_if_client=>ty_t_bind_tab_sort`: a column (the upper-case component name) and `descending`. The first sorter that tells two rows apart decides |
| `t_filter` | the rows that show, type `z2ui5_if_client=>ty_t_bind_tab_filter`: `group`, `path`, `operator`, `value1`, `value2`. A row passes a group when it meets one of its conditions, and the filter when it passes every group |

The operators are the names of `sap.ui.model.FilterOperator`: `EQ`, `NE`,
`LT`, `LE`, `GT`, `GE`, `BT`, `NB`, `Contains`, `NotContains`, `StartsWith`,
`NotStartsWith`, `EndsWith` and `NotEndsWith`. `value2` is the upper bound of
`BT` and `NB`. Numbers, dates (`yyyy-MM-dd` or `yyyyMMdd`), times and booleans
(`true` / `false`) compare as such; text compares without regard to case.

Next to the rows, the model carries the state of the window under
`/__WINDOW/<path>`: `TOTAL` (the rows the filter lets through), `COUNT` (the
rows sent) and `SIZE` (one window). Bind `TOTAL` to show the user how many
rows there are. `_bind_path( )` gives the path of the table:

```abap
)->tag( `Text`
    )->a( n = `text` v = |\{/__WINDOW{ client->_bind_path( mt_rows ) }/TOTAL\} rows|
```

## What the Browser Does

The list binding of a windowed table is one of the framework's own. It
answers its length with `TOTAL`, so a growing `sap.m.Table` shows
*More [20 / 5000]* and a `sap.ui.table.Table` scrolls over all rows.
When the control asks for rows past the ones it holds, the binding asks the
server for the next windows:

- **Growing and scrolling.** The request is a roundtrip of the framework's
  own. It does **not** call your `main( )`, and it shows no busy indicator.
  The server answers with more rows of the same order.
- **Sorting and filtering.** The column menu of a `sap.ui.table.Table`
  (`sortProperty`, `filterProperty`) and a `cs_event-binding_call` with
  `sort` or `filter` reach the server instead of sorting the rows in the
  browser. The server sorts or filters all rows and sends the first window of
  the new order. This roundtrip does not call `main( )` either.
- **Edits.** A cell edited or a row ticked is sent with the next roundtrip,
  as for any bound table, and lands in the row of the table it was made in -
  whatever order the browser showed.

A search field and a sort button that work on the server, with
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
they go to the server. The view does not change.

## A Complete Example

Five thousand rows in a growing `sap.m.Table`, fifty at a time. A row can be
ticked and pressed, a button counts the ticked rows, and another sets the
app's own order:

```abap
CLASS zcl_sample_window DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_row,
        id       TYPE i,
        name     TYPE string,
        price    TYPE p LENGTH 8 DECIMALS 2,
        selected TYPE abap_bool,
      END OF ty_s_row.
    DATA mt_rows   TYPE STANDARD TABLE OF ty_s_row WITH EMPTY KEY.
    DATA mv_result TYPE string.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS view_display.

  PRIVATE SECTION.
    CONSTANTS c_window TYPE i VALUE 50.

ENDCLASS.

CLASS zcl_sample_window IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ).
      DO 5000 TIMES.
        INSERT VALUE #( id    = sy-index
                        name  = |Item { sy-index }|
                        price = ( sy-index * 37 ) MOD 251 ) INTO TABLE mt_rows.
      ENDDO.
      view_display( ).

    ELSEIF client->check_on_navigated( ).
      view_display( ).

    ELSEIF client->check_on_event( `ROW` ).
      " the argument is a position in the order the browser shows, from 0
      DATA(lv_row) = client->window_get_row( val = mt_rows
                                             row = CONV i( client->get_event_arg( ) ) + 1 ).
      mv_result = COND #( WHEN lv_row > 0 THEN |{ mt_rows[ lv_row ]-name } pressed| ELSE `no row` ).

    ELSEIF client->check_on_event( `CHECK` ).
      DATA(lv_count) = REDUCE i( INIT n = 0 FOR ls IN mt_rows WHERE ( selected = abap_true ) NEXT n = n + 1 ).
      mv_result = |{ lv_count } rows ticked|.

    ELSEIF client->check_on_event( `HIGHEST_ID` ).
      " the app's order: binding the table again is enough, no view_display( )
      client->window_bind_tab( val    = mt_rows
                               window = c_window
                               t_sort = VALUE #( ( path = `ID` descending = abap_true ) ) ).
    ENDIF.

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
                                                                              window = c_window )
                    )->a( n = `growing`          v = `true`
                    )->a( n = `growingThreshold` v = `20`

                    )->ele( `headerToolbar`
                        )->ele( `OverflowToolbar`
                            )->tag( `Title`
                                )->a( n = `text` v = |\{/__WINDOW{ client->_bind_path( mt_rows ) }/TOTAL\} rows|
                            )->tag( `ToolbarSpacer`
                            )->tag( `Text`
                                )->a( n = `text` v = client->_bind( mv_result )
                            )->tag( `Button`
                                )->a( n = `text`  v = `Highest id first`
                                )->a( n = `press` v = client->_event( `HIGHEST_ID` )
                            )->tag( `Button`
                                )->a( n = `text`  v = `Count ticked`
                                )->a( n = `press` v = client->_event( `CHECK` )
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
                        )->ele( `Column`
                            )->tag( `Text`
                                )->a( n = `text` v = `Ticked`
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
                                    )->a( n = `text` v = `{PRICE}`
                                )->tag( `CheckBox`
                                    )->a( n = `selected` v = `{SELECTED}` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

What to try:

- Press *More* twice. The first press stays inside the 50 rows the browser
  holds; the second asks the server for the next window. `main( )` runs
  for neither.
- Tick a few rows, press *Highest id first*, tick more, press *Count
  ticked*. The count reads the internal table in its own order and finds
  every tick in the row it was made in.
- Press a row. The handler names the row of `mt_rows` behind it, also after
  the sort.

`CHECK` loops over `mt_rows` like any table: the rows are all there, in the
order the app filled them. Only the browser sees them sorted.

## Grid Table

A `sap.ui.table.Table` takes the same binding in `rows`. It asks for the next
window as the user scrolls. A column with `sortProperty` and `filterProperty`
gets the column menu, and both run on the server:

```abap
)->ele( n = `Table` ns = `t`
    )->a( n = `rows`            v = client->window_bind_tab( val    = mt_rows
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

The view declares `xmlns:t` as `sap.ui.table`. The column menu calls
`sort( )` and `filter( )` on the binding, and the binding sends both to the
server.

## Which Row Was Pressed

The browser holds the rows in the order the server sorted and filtered them.
So the index in a binding path that an event hands over - `/MT_ROWS/5` - is a
**position in that order**, not the row of the table. Row 6 on screen may be
row 4711 of `mt_rows`.

`client->window_get_row( )` answers which row it is, in the same order the
response was built with:

```abap
DATA(lv_row) = client->window_get_row( val = mt_rows
                                       row = lv_position + 1 ).
```

| Parameter | What it is |
| --- | --- |
| `val` | the table, as passed to `window_bind_tab( )` - the table or a data reference to it |
| `row` | the position in the browser, counted from 1: the index of the binding path plus 1 |
| result | the row of the table, counted from 1; `0` when the position is past the rows the filter lets through |

For a table that is not windowed it answers `row` itself. Send the position
from the row with
`$event.getSource().getBindingContext().getPath().split('/').pop()`, as in the
example above. When the handler needs the data of the row rather than its
position, sending a key field (`${ID}`) and reading the table by it is simpler
and needs no mapping ([Tables](/cookbook/model/tables#which-row-was-clicked)).

## The App's Order and the User's

The server keeps one order per windowed table - a sort and a filter - and
nobody else does. The next window is more rows of the same order, whoever set
it, and `window_get_row( )` and every edit follow it. Two parties can change
it: the app, with `t_sort` and `t_filter`, and the user, through the list
binding (the column menu, a `binding_call`).

- **There is one sort: the last one set.** A sort the user picks replaces the
  app's, and a new sort from the app replaces the user's.
- **The filter has two parts, and a row has to pass both.** `t_filter` and the
  filter of the binding apply together. A column filter narrows what `t_filter`
  lets through, a new `t_filter` keeps the user's column filter, and clearing
  one leaves the other.
- **`t_sort` and `t_filter` take effect when they change.** They apply when
  they differ from the ones the previous `window_bind_tab( )` of the table
  passed. So a view rendered again keeps what the user sorted and filtered,
  and a new order from the app - from a dialog, a layout, a button - replaces
  it. An empty table clears it the same way. The other side of this: passing
  the same `t_sort` a second time does not bring it back after the user
  sorted by a column; the app has to pass a different one.

Binding the table again in an event handler is enough to set a new order; the
example's `HIGHEST_ID` branch does nothing else. The new window reaches the
browser in the same roundtrip, without a `view_display( )`.

## Limits

- **Only a `STANDARD` table, bound whole.** A sorted or hashed table is
  refused with an error that names the attribute, and so is a table bound
  elsewhere with `json`, `omit_initial` or a custom mapper. A list bound
  relative to a context - a table inside the rows of another table - is not
  windowed.
- **Sorters and filters in the view are ignored.** A `sorter` or `filters`
  written into the binding info of the view is not applied; the rows arrive
  in the order the server gives. Use `t_sort` and `t_filter`, or the
  binding's own `sort` and `filter`.
- **Not every filter has a form for the server.** A filter with a test
  function, or a nesting deeper than an AND of ORs, is not sent; the browser
  logs `WINDOW: a filter of this shape is not sent to the server`.
- **The control has to ask by range.** A growing `sap.m.Table` or
  `sap.m.List` and a `sap.ui.table.Table` do. A list that does not grow asks
  for no range and shows the window it holds.
- **The browser never sends the table whole.** An edit travels as a change of
  single cells. A whole table value sent back for a windowed table is refused,
  so the table cannot be cut down to the window by accident.
- **A data reference costs more in the draft.** A table behind a data
  reference is serialized with the reference on every roundtrip; only a table
  attribute gets the reuse described under
  [Performance](/configuration/performance#large-tables).
- **`__WINDOW` is reserved.** Do not name an attribute of the app that way.

The full contract - what travels, in which order, and what a backend has to
do - is the specification
[Windowed tables](https://github.com/abap2UI5/protocol/blob/main/spec/windowed-tables.md)
of the abap2UI5 protocol.
