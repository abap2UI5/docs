---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_011
  - z2ui5_cl_smp_app_019
  - z2ui5_cl_smp_app_006
  - z2ui5_cl_smp_app_045
  - z2ui5_cl_smp_app_053
  - z2ui5_cl_smp_app_059
  - z2ui5_cl_smp_app_070
  - z2ui5_cl_smp_app_160
  - z2ui5_cl_smp_app_143
  - z2ui5_cl_smp_app_459
description: Tables in abap2UI5 - binding an internal table to a UI5 table, editable cells, nested structures, and the working samples for each.
---
# Tables

This section walks through rendering tabular and nested data in views.

## Basic Table
The example below binds a simple table to a UI5 control:
```abap
CLASS zcl_sample_table DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    TYPES:
      BEGIN OF ty_row,
        count      TYPE i,
        value      TYPE string,
        descr      TYPE string,
      END OF ty_row.
    DATA mt_itab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_table IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      DO 100 TIMES.
        INSERT VALUE #(
          count = sy-index
          value = `red`
          descr = `this is a description` ) INTO TABLE mt_itab.
      ENDDO.

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->ele( `Table`
                      )->a( n = `items` v = client->_bind( mt_itab )

                      )->ele( `columns`
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Count`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Value`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Description`
                          )->end(

                      )->end(

                      )->ele( `items`
                          )->ele( `ColumnListItem`
                              )->ele( `cells`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{COUNT}`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{VALUE}`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{DESCR}` ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

## Editable
To make a table editable, use editable cell controls (e.g. `input`) — the
binding is the same `_bind`:
```abap
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      DO 100 TIMES.
        INSERT VALUE #(
          count = sy-index
          value = `red`
          descr = `this is a description` ) INTO TABLE mt_itab.
      ENDDO.

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->ele( `Table`
                      )->a( n = `items` v = client->_bind( mt_itab )

                      )->ele( `columns`
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Count`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Value`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Description`
                          )->end(

                      )->end(

                      )->ele( `items`
                          )->ele( `ColumnListItem`
                              )->ele( `cells`
                                  )->tag( `Input`
                                      )->a( n = `value` v = `{COUNT}`
                                  )->tag( `Input`
                                      )->a( n = `value` v = `{VALUE}`
                                  )->tag( `Input`
                                      )->a( n = `value` v = `{DESCR}` ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
```

## Nested Structures
You can also bind nested structures — use `structure/component` as the binding
path:
```abap
CLASS zcl_sample_nested_struc DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_tab,
        product TYPE string,
        BEGIN OF s_details,
          create_date TYPE string,
          create_by   TYPE string,
        END OF s_details,
      END OF ty_s_tab.
    DATA mt_itab TYPE STANDARD TABLE OF ty_s_tab WITH EMPTY KEY.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_nested_struc IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      mt_itab = VALUE #(
          ( product = `table` s_details = VALUE #( create_date = `01.01.2023` create_by = `Peter`  ) )
          ( product = `chair` s_details = VALUE #( create_date = `25.10.2022` create_by = `Frank`  ) )
          ( product = `sofa`  s_details = VALUE #( create_date = `12.03.2024` create_by = `George` ) ) ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Table`
                  )->a( n = `items` v = client->_bind( mt_itab )

                  )->ele( `columns`
                      )->ele( `Column`
                          )->tag( `Text`
                              )->a( n = `text` v = `Product`
                      )->end(
                      )->ele( `Column`
                          )->tag( `Text`
                              )->a( n = `text` v = `Created at`
                      )->end(
                      )->ele( `Column`
                          )->tag( `Text`
                              )->a( n = `text` v = `By`
                      )->end(

                  )->end(

                  )->ele( `items`
                      )->ele( `ColumnListItem`
                          )->ele( `cells`
                              )->tag( `Text`
                                  )->a( n = `text` v = `{PRODUCT}`
                              )->tag( `Text`
                                  )->a( n = `text` v = `{S_DETAILS/CREATE_DATE}`
                              )->tag( `Text`
                                  )->a( n = `text` v = `{S_DETAILS/CREATE_BY}` ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

## Selection

A row's selection is data like any cell. Bind the item's `selected` property
to a boolean field of the row and set the table's `mode`: in `SingleSelect`,
`SingleSelectLeft`, `SingleSelectMaster` or `MultiSelect` the user's
selection is written into that field before the next `main( )` runs, so the
handler reads the internal table and nothing else. In the default mode
`None` no row is selectable and `selected` is ignored. `selectionChange` is
the event for reacting to the click itself; a handler that only needs the
selection when a button is pressed does not need it:

```abap
CLASS zcl_sample_table_select DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_row,
        selkz TYPE abap_bool,
        title TYPE string,
        value TYPE string,
      END OF ty_s_row.
    DATA t_tab TYPE STANDARD TABLE OF ty_s_row WITH EMPTY KEY.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS view_display.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_table_select IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    me->client = client.

    CASE abap_true.
      WHEN client->check_on_init( ).
        t_tab = VALUE #( ( title = `entry 01` value = `red` )
                         ( title = `entry 02` value = `blue` )
                         ( title = `entry 03` value = `green` ) ).
        view_display( ).

      WHEN client->check_on_navigated( ).
        view_display( ).

      WHEN client->check_on_event( `SELECTED` ).
        DATA(lv_count) = REDUCE i( INIT n = 0 FOR ls IN t_tab WHERE ( selkz = abap_true ) NEXT n = n + 1 ).
        client->message_toast_display( |{ lv_count } selected| ).

      WHEN client->check_on_event( `ADD` ).
        INSERT VALUE #( title = |entry { lines( t_tab ) + 1 }| ) INTO TABLE t_tab.

      WHEN client->check_on_event( `DELETE` ).
        DELETE t_tab WHERE selkz = abap_true.

    ENDCASE.

  ENDMETHOD.

  METHOD view_display.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->a( n = `title` v = `Selection`

                )->ele( `Table`
                    )->a( n = `items`           v = client->_bind( t_tab )
                    )->a( n = `mode`            v = `MultiSelect`
                    )->a( n = `selectionChange` v = client->_event( `SELECTED` )

                    )->ele( `headerToolbar`
                        )->ele( `OverflowToolbar`
                            )->tag( `Title`
                                )->a( n = `text` v = `Rows`
                            )->tag( `ToolbarSpacer`
                            )->tag( `Button`
                                )->a( n = `text`  v = `add`
                                )->a( n = `press` v = client->_event( `ADD` )
                            )->tag( `Button`
                                )->a( n = `text`  v = `delete selected`
                                )->a( n = `press` v = client->_event( `DELETE` )
                        )->end(
                    )->end(

                    )->ele( `columns`
                        )->ele( `Column`
                            )->tag( `Text`
                                )->a( n = `text` v = `Title`
                        )->end(
                        )->ele( `Column`
                            )->tag( `Text`
                                )->a( n = `text` v = `Value`
                        )->end(
                    )->end(

                    )->ele( `items`
                        )->ele( `ColumnListItem`
                            )->a( n = `selected` v = `{SELKZ}`
                            )->ele( `cells`
                                )->tag( `Text`
                                    )->a( n = `text` v = `{TITLE}`
                                )->tag( `Input`
                                    )->a( n = `value` v = `{VALUE}` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

### Adding and Deleting Rows

The `ADD` and `DELETE` branches above are the whole recipe: change the
internal table and return. The framework compares the model before and after
`main( )` and pushes the difference, so the new row appears and the deleted
ones go without a `view_display( )`. Deleting *selected* rows is a
`DELETE … WHERE selkz = abap_true` because the selection is already in the
table when the event arrives. Sample `Z2UI5_CL_SMP_APP_011` adds an
edit/display switch to the same shape; there the button *text* is rendered
from a flag rather than bound, which is the one case that does need a
re-render.

## Growing

A `sap.m.Table` renders every row of its binding at once. For a long table
set `growing`, and it renders `growingThreshold` rows and loads the next
batch on *More* — or, inside a `ScrollContainer` with `growingScrollToLoad`,
when the user scrolls to the end. The data is still sent whole; growing only
spreads the rendering. Sample `Z2UI5_CL_SMP_APP_006` is ten thousand rows
with a sticky header:

```abap
)->ele( `ScrollContainer`
    )->a( n = `height`   v = `70%`
    )->a( n = `vertical` b = abap_true
    )->ele( `Table`
        )->a( n = `items`               v = client->_bind( t_tab )
        )->a( n = `growing`             b = abap_true
        )->a( n = `growingThreshold`    v = `20`
        )->a( n = `growingScrollToLoad` b = abap_true
        )->a( n = `sticky`              v = `ColumnHeaders,HeaderToolbar`
```

The binding itself stops at 100 rows regardless of `growing` — that is the
UI5 model's size limit, raised per view slot with `cs_event-set_size_limit`;
see [Size Limit](/cookbook/model/size_limit).

## Sorting and Filtering

Two places can do it. **In the backend**, a sort button is an event: `SORT
t_tab BY value` in its branch, and the sorted table reaches the view with the
model push — no re-render, and the order survives the next roundtrip because
it *is* the data (`Z2UI5_CL_SMP_APP_006`). A search field works the same way
with a `DELETE` or a filtered re-read (`Z2UI5_CL_SMP_APP_045`,
`Z2UI5_CL_SMP_APP_053`); for a live search over every keystroke, wire
`liveChange` with `s_ctrl-check_queue_last` and `check_no_busy`, as
[Backend](/cookbook/event_navigation/backend#the-per-wire-options-s-ctrl)
explains and `Z2UI5_CL_SMP_APP_059` shows.

**In the browser**, `cs_event-binding_call` applies a filter or sorter to the
aggregation binding of a control — the controller's
`getBinding( 'items' ).filter( … )` — and the model data stays untouched. The
arguments are positional: id, aggregation, method, then the method's own
parameters. `filter` takes path, operator, value1 and an optional value2;
empty values clear the filter. `sort` takes path, descending and group as
`X`/``:

```abap
" wired into a SearchField: filters the table in the browser, no roundtrip
)->tag( `SearchField`
    )->a( n = `liveChange` v = client->follow_up_action(
                                 val   = client->cs_event-binding_call
                                 t_arg = VALUE #( ( `tab` ) ( `items` ) ( `filter` )
                                                  ( `TITLE` ) ( `Contains` ) ( `${$parameters>/newValue}` ) ) ) )
```

```abap
" scheduled after a handler: sort the rendered table by VALUE, descending
client->follow_up_action( val   = client->cs_event-binding_call
                          t_arg = VALUE #( ( `tab` ) ( `items` ) ( `sort` )
                                           ( `VALUE` ) ( `X` ) ) ).
```

The table needs an `id` (`tab` above) for the call to find it. A client-side
sort is cosmetic: the internal table keeps its order, and a `view_display( )`
renders the binding fresh.

## Events on Cell Level

A cell is a control, so it fires events like any other, and `t_arg` tells the
handler which row it was. Relative paths work inside the row template —
`${INDEX}` is the row's own field — and `$source` is the cell that fired:

```abap
)->tag( `Input`
    )->a( n = `value`  v = `{PL_TOTAL}`
    )->a( n = `submit` v = client->_event( val   = `PL_TOTAL_CHANGE`
                                           t_arg = VALUE #( ( `${INDEX}` )
                                                            ( `${$parameters>/value}` ) ) ) )
```

```abap
ELSEIF client->check_on_event( `PL_TOTAL_CHANGE` ).
  DATA(lv_index) = client->get_event_arg( ).
  DATA(lv_value) = client->get_event_arg( 2 ).
```

The edited value is in the internal table by then as well — the delta travels
with the event — so the arguments are for knowing *where* the edit happened,
not for carrying it. `Z2UI5_CL_SMP_APP_160` reads five arguments from one
`sap.ui.table` cell, including the ids of the cell and of its row.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Editable Cells, Add and Delete Rows | [`Z2UI5_CL_SMP_APP_011`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_011.clas.abap) |
| Selection Modes: Single and Multi Select | [`Z2UI5_CL_SMP_APP_019`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_019.clas.abap) |
| Large Table with Growing and ScrollContainer | [`Z2UI5_CL_SMP_APP_006`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_006.clas.abap) |
| Filter Rows in the Backend | [`Z2UI5_CL_SMP_APP_045`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_045.clas.abap) |
| Search in the Backend (SearchField) | [`Z2UI5_CL_SMP_APP_053`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_053.clas.abap) |
| Live Search over a Large Table | [`Z2UI5_CL_SMP_APP_059`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_059.clas.abap) |
| Full Example with sap.ui.table | [`Z2UI5_CL_SMP_APP_070`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_070.clas.abap) |
| Events on Cell Level | [`Z2UI5_CL_SMP_APP_160`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_160.clas.abap) |
| Keep Column Filters on Refresh (C) | [`Z2UI5_CL_SMP_APP_143`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_143.clas.abap) |
| Drag and Drop Rows (A) | [`Z2UI5_CL_SMP_APP_459`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_459.clas.abap) |

<!-- samples:end -->
