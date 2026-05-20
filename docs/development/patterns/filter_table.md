---
outline: [2, 4]
---
# Filter, Search and Table

The closest UI5 cousin of an ALV report: a table with a search field and one or two filter dropdowns above it. The user types, picks filters, the list narrows.

The split of work is simple — UI5 takes the input, ABAP does the filtering. Each filter change fires a backend event, the handler re-runs the `SELECT` (or filters the in-memory table), and the table re-renders.

#### Step 1 — The bare table

Start with the result table. No filters yet — just bind the data and show columns. This is the [tables](../model/tables.md) pattern unchanged:

```abap
DATA(view) = z2ui5_cl_xml_view=>factory( )->page( `Sales Orders` ).
DATA(tab)  = view->table( client->_bind( mt_orders ) growing = abap_true ).
tab->columns( )->column( )->text( `Order` )->get_parent(
              )->column( )->text( `Customer` )->get_parent(
              )->column( )->text( `Value` ).
tab->items( )->column_list_item( )->cells(
   )->text( `{VBELN}` )->text( `{KUNNR}` )->text( `{NETWR}` ).
```

#### Step 2 — A search field in the toolbar

Add a header toolbar to the table with a `search_field`. Bind its `value` two-way to a filter string and fire `SEARCH` on enter or button click. The `liveChange` event would fire on every keystroke — usually too chatty for a backend roundtrip:

```abap
DATA(tab) = view->table( client->_bind( mt_orders ) growing = abap_true ).
tab->header_toolbar( )->overflow_toolbar(
    )->title( `Sales Orders`
    )->toolbar_spacer( )
    )->search_field(
        value  = client->_bind_edit( mv_search )
        width  = `20rem`
        search = client->_event( `SEARCH` ) ).
```

#### Step 3 — A filter dropdown

Add a `select` next to the search field for typed filtering — sales organisation, status, type — anything with a small fixed set of values. A `change` event makes it re-filter immediately, without a separate apply button:

```abap
tab->header_toolbar( )->overflow_toolbar(
    )->title( `Sales Orders`
    )->toolbar_spacer( )
    )->label( `Org:` )
    )->select(
        selectedkey = client->_bind_edit( mv_org )
        change      = client->_event( `APPLY_FILTER` )
        )->items( )->core_item( key = ``     text = `All`
                      )->core_item( key = `1000` text = `Germany`
                      )->core_item( key = `2000` text = `France` )->get_parent(
    )->search_field(
        value  = client->_bind_edit( mv_search )
        width  = `20rem`
        search = client->_event( `APPLY_FILTER` ) ).
```

#### Step 4 — Apply the filter in ABAP

One event handler covers both filter sources. Use a clean re-`SELECT` for small datasets, or — when the source is already in memory — filter with `FILTER` / `DELETE … WHERE`:

```abap
WHEN client->check_on_event( `APPLY_FILTER` ).
  SELECT FROM vbak
    FIELDS vbeln, kunnr, netwr, vkorg
    WHERE   ( @mv_org      = ''  OR vkorg = @mv_org )
      AND   ( @mv_search   = ''  OR vbeln LIKE @( `%` && mv_search && `%` )
                              OR  kunnr LIKE @( `%` && mv_search && `%` ) )
    INTO TABLE @mt_orders
    UP TO 100 ROWS.
  client->view_model_update( ).
```

#### Full Example

A complete order list with search and a sales-org filter, both wired to the same event:

```abap
CLASS z2ui5_cl_sample_filter_table DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    TYPES:
      BEGIN OF ty_order,
        vbeln TYPE vbak-vbeln,
        kunnr TYPE vbak-kunnr,
        netwr TYPE vbak-netwr,
        vkorg TYPE vbak-vkorg,
      END OF ty_order.
    DATA mt_orders TYPE STANDARD TABLE OF ty_order WITH EMPTY KEY.
    DATA mv_search TYPE string.
    DATA mv_org    TYPE vbak-vkorg.

ENDCLASS.

CLASS z2ui5_cl_sample_filter_table IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).
        load_data( ).
        render( ).

      WHEN client->check_on_event( `APPLY_FILTER` ).
        load_data( ).
        client->view_model_update( ).

    ENDCASE.

  ENDMETHOD.

  METHOD load_data.

    SELECT FROM vbak
      FIELDS vbeln, kunnr, netwr, vkorg
      WHERE ( @mv_org    = ''  OR vkorg = @mv_org )
        AND ( @mv_search = ''  OR vbeln LIKE @( `%` && mv_search && `%` )
                              OR kunnr LIKE @( `%` && mv_search && `%` ) )
      INTO TABLE @mt_orders
      UP TO 100 ROWS.

  ENDMETHOD.

  METHOD render.

    DATA(view) = z2ui5_cl_xml_view=>factory( )->shell( )->page( `Sales Orders` ).
    DATA(tab)  = view->table( client->_bind( mt_orders ) growing = abap_true ).

    tab->header_toolbar( )->overflow_toolbar(
        )->title( `Sales Orders`
        )->toolbar_spacer( )
        )->label( `Org:` )
        )->select(
            selectedkey = client->_bind_edit( mv_org )
            change      = client->_event( `APPLY_FILTER` )
            )->items( )->core_item( key = ``     text = `All`
                          )->core_item( key = `1000` text = `Germany`
                          )->core_item( key = `2000` text = `France` )->get_parent(
        )->search_field(
            value  = client->_bind_edit( mv_search )
            width  = `20rem`
            search = client->_event( `APPLY_FILTER` ) ).

    tab->columns( )->column( )->text( `Order` )->get_parent(
                  )->column( )->text( `Customer` )->get_parent(
                  )->column( )->text( `Value` )->get_parent(
                  )->column( )->text( `Org` ).
    tab->items( )->column_list_item( )->cells(
       )->text( `{VBELN}` )->text( `{KUNNR}` )->text( `{NETWR}` )->text( `{VKORG}` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

::: tip
For more advanced patterns — saved filter variants, multi-range selection, fuzzy search — the [layout-variant add-on](https://github.com/abap2UI5-addons) and `Z2UI5_CL_POP_GET_RANGE` (see [Built-In Popups](../popups/built_in.md)) are worth a look.
:::
