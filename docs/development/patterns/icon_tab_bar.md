---
outline: [2, 4]
---
# IconTabBar

A page split into tabs — *General*, *Items*, *Status*, *Notes* — each one a small section instead of a long scroll. Standard on every Fiori detail screen.

In abap2UI5 it's a three-level chain: `icon_tab_bar` → `items` → one `icon_tab_filter` per tab. Each filter holds its own `content` block, and only one tab is rendered at a time on the frontend.

#### Step 1 — Two tabs

The smallest version — two tabs, each with a piece of text. Note the three nested calls: the *bar*, then *items*, then a *filter* per tab:

```abap
client->view_display( z2ui5_cl_xml_view=>factory(
    )->page( `Order 4711`
        )->icon_tab_bar(
            )->items(
                )->icon_tab_filter( text = `General`
                    )->content( )->text( `header data here` )->get_parent( )->get_parent(
                )->icon_tab_filter( text = `Items`
                    )->content( )->text( `line items here` )
    )->stringify( ) ).
```

`get_parent( )->get_parent( )` walks back up out of the first tab's `content` so the next `icon_tab_filter` is added as a sibling, not as a child.

#### Step 2 — Counters and icons

Tabs become much more useful with a count (*Items (12)*) or a status icon. Both are properties on `icon_tab_filter` — `count`, `icon`, `iconcolor` for the dot:

```abap
DATA(bar) = view->icon_tab_bar( )->items( ).

bar->icon_tab_filter(
    text  = `General`
    icon  = `sap-icon://hint`
    )->content( )->text( `header data here` ).

bar->icon_tab_filter(
    text  = `Items`
    count = |{ lines( mt_items ) }|
    icon  = `sap-icon://list`
    )->content( )->text( `line items here` ).

bar->icon_tab_filter(
    text       = `Status`
    icon       = `sap-icon://message-warning`
    iconcolor  = COND #( WHEN mv_has_errors = abap_true THEN `Negative` ELSE `Positive` )
    )->content( )->text( `status info here` ).
```

Cheap to read at a glance — the user sees the warning colour and the item count without opening the tab.

#### Step 3 — Real content per tab

Replace the placeholder `text` with whatever each tab needs — a form, a table, a tree. The tab is just a container, anything that works in a `page` works inside `content`:

```abap
DATA(tab_items) = bar->icon_tab_filter( text = `Items` count = |{ lines( mt_items ) }| )->content( ).
DATA(items_tab) = tab_items->table( client->_bind( mt_items ) ).
items_tab->columns( )->column( )->text( `Pos` )->get_parent(
                    )->column( )->text( `Material` ).
items_tab->items( )->column_list_item( )->cells(
   )->text( `{POSNR}` )->text( `{MATNR}` ).
```

#### Full Example

A sales-order detail page with three tabs — header form, item table, status overview:

```abap
CLASS z2ui5_cl_sample_icon_tab_bar DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    TYPES:
      BEGIN OF ty_item,
        posnr TYPE posnr,
        matnr TYPE matnr,
        kwmeng TYPE kwmeng,
      END OF ty_item.
    DATA ms_header TYPE vbak.
    DATA mt_items  TYPE STANDARD TABLE OF ty_item WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_sample_icon_tab_bar IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      ms_header = VALUE #( vbeln = `0000004711` auart = `OR` vkorg = `1000` ).
      mt_items  = VALUE #(
        ( posnr = `0010` matnr = `M-01` kwmeng = `10`  )
        ( posnr = `0020` matnr = `M-02` kwmeng = `5`   )
        ( posnr = `0030` matnr = `M-03` kwmeng = `15`  ) ).

      DATA(view) = z2ui5_cl_xml_view=>factory( )->shell( )->page( title = |Order { ms_header-vbeln }| ).
      DATA(bar)  = view->icon_tab_bar( )->items( ).

      " --- Tab 1: header form ---
      DATA(form) = bar->icon_tab_filter(
          text = `General`
          icon = `sap-icon://hint`
          )->content( )->simple_form( editable = abap_false )->content( `form` ).
      form->label( `Order`    )->text( ms_header-vbeln ).
      form->label( `Type`     )->text( ms_header-auart ).
      form->label( `Sales Org`)->text( ms_header-vkorg ).

      " --- Tab 2: item table ---
      DATA(tab_items) = bar->icon_tab_filter(
          text  = `Items`
          count = |{ lines( mt_items ) }|
          icon  = `sap-icon://list`
          )->content( ).
      DATA(items_tab) = tab_items->table( client->_bind( mt_items ) ).
      items_tab->columns( )->column( )->text( `Pos`      )->get_parent(
                          )->column( )->text( `Material` )->get_parent(
                          )->column( )->text( `Qty`      ).
      items_tab->items( )->column_list_item( )->cells(
         )->text( `{POSNR}` )->text( `{MATNR}` )->text( `{KWMENG}` ).

      " --- Tab 3: status ---
      bar->icon_tab_filter(
          text      = `Status`
          icon      = `sap-icon://message-success`
          iconcolor = `Positive`
          )->content(
              )->message_strip( text = `Order is released and ready for delivery.` type = `Success` ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

::: tip
Pre-select a tab with `selectedkey` on `icon_tab_bar` plus a matching `key` on each filter — handy when navigating into the page already expecting a specific tab (e.g. *Status* after an error).
:::
