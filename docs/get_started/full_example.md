---
outline: [2, 4]
---
# Full Example

This tutorial walks through a complete app that follows a typical ABAP flow: a small selection screen, reading data from the database, showing the result in a table, opening a popup to edit a row, and posting the changes back. It ties together everything from the [Hello World](/get_started/hello_world) page and shows how the pieces fit into a real screen.

The example uses sales-order-like data but keeps the SELECTs and updates as plain ABAP so you can adapt them to your own tables. Drop the class into your system and launch it the same way as the Hello World app.

## What You Will Build

1. **Selection screen** — date range and a customer filter.
2. **Read data** — fetch matching orders into an internal table on button press.
3. **Result table** — show the orders, with one row editable via popup.
4. **Popup** — open a dialog that lets the user change the delivery date.
5. **Post** — confirm in the popup, write the change back, refresh the table.

## Tutorial

### Step 1 — State: Types and Attributes

The whole app is one class. Public attributes hold everything the framework needs to remember between roundtrips: the selection criteria, the order table, and the row currently being edited. abap2UI5 serializes and restores them automatically — no session handling on your side.

```abap
CLASS zcl_app_full_example DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_order,
        order_id      TYPE string,
        customer      TYPE string,
        order_date    TYPE string,
        delivery_date TYPE string,
      END OF ty_s_order.

    DATA:
      BEGIN OF s_search,
        date_from TYPE string,
        date_to   TYPE string,
        customer  TYPE string,
      END OF s_search.

    DATA t_orders TYPE STANDARD TABLE OF ty_s_order WITH EMPTY KEY.
    DATA s_edit   TYPE ty_s_order.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_event.
    METHODS view_display.
    METHODS popup_edit_display.
    METHODS data_read.
    METHODS data_update.

  PRIVATE SECTION.
ENDCLASS.
```

The `main` method is a pure dispatcher — the same pattern as in [Hello World](/get_started/hello_world), just with the branches extracted into methods:

```abap
  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_init( ).
      view_display( ).
    ELSEIF client->check_on_event( ).
      on_event( ).
    ENDIF.

  ENDMETHOD.
```

### Step 2 — The View: Selection Screen and Table

`view_display` builds the entire screen: a form with the selection criteria on top and the result table below. It is called exactly once, on the first roundtrip — every later interaction only updates data on this view.

The two date pickers and the customer input use `_bind_edit`, so whatever the user types travels back to the ABAP attributes automatically. The table uses read-only `_bind` since the rows are only displayed:

```abap
  METHOD view_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    DATA(page) = view->shell( )->page(
        title          = `abap2UI5 - Full Example`
        navbuttonpress = client->_event_nav_app_leave( )
        shownavbutton  = client->check_app_prev_stack( ) ).

    page->simple_form(
        title    = `Order Selection`
        editable = abap_true
        )->content( `form`
        )->label( `Order Date From`
        )->date_picker(
            value       = client->_bind_edit( s_search-date_from )
            valueformat = `yyyy-MM-dd`
        )->label( `Order Date To`
        )->date_picker(
            value       = client->_bind_edit( s_search-date_to )
            valueformat = `yyyy-MM-dd`
        )->label( `Customer`
        )->input( client->_bind_edit( s_search-customer )
        )->button(
            text  = `Read Orders`
            press = client->_event( `READ` ) ).

    DATA(tab) = page->table( client->_bind( t_orders ) ).

    tab->columns(
        )->column( )->text( `Order` )->get_parent(
        )->column( )->text( `Customer` )->get_parent(
        )->column( )->text( `Order Date` )->get_parent(
        )->column( )->text( `Delivery Date` )->get_parent(
        )->column( `10%` )->get_parent( ).

    tab->items( )->column_list_item(
        )->cells(
            )->text( `{ORDER_ID}`
            )->text( `{CUSTOMER}`
            )->text( `{ORDER_DATE}`
            )->text( `{DELIVERY_DATE}`
            )->button(
                icon  = `sap-icon://edit`
                press = client->_event( val = `EDIT` t_arg = VALUE #( ( `${ORDER_ID}` ) ) ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
```

Two details worth a second look:

- **Cell bindings** like `{ORDER_ID}` are plain UI5 binding paths relative to the table row — the framework maps the ABAP field names for you.
- **The edit button** sends the row's key along with the event: ``t_arg = VALUE #( ( `${ORDER_ID}` ) )``. The `${...}` syntax is resolved *per row* in the browser, so the backend later knows exactly which order was clicked.

### Step 3 — Reading Data

When the user presses **Read Orders**, the `READ` event arrives in `on_event`. Reading is plain ABAP — in a real system this is a `SELECT` on your own tables; the tutorial fakes it with demo data and applies the selection criteria the same way a `WHERE` clause would:

```abap
  METHOD data_read.

    " demo data — in your system, replace this with a SELECT, e.g.:
    " SELECT order_id, customer, order_date, delivery_date
    "   FROM zsd_order
    "   WHERE order_date BETWEEN @s_search-date_from AND @s_search-date_to
    "   INTO TABLE @t_orders.
    t_orders = VALUE #(
        ( order_id = `1000` customer = `Ace Manufacturing` order_date = `2026-05-04` delivery_date = `2026-07-15` )
        ( order_id = `1001` customer = `Bright Retail`     order_date = `2026-05-12` delivery_date = `2026-07-20` )
        ( order_id = `1002` customer = `Ace Manufacturing` order_date = `2026-06-01` delivery_date = `2026-08-01` )
        ( order_id = `1003` customer = `Corner Logistics`  order_date = `2026-06-18` delivery_date = `2026-08-10` ) ).

    IF s_search-date_from IS NOT INITIAL.
      DELETE t_orders WHERE order_date < s_search-date_from.
    ENDIF.

    IF s_search-date_to IS NOT INITIAL.
      DELETE t_orders WHERE order_date > s_search-date_to.
    ENDIF.

    IF s_search-customer IS NOT INITIAL.
      DELETE t_orders WHERE customer NS s_search-customer.
    ENDIF.

  ENDMETHOD.
```

Back in `on_event`, the crucial line is `view_model_update`. The view already exists in the browser — only the data changed — so instead of rebuilding it, the framework just sends the new model:

```abap
      WHEN `READ`.
        data_read( ).
        client->view_model_update( ).
```

### Step 4 — The Edit Popup

Pressing the edit icon fires the `EDIT` event, and `client->get_event_arg( )` returns the `ORDER_ID` we attached to the button in Step 2. The handler copies the matching row into `s_edit` and opens the dialog:

```abap
      WHEN `EDIT`.
        s_edit = VALUE #( t_orders[ order_id = client->get_event_arg( ) ] OPTIONAL ).
        popup_edit_display( ).
```

A popup is built exactly like the main view — same fluent builder, just created with `factory_popup` and displayed with `popup_display`. The main view stays untouched in the background:

```abap
  METHOD popup_edit_display.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( ).
    DATA(dialog) = popup->dialog( |Order { s_edit-order_id } - { s_edit-customer }| ).

    dialog->simple_form( editable = abap_true
        )->content( `form`
        )->label( `Delivery Date`
        )->date_picker(
            value       = client->_bind_edit( s_edit-delivery_date )
            valueformat = `yyyy-MM-dd` ).

    dialog->buttons(
        )->button(
            text  = `Cancel`
            press = client->_event( `CANCEL` )
        )->button(
            text  = `Post`
            press = client->_event( `POST` )
            type  = `Emphasized` ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.
```

The date picker in the dialog binds to `s_edit-delivery_date` with `_bind_edit` — when the user picks a new date, the attribute is already updated by the time the next event reaches your ABAP code.

### Step 5 — Posting the Change

**Post** writes the change back, closes the popup, and refreshes the table — again without rebuilding the view. **Cancel** just closes the popup:

```abap
      WHEN `POST`.
        data_update( ).
        client->popup_destroy( ).
        client->view_model_update( ).
        client->message_toast_display( |Delivery date of order { s_edit-order_id } updated.| ).
      WHEN `CANCEL`.
        client->popup_destroy( ).
```

Updating is plain ABAP once more — the tutorial changes the internal table, a real app runs an `UPDATE`:

```abap
  METHOD data_update.

    " in your system, persist the change with an UPDATE, e.g.:
    " UPDATE zsd_order SET delivery_date = @s_edit-delivery_date
    "   WHERE order_id = @s_edit-order_id.
    t_orders[ order_id = s_edit-order_id ]-delivery_date = s_edit-delivery_date.

  ENDMETHOD.
```

### The Complete Class

Copy this into your system and launch it like the Hello World app:

::: details Full source code
```abap
CLASS zcl_app_full_example DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_order,
        order_id      TYPE string,
        customer      TYPE string,
        order_date    TYPE string,
        delivery_date TYPE string,
      END OF ty_s_order.

    DATA:
      BEGIN OF s_search,
        date_from TYPE string,
        date_to   TYPE string,
        customer  TYPE string,
      END OF s_search.

    DATA t_orders TYPE STANDARD TABLE OF ty_s_order WITH EMPTY KEY.
    DATA s_edit   TYPE ty_s_order.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_event.
    METHODS view_display.
    METHODS popup_edit_display.
    METHODS data_read.
    METHODS data_update.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_app_full_example IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_init( ).
      view_display( ).
    ELSEIF client->check_on_event( ).
      on_event( ).
    ENDIF.

  ENDMETHOD.


  METHOD on_event.

    CASE client->get( )-event.
      WHEN `READ`.
        data_read( ).
        client->view_model_update( ).
      WHEN `EDIT`.
        s_edit = VALUE #( t_orders[ order_id = client->get_event_arg( ) ] OPTIONAL ).
        popup_edit_display( ).
      WHEN `POST`.
        data_update( ).
        client->popup_destroy( ).
        client->view_model_update( ).
        client->message_toast_display( |Delivery date of order { s_edit-order_id } updated.| ).
      WHEN `CANCEL`.
        client->popup_destroy( ).
    ENDCASE.

  ENDMETHOD.


  METHOD view_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    DATA(page) = view->shell( )->page(
        title          = `abap2UI5 - Full Example`
        navbuttonpress = client->_event_nav_app_leave( )
        shownavbutton  = client->check_app_prev_stack( ) ).

    page->simple_form(
        title    = `Order Selection`
        editable = abap_true
        )->content( `form`
        )->label( `Order Date From`
        )->date_picker(
            value       = client->_bind_edit( s_search-date_from )
            valueformat = `yyyy-MM-dd`
        )->label( `Order Date To`
        )->date_picker(
            value       = client->_bind_edit( s_search-date_to )
            valueformat = `yyyy-MM-dd`
        )->label( `Customer`
        )->input( client->_bind_edit( s_search-customer )
        )->button(
            text  = `Read Orders`
            press = client->_event( `READ` ) ).

    DATA(tab) = page->table( client->_bind( t_orders ) ).

    tab->columns(
        )->column( )->text( `Order` )->get_parent(
        )->column( )->text( `Customer` )->get_parent(
        )->column( )->text( `Order Date` )->get_parent(
        )->column( )->text( `Delivery Date` )->get_parent(
        )->column( `10%` )->get_parent( ).

    tab->items( )->column_list_item(
        )->cells(
            )->text( `{ORDER_ID}`
            )->text( `{CUSTOMER}`
            )->text( `{ORDER_DATE}`
            )->text( `{DELIVERY_DATE}`
            )->button(
                icon  = `sap-icon://edit`
                press = client->_event( val = `EDIT` t_arg = VALUE #( ( `${ORDER_ID}` ) ) ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.


  METHOD popup_edit_display.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( ).
    DATA(dialog) = popup->dialog( |Order { s_edit-order_id } - { s_edit-customer }| ).

    dialog->simple_form( editable = abap_true
        )->content( `form`
        )->label( `Delivery Date`
        )->date_picker(
            value       = client->_bind_edit( s_edit-delivery_date )
            valueformat = `yyyy-MM-dd` ).

    dialog->buttons(
        )->button(
            text  = `Cancel`
            press = client->_event( `CANCEL` )
        )->button(
            text  = `Post`
            press = client->_event( `POST` )
            type  = `Emphasized` ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.


  METHOD data_read.

    " demo data — in your system, replace this with a SELECT, e.g.:
    " SELECT order_id, customer, order_date, delivery_date
    "   FROM zsd_order
    "   WHERE order_date BETWEEN @s_search-date_from AND @s_search-date_to
    "   INTO TABLE @t_orders.
    t_orders = VALUE #(
        ( order_id = `1000` customer = `Ace Manufacturing` order_date = `2026-05-04` delivery_date = `2026-07-15` )
        ( order_id = `1001` customer = `Bright Retail`     order_date = `2026-05-12` delivery_date = `2026-07-20` )
        ( order_id = `1002` customer = `Ace Manufacturing` order_date = `2026-06-01` delivery_date = `2026-08-01` )
        ( order_id = `1003` customer = `Corner Logistics`  order_date = `2026-06-18` delivery_date = `2026-08-10` ) ).

    IF s_search-date_from IS NOT INITIAL.
      DELETE t_orders WHERE order_date < s_search-date_from.
    ENDIF.

    IF s_search-date_to IS NOT INITIAL.
      DELETE t_orders WHERE order_date > s_search-date_to.
    ENDIF.

    IF s_search-customer IS NOT INITIAL.
      DELETE t_orders WHERE customer NS s_search-customer.
    ENDIF.

  ENDMETHOD.


  METHOD data_update.

    " in your system, persist the change with an UPDATE, e.g.:
    " UPDATE zsd_order SET delivery_date = @s_edit-delivery_date
    "   WHERE order_id = @s_edit-order_id.
    t_orders[ order_id = s_edit-order_id ]-delivery_date = s_edit-delivery_date.

  ENDMETHOD.

ENDCLASS.
```
:::

## What to Take Away

- One controller class, one `main` method, all state in public attributes — that is the whole app
- The view is rebuilt only when the structure changes. Edits, saves, and popup open/close do not need a fresh `view_display( )`
- Popups are just a different factory (`factory_popup`) on the same `z2ui5_cl_xml_view`, displayed via `popup_display` / `popup_destroy` while the main view stays in place
- Reading and writing the database is plain ABAP — abap2UI5 does not abstract that layer, which is what makes it easy to plug into existing code

From here, look at the [Cookbook](/cookbook/event_navigation/life_cycle) for value helps, navigation between apps, message handling, and other patterns you will need next.
