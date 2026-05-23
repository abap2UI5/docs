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

## The Class Definition

The controller holds all state in **public** attributes so binding works (see [Lifecycle Pitfalls](/cookbook/event_navigation/life_cycle#lifecycle-pitfalls)). The handler methods are protected — they are called from `main` and never bound.

```abap
CLASS z2ui5_cl_demo_full_example DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_row,
        order_id      TYPE c LENGTH 10,
        customer      TYPE c LENGTH 10,
        material      TYPE c LENGTH 18,
        quantity      TYPE i,
        delivery_date TYPE d,
      END OF ty_row.

    " Selection screen
    DATA date_from TYPE d.
    DATA date_to   TYPE d.
    DATA customer  TYPE c LENGTH 10.

    " Result set
    DATA orders    TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

    " Edit buffer (used by the popup)
    DATA edit_row   TYPE ty_row.
    DATA edit_index TYPE i.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS render_main.
    METHODS open_edit_popup.
    METHODS read_data.
    METHODS save_changes.

ENDCLASS.
```

## The `main` Dispatcher

`main` is the only entry point. It uses `CASE abap_true` to dispatch between init, events, and navigation returns — the standard [life-cycle pattern](/cookbook/event_navigation/life_cycle):

```abap
CLASS z2ui5_cl_demo_full_example IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    CASE abap_true.

      WHEN client->check_on_init( ).
        " preset the selection screen with a useful default
        date_from = sy-datum - 30.
        date_to   = sy-datum.
        render_main( ).

      WHEN client->check_on_event( `SEARCH` ).
        read_data( ).
        render_main( ).

      WHEN client->check_on_event( `EDIT` ).
        open_edit_popup( ).

      WHEN client->check_on_event( `SAVE` ).
        save_changes( ).
        client->popup_destroy( ).

      WHEN client->check_on_event( `CANCEL` ).
        client->popup_destroy( ).

    ENDCASE.

  ENDMETHOD.
```

A few things worth noting:

- The view is rebuilt and re-sent in `render_main( )` on the events that change the visible structure (initial render, after the search). On `EDIT`, `SAVE`, and `CANCEL` we only open or close a popup — the underlying view stays.
- Public attributes (`date_from`, `customer`, `orders`, …) carry the state across roundtrips. The framework serializes them between requests automatically.
- Two-way-bound inputs (`_bind_edit`) write user changes back into the public attributes *before* the event handler runs.

## The Selection Screen and the Table

`render_main` paints the selection panel and, if `orders` already has rows, the result table below it. The two parts live in the same `Page`:

```abap
  METHOD render_main.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    DATA(page) = view->shell( )->page( title = `Sales Orders` ).

    " --- Selection screen -------------------------------------------------
    DATA(form) = page->simple_form( title = `Selection` editable = abap_true
        )->content( `f` ).
    form->label( `Date from`     )->date_picker( client->_bind_edit( date_from ) ).
    form->label( `Date to`       )->date_picker( client->_bind_edit( date_to   ) ).
    form->label( `Customer`      )->input(       client->_bind_edit( customer  ) ).
    form->button( text  = `Search`
                  type  = `Emphasized`
                  press = client->_event( `SEARCH` ) ).

    " --- Result table -----------------------------------------------------
    IF orders IS NOT INITIAL.
      DATA(tab) = page->table(
          items = client->_bind( orders )
          headerText = `Results` ).
      tab->columns(
          )->column( )->text( `Order`         )->get_parent(
          )->column( )->text( `Customer`      )->get_parent(
          )->column( )->text( `Material`      )->get_parent(
          )->column( )->text( `Quantity`      )->get_parent(
          )->column( )->text( `Delivery Date` )->get_parent(
          )->column( )->text( `` ).
      DATA(cells) = tab->items( )->column_list_item( )->cells( ).
      cells->text( `{ORDER_ID}` ).
      cells->text( `{CUSTOMER}` ).
      cells->text( `{MATERIAL}` ).
      cells->text( `{QUANTITY}` ).
      cells->text( `{DELIVERY_DATE}` ).
      cells->button( text  = `Edit`
                     press = client->_event(
                       val   = `EDIT`
                       t_arg = VALUE #( ( `${ORDER_ID}` ) ) ) ).
    ENDIF.

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
```

The `Edit` button passes the bound row's `ORDER_ID` as an event argument. On the next roundtrip, the handler reads it via `client->get_event_arg( 1 )` to know which row was clicked.

## Reading Data

`read_data` is plain ABAP — replace the dummy fill with a real `SELECT` against your tables. The framework does not get in the way here:

```abap
  METHOD read_data.

    CLEAR orders.

    " Replace with your own SELECT, e.g.:
    "
    " SELECT vbeln AS order_id, kunnr AS customer,
    "        matnr AS material, kwmeng AS quantity, edatu AS delivery_date
    "   FROM vbap
    "   INNER JOIN vbak ON vbap~vbeln = vbak~vbeln
    "   WHERE vbak~erdat BETWEEN @date_from AND @date_to
    "     AND ( @customer IS INITIAL OR vbak~kunnr = @customer )
    "   INTO TABLE @orders.

    " Dummy data so the tutorial runs without a database table:
    orders = VALUE #(
      ( order_id = `0000010001` customer = `1000` material = `M-01`
        quantity = 5  delivery_date = sy-datum + 7 )
      ( order_id = `0000010002` customer = `1000` material = `M-02`
        quantity = 12 delivery_date = sy-datum + 14 )
      ( order_id = `0000010003` customer = `1010` material = `M-03`
        quantity = 1  delivery_date = sy-datum + 3 ) ).

  ENDMETHOD.
```

## The Edit Popup

When the user presses `Edit`, the handler picks the row out of `orders`, stores it in `edit_row`, and renders a popup with two-way-bound fields. Because `edit_row` is a public attribute, anything the user types ends up there before `SAVE` runs:

```abap
  METHOD open_edit_popup.

    DATA(order_id) = client->get_event_arg( 1 ).
    READ TABLE orders WITH KEY order_id = order_id
         ASSIGNING FIELD-SYMBOL(<row>).
    IF sy-subrc <> 0.
      client->message_box_display(
        text = |Order { order_id } not found| type = `error` ).
      RETURN.
    ENDIF.

    edit_row   = <row>.
    edit_index = sy-tabix.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( ).
    DATA(dlg)   = popup->dialog(
        title          = |Edit Order { edit_row-order_id }|
        contentWidth   = `400px` ).

    DATA(form) = dlg->simple_form( editable = abap_true )->content( `f` ).
    form->label( `Customer`      )->text(        client->_bind( edit_row-customer ) ).
    form->label( `Material`      )->text(        client->_bind( edit_row-material ) ).
    form->label( `Quantity`      )->input(       client->_bind_edit( edit_row-quantity ) ).
    form->label( `Delivery Date` )->date_picker( client->_bind_edit( edit_row-delivery_date ) ).

    dlg->buttons(
        )->button( text  = `Save`
                   type  = `Emphasized`
                   press = client->_event( `SAVE` )
        )->button( text  = `Cancel`
                   press = client->_event( `CANCEL` ) ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.
```

Note that **the same** `client->view_display` from `render_main` is still active behind the popup. UI5 dims the page and overlays the dialog — there is no second view to manage.

## Posting the Changes

`save_changes` writes `edit_row` back into the `orders` table at the position captured when the popup opened, then runs whatever persistence you need. In a real app this is the place to call `UPDATE vbap …` or trigger a BAPI:

```abap
  METHOD save_changes.

    READ TABLE orders INDEX edit_index ASSIGNING FIELD-SYMBOL(<row>).
    IF sy-subrc <> 0.
      client->message_box_display(
        text = `Row no longer exists — please search again` type = `error` ).
      RETURN.
    ENDIF.

    <row> = edit_row.

    " Persistence — replace with your real update / BAPI / EML call:
    "
    " UPDATE vbap
    "   SET kwmeng = @edit_row-quantity, edatu = @edit_row-delivery_date
    "   WHERE vbeln = @edit_row-order_id.
    " COMMIT WORK.

    client->message_toast_display(
      |Order { edit_row-order_id } updated| ).

  ENDMETHOD.

ENDCLASS.
```

When `save_changes` returns, `main` calls `client->popup_destroy( )` to close the dialog. The table on the main view updates automatically because UI5 keeps reading from the same model path — the framework re-serializes `orders` on every response, so the edited row appears with its new values without a second `view_display( )` call.

## What to Take Away

- One controller class, one `main` method, all state in public attributes — that is the whole app.
- The view tree is rebuilt only when the structure changes. Edits, saves, and popup open/close do not need a fresh `view_display( )`.
- Popups are just a different factory (`factory_popup`) on the same `z2ui5_cl_xml_view`, displayed via `popup_display` / `popup_destroy` while the main view stays in place.
- Reading and writing the database is plain ABAP — abap2UI5 does not abstract that layer, which is what makes it easy to plug into existing code.

From here, look at the [Cookbook](/cookbook/event_navigation/life_cycle) for value helps, navigation between apps, message handling, and other patterns you will need next.
