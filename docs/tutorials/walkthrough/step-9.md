---
outline: [2, 4]
description: Swap the list for a real UI5 table — columns, cells and a row action that opens the editor.
samples:
  - z2ui5_cl_smp_app_070
---
# Step 9: Tables

A list shows three fields per row. A table shows columns — with headers, with
a cell per field, and with room for a row action. This step swaps
`sap.m.List` for `sap.m.Table` and puts the edit button from
[Step 7](/tutorials/walkthrough/step-7) back, now in a column of its own:

```abap
CLASS zcl_app_walkthrough DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_invoice,
        product       TYPE string,
        supplier      TYPE string,
        quantity      TYPE string,
        delivery_date TYPE string,
      END OF ty_s_invoice.

    DATA t_invoices TYPE STANDARD TABLE OF ty_s_invoice WITH EMPTY KEY.
    DATA s_edit     TYPE ty_s_invoice.
ENDCLASS.

CLASS zcl_app_walkthrough IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      t_invoices = VALUE #(
          ( product = `Pineapple`    supplier = `ACME`          quantity = `21` delivery_date = `2026-07-15` )
          ( product = `Milk`         supplier = `Green Growers` quantity = `4`  delivery_date = `2026-07-20` )
          ( product = `Canned Beans` supplier = `Corner Deli`   quantity = `3`  delivery_date = `2026-08-01` )
          ( product = `Salad`        supplier = `Green Growers` quantity = `2`  delivery_date = `2026-08-10` )
          ( product = `Bread`        supplier = `Corner Deli`   quantity = `1`  delivery_date = `2026-08-12` ) ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc` ).

      DATA(tab) = view->ele( `Shell`
          )->ele( `Page`
              )->a( n = `title` v = `Walkthrough - Step 9`

              )->ele( `Table`
                  )->a( n = `headerText` v = `Invoices`
                  )->a( n = `items`      v = client->_bind( t_invoices ) ).

      tab->ele( `columns`

          )->ele( `Column`
              )->tag( `Text`
                  )->a( n = `text` v = `Product`

          )->end(
          )->ele( `Column`
              )->tag( `Text`
                  )->a( n = `text` v = `Supplier`

          )->end(
          )->ele( `Column`
              )->tag( `Text`
                  )->a( n = `text` v = `Quantity`

          )->end(
          )->ele( `Column`
              )->tag( `Text`
                  )->a( n = `text` v = `Delivery Date`

          )->end(
          )->ele( `Column`
              )->a( n = `width` v = `10%` ).

      tab->ele( `items`
          )->ele( `ColumnListItem`
              )->ele( `cells`

                  )->tag( `Text`
                      )->a( n = `text` v = `{PRODUCT}`
                  )->tag( `Text`
                      )->a( n = `text` v = `{SUPPLIER}`
                  )->tag( `Text`
                      )->a( n = `text` v = `{QUANTITY}`
                  )->tag( `Text`
                      )->a( n = `text` v = `{DELIVERY_DATE}`
                  )->tag( `Button`
                      )->a( n = `icon`    v = `sap-icon://edit`
                      )->a( n = `tooltip` v = `Edit delivery date`
                      )->a( n = `press`   v = client->_event( val   = `EDIT`
                                                              t_arg = VALUE #( ( `${PRODUCT}` ) ) ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `EDIT` ).

      s_edit = VALUE #( t_invoices[ product = client->get_event_arg( ) ] OPTIONAL ).

      DATA(popup) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `FragmentDefinition` ns = `core`
              )->a( n = `xmlns`      v = `sap.m`
              )->a( n = `xmlns:core` v = `sap.ui.core`

              )->ele( `Dialog`
                  )->a( n = `title` v = |Edit { s_edit-product }|

                  )->ele( `content`

                      )->tag( `Label`
                          )->a( n = `text` v = `Delivery Date`
                      )->tag( `DatePicker`
                          )->a( n = `value`       v = client->_bind( s_edit-delivery_date )
                          )->a( n = `valueFormat` v = `yyyy-MM-dd`

                  )->end(

                  )->ele( `buttons`

                      )->tag( `Button`
                          )->a( n = `text`  v = `Cancel`
                          )->a( n = `press` v = client->_event( `CANCEL` )
                      )->tag( `Button`
                          )->a( n = `text`  v = `Save`
                          )->a( n = `press` v = client->_event( `SAVE` )
                          )->a( n = `type`  v = `Emphasized` ).

      client->popup_display( popup->stringify( ) ).

    ELSEIF client->check_on_event( `SAVE` ).

      t_invoices[ product = s_edit-product ]-delivery_date = s_edit-delivery_date.
      client->popup_destroy( ).
      client->message_toast_display( |{ s_edit-product } updated.| ).

    ELSEIF client->check_on_event( `CANCEL` ).

      client->popup_destroy( ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

## Columns and Cells

- **A table is two aggregations.** `columns` holds one `Column` per column,
  each with its header control; `items` holds the row template, whose `cells`
  hold one control per column. The two have to line up — the third cell lands
  under the third column, whatever it contains.
- **`end( )` ascends.** After a `Column` has been given its header `Text`, the
  chain is standing inside that column. `end( )` — alone, in the column of the
  `ele( )` it closes — steps back out so the next `Column` becomes a sibling
  rather than a child.
- **`tab` holds the table.** The chain is split again: the `Table` goes into a
  variable, and `columns` and `items` are filled from it in two statements. A
  single chain would work too, but it would ascend six levels between the last
  column and the first cell.
- **The fifth column has no header text and a fixed `10%` width.** It only
  carries the edit button defined in the row cells — this is how a row action
  gets a column of its own.
- **`sap-icon://edit`** is one of the several hundred icons the UI5 icon font
  ships. The tooltip is what a reader of the screen — and a screen reader —
  gets instead of a label.

Everything else is [Step 7](/tutorials/walkthrough/step-7) unchanged: the row
event carries `${PRODUCT}`, the dialog binds `s_edit`, and `SAVE` writes back
into the internal table without rebuilding the view.

One step remains: putting all of it into the shape a real app has.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Full Example with sap.ui.table | [`Z2UI5_CL_SMP_APP_070`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_070.clas.abap) |

<!-- samples:end -->
