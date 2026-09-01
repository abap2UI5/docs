---
outline: [2, 4]
description: A selection form above the list, and reading the data it asks for — the shape of a classic ABAP report.
samples:
  - z2ui5_cl_smp_app_011
---
# Step 8: Selection Screen

Real apps rarely show all the data at once. They ask first: a selection screen
on top, a result below, and a button between them. This step adds that form —
and with it the way a real app reads its data.

The invoice record gains a delivery date, so there is a date to select on:

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

    DATA:
      BEGIN OF s_search,
        supplier  TYPE string,
        date_from TYPE string,
        date_to   TYPE string,
      END OF s_search.

    DATA t_invoices TYPE STANDARD TABLE OF ty_s_invoice WITH EMPTY KEY.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_app_walkthrough IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`      v = `sap.m`
              )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
              )->a( n = `xmlns:form` v = `sap.ui.layout.form` ).

      DATA(page) = view->ele( `Shell`
          )->ele( `Page`
              )->a( n = `title` v = `Walkthrough - Step 8` ).

      page->ele( n = `SimpleForm` ns = `form`
          )->a( n = `title`    v = `Selection`
          )->a( n = `editable` v = `true`

          )->ele( n = `content` ns = `form`

              )->tag( `Label`
                  )->a( n = `text` v = `Supplier`
              )->tag( `Input`
                  )->a( n = `value` v = client->_bind( s_search-supplier )
              )->tag( `Label`
                  )->a( n = `text` v = `Delivery Date From`
              )->tag( `DatePicker`
                  )->a( n = `value`       v = client->_bind( s_search-date_from )
                  )->a( n = `valueFormat` v = `yyyy-MM-dd`
              )->tag( `Label`
                  )->a( n = `text` v = `Delivery Date To`
              )->tag( `DatePicker`
                  )->a( n = `value`       v = client->_bind( s_search-date_to )
                  )->a( n = `valueFormat` v = `yyyy-MM-dd`
              )->tag( `Button`
                  )->a( n = `text`  v = `Read Invoices`
                  )->a( n = `press` v = client->_event( `READ` )
                  )->a( n = `type`  v = `Emphasized` ).

      page->ele( `List`
          )->a( n = `headerText` v = `Invoices`
          )->a( n = `items`      v = client->_bind( t_invoices )

          )->ele( `items`
              )->tag( `StandardListItem`
                  )->a( n = `title`       v = `{PRODUCT}`
                  )->a( n = `description` v = `{SUPPLIER}`
                  )->a( n = `info`        v = `{DELIVERY_DATE}` ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `READ` ).

      " demo data — in your system, replace this with a SELECT, e.g.:
      " SELECT product, supplier, quantity, delivery_date
      "   FROM zinvoice
      "   WHERE supplier      LIKE @s_search-supplier
      "     AND delivery_date BETWEEN @s_search-date_from AND @s_search-date_to
      "   INTO TABLE @t_invoices.
      t_invoices = VALUE #(
          ( product = `Pineapple`    supplier = `ACME`          quantity = `21` delivery_date = `2026-07-15` )
          ( product = `Milk`         supplier = `Green Growers` quantity = `4`  delivery_date = `2026-07-20` )
          ( product = `Canned Beans` supplier = `Corner Deli`   quantity = `3`  delivery_date = `2026-08-01` )
          ( product = `Salad`        supplier = `Green Growers` quantity = `2`  delivery_date = `2026-08-10` )
          ( product = `Bread`        supplier = `Corner Deli`   quantity = `1`  delivery_date = `2026-08-12` ) ).

      IF s_search-supplier IS NOT INITIAL.
        " NS = `contains no string` — drop the rows whose supplier does not match
        DELETE t_invoices WHERE supplier NS s_search-supplier.
      ENDIF.

      IF s_search-date_from IS NOT INITIAL.
        DELETE t_invoices WHERE delivery_date < s_search-date_from.
      ENDIF.

      IF s_search-date_to IS NOT INITIAL.
        DELETE t_invoices WHERE delivery_date > s_search-date_to.
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

## What Is New

- **A form, from a second statement.** The chain is split: `view` holds the
  root, `page` holds the `Page`, and the form and the list are each filled from
  `page` in a statement of their own. That is the shape most abap2UI5 apps use
  once a view has more than one part — see
  [View → Definition](/cookbook/view/definition).
- **`sap.ui.layout.form` needs its own namespace.** `xmlns:form` is declared on
  the root next to `xmlns:mvc`, and `SimpleForm` and its `content` aggregation
  are written with `ns = \`form\``. A `SimpleForm` lays out label/field pairs by
  itself — no grid, no widths.
- **`DatePicker` with `valueFormat`.** The picker shows the date in the user's
  locale and hands your ABAP attribute the format you asked for — here
  `yyyy-MM-dd`, so a string comparison sorts correctly.
- **The event handler reads data, and nothing else.** No `view_display( )` in
  the `READ` branch: the view already exists in the browser, only `t_invoices`
  changed, and every roundtrip that changed bound data pushes the new model by
  itself.
- **The `SELECT` is the point.** In your system the demo data and the three
  `DELETE`s are one `SELECT` with a `WHERE` clause. abap2UI5 does not abstract
  the database layer — reading and writing stays plain ABAP, which is what
  makes it easy to plug into code you already have.

Next, the list becomes a real table.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Editable Cells, Add and Delete Rows | [`Z2UI5_CL_SMP_APP_011`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_011.clas.abap) |

<!-- samples:end -->
