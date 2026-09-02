---
outline: [2, 4]
description: Put everything into the structure real apps use — a dispatcher, named methods, protected state, and the SELECT and UPDATE in one place.
samples:
  - z2ui5_cl_smp_app_070
  - z2ui5_cl_smp_app_011
---
# Step 10: App Structure

Everything so far lived in one `main` method, and by now that method does five
different jobs. Real apps — the framework's own, and the sample catalogues' —
separate the phases into methods. This step changes no behavior at all: it
puts the code where a reader expects it, and assembles every part of the
tutorial into the complete app.

## What It Does

1. **Selection screen** — supplier and a delivery-date range ([Step 8](/tutorials/walkthrough/step-8)).
2. **Read** — fetch the matching invoices on button press ([Step 8](/tutorials/walkthrough/step-8)).
3. **Result table** — columns, cells and a row action ([Step 9](/tutorials/walkthrough/step-9)).
4. **Popup** — edit the delivery date of one row ([Step 7](/tutorials/walkthrough/step-7)).
5. **Post** — write the change back and refresh the table.

## The Class

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
    DATA s_edit     TYPE ty_s_invoice.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_event.
    METHODS view_display.
    METHODS popup_edit_display.
    METHODS data_read.
    METHODS data_update.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_app_walkthrough IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_navigated( ).
      view_display( ).
    ELSEIF client->check_on_event( ).
      on_event( ).
    ENDIF.

  ENDMETHOD.


  METHOD on_event.

    " get_event( ) holds the name passed to _event( );
    " get_event_arg( ) returns the extra argument attached via t_arg
    CASE client->get_event( ).
      WHEN `READ`.
        data_read( ).
      WHEN `EDIT`.
        s_edit = VALUE #( t_invoices[ product = client->get_event_arg( ) ] OPTIONAL ).
        popup_edit_display( ).
      WHEN `SAVE`.
        data_update( ).
        client->popup_destroy( ).
        client->message_toast_display( |{ s_edit-product } updated.| ).
      WHEN `CANCEL`.
        client->popup_destroy( ).
    ENDCASE.

  ENDMETHOD.


  METHOD view_display.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`      v = `sap.m`
            )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
            )->a( n = `xmlns:form` v = `sap.ui.layout.form` ).

    DATA(page) = view->ele( `Shell`
        )->ele( `Page`
            )->a( n = `title`          v = `Walkthrough - Step 10`
            )->a( n = `navButtonPress` v = client->_event_nav_app_leave( )
            )->a( n = `showNavButton`  b = client->check_app_prev_stack( ) ).

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

    DATA(tab) = page->ele( `Table`
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

  ENDMETHOD.


  METHOD popup_edit_display.

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

  ENDMETHOD.


  METHOD data_read.

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

  ENDMETHOD.


  METHOD data_update.

    " in your system, persist the change with an UPDATE, e.g.:
    " UPDATE zinvoice SET delivery_date = @s_edit-delivery_date
    "   WHERE product = @s_edit-product.
    t_invoices[ product = s_edit-product ]-delivery_date = s_edit-delivery_date.

  ENDMETHOD.

ENDCLASS.
```

## The Structure

- **`main` is a pure dispatcher.** It stashes `client` in a protected attribute
  — so the handler methods can use it without passing it around — and routes
  each roundtrip to the method for its phase. `check_on_event( )` without an
  argument is true for *any* event; the `CASE` in `on_event` decides which one.
  Both dispatch shapes are fine, and
  [Life Cycle](/cookbook/event_navigation/life_cycle) compares them: dispatch
  by event name in the `IF` chain for a small app, this second level once there
  are handler methods.
- **State stays public, everything else protected.** Public attributes are the
  serialized, browser-visible model — bound data and nothing more. The `client`
  reference and the methods are implementation.
- **One method per screen, one per data operation.** `view_display` and
  `popup_edit_display` build; `data_read` and `data_update` touch the database.
  When an app grows, those are the seams it grows along — and the two data
  methods are the only places a `SELECT` or an `UPDATE` ever appears.
- **A back button, when there is somewhere to go.** `navButtonPress` and
  `showNavButton` give the page the standard back navigation, shown only when
  this app was called from another one — see
  [Navigation](/cookbook/event_navigation/navigation/inner_app).

## What to Take Away

- One controller class, one `main` method, all state in public attributes — that is the whole app
- The view is rebuilt only when the structure changes. Reading data, saving, and opening or closing a popup do not need a fresh `view_display( )`
- Popups use the same builder as the view — a `core:FragmentDefinition` root instead of `mvc:View` — shown with `popup_display` / `popup_destroy` while the main view stays in place
- Reading and writing the database is plain ABAP; abap2UI5 does not abstract that layer, which is what makes it easy to plug into existing code

## Where to Go From Here

The app is built — the walkthrough's last two steps take it out of the
playground:

- **[Step 11: From Playground to Production](/tutorials/walkthrough/step-11)** —
  real data, the transport order, authorization, and the URL users start from,
- **[Step 12: Unit Tests](/tutorials/walkthrough/step-12)** — the structure of
  this step pays off: the data methods are testable without any UI.

And for everything beyond the walkthrough:

- the [Cookbook](/cookbook/view/definition) — every topic of this walkthrough as a
  reference chapter, from [value helps](/cookbook/expert_more/value_help) to
  [navigation between apps](/cookbook/event_navigation/navigation/inner_app),
- the [sample catalogues](https://abap2ui5.github.io/samples/) — complete,
  tested apps for nearly every pattern, each one class like here.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Full Example with sap.ui.table | [`Z2UI5_CL_SMP_APP_070`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_070.clas.abap) |
| Editable Cells, Add and Delete Rows | [`Z2UI5_CL_SMP_APP_011`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_011.clas.abap) |

<!-- samples:end -->
