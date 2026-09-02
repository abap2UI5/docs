---
outline: [2, 4]
description: Unit-test the app class like any ABAP class — the data methods are plain ABAP, the test class never touches the UI, and the structure from Step 10 is what makes that possible.
---
# Step 12: Unit Tests

The app is in production since [Step 11](/tutorials/walkthrough/step-11), and
changes will keep coming — a new filter, a second editable field, the next
framework release. What lets the next transport leave with confidence is the
same thing as in every other ABAP project: unit tests. This step adds them to
the walkthrough app, and the point of it is how little abap2UI5 gets in the
way — the methods worth testing are plain ABAP, so the test class is one you
could have written before ever hearing of this framework.

## The Class Under Test

The app is unchanged from [Step 10](/tutorials/walkthrough/step-10) — printed
here in full so this step stands on its own:

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

## The Test Class

Tests live where they always live: in the class's **Test Classes** include
(the *Test Classes* tab in ADT). Nothing abap2UI5-specific is in them:

```abap
CLASS ltcl_walkthrough DEFINITION DEFERRED.
CLASS zcl_app_walkthrough DEFINITION LOCAL FRIENDS ltcl_walkthrough.

CLASS ltcl_walkthrough DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS read_filters_by_supplier FOR TESTING.
    METHODS read_filters_by_date     FOR TESTING.
    METHODS update_writes_back       FOR TESTING.

ENDCLASS.

CLASS ltcl_walkthrough IMPLEMENTATION.

  METHOD read_filters_by_supplier.

    DATA(cut) = NEW zcl_app_walkthrough( ).
    cut->s_search-supplier = `Green Growers`.

    cut->data_read( ).

    cl_abap_unit_assert=>assert_equals(
        act = lines( cut->t_invoices )
        exp = 2
        msg = `expected exactly the two Green Growers invoices` ).

  ENDMETHOD.


  METHOD read_filters_by_date.

    DATA(cut) = NEW zcl_app_walkthrough( ).
    cut->s_search-date_from = `2026-08-01`.

    cut->data_read( ).

    LOOP AT cut->t_invoices INTO DATA(ls_invoice).
      cl_abap_unit_assert=>assert_true(
          act = xsdbool( ls_invoice-delivery_date >= `2026-08-01` )
          msg = |{ ls_invoice-product } lies before the date filter| ).
    ENDLOOP.

  ENDMETHOD.


  METHOD update_writes_back.

    DATA(cut) = NEW zcl_app_walkthrough( ).
    cut->data_read( ).

    cut->s_edit = VALUE #( product = `Milk` delivery_date = `2026-09-01` ).
    cut->data_update( ).

    cl_abap_unit_assert=>assert_equals(
        act = cut->t_invoices[ product = `Milk` ]-delivery_date
        exp = `2026-09-01` ).

  ENDMETHOD.

ENDCLASS.
```

Run them as always — in ADT with `Ctrl+Shift+F10`. Three tests, three
different seams:

- **`read_filters_by_supplier`** fills the selection screen's backing
  structure `s_search` — a plain public attribute — calls `data_read` and
  counts the result. No button was pressed and no view was built: the test
  enters through the same attribute the UI binds.
- **`read_filters_by_date`** does the same for the date filter and checks a
  property of every row rather than a count.
- **`update_writes_back`** plays the popup workflow without the popup: fill
  `s_edit` the way the dialog's bindings would, call `data_update`, and check
  the table. When the demo data becomes a real `UPDATE` in
  [Step 11](/tutorials/walkthrough/step-11), this is the test that grows a
  test double for the database layer — the seam is already in place.

## Why This Worked

- **The tests never mock the framework.** `main` is a dispatcher and the
  handler methods do not take `client` as a parameter — so the logic under
  test is reachable without a single framework object. That is the payoff of
  Step 10's structure.
- **`LOCAL FRIENDS` opens the protected section.** `data_read` and
  `data_update` are protected — implementation, not model. The two lines
  above the test class (`DEFERRED`, then `LOCAL FRIENDS`) let the test class
  call them anyway, without making them public for everyone. Do not skip
  them: without `LOCAL FRIENDS` the class fails to activate on a real system
  even where a linter stays quiet.
- **Public attributes are the natural test interface.** The same attributes
  the framework serializes and the view binds — `s_search`, `t_invoices`,
  `s_edit` — are what tests fill and assert on. The UI enters the class the
  same way the test does.

What the unit tests deliberately do not cover is the view: whether `Table`
has an `items` aggregation is not a question ABAP can answer. That check
exists too, without a system — the [abap2UI5 linter](/advanced/linter)
reconstructs the view from the builder chain and validates it against UI5,
and the [tooling page](/get_started/tooling) shows how it runs in CI next to
these tests.

## What to Take Away

- An abap2UI5 app is testable like any ABAP class, because it *is* one —
  no UI5 runtime, no HTTP, no mocks
- Test through the same public attributes the view binds; call the data
  methods directly
- `DEFERRED` + `LOCAL FRIENDS` is the pattern for testing protected methods —
  and a missing `LOCAL FRIENDS` is an activation error, not a style issue
- Views are checked by the linter, logic by unit tests; together they run
  without an SAP system

That is the end of the walkthrough: one class, grown from a message box to a
tested app in production. The [Cookbook](/cookbook/view/definition) covers
every topic again as a reference chapter, and the
[sample catalogues](https://abap2ui5.github.io/samples/) continue from here.
