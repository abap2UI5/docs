---
outline: [2, 4]
description: Refactor the walkthrough app into the structure real apps use — a dispatcher, named methods, protected state.
---
# Step 8: App Structure

The app is complete, but everything lives in one `main` method. Real apps —
the framework's own, and the sample catalogues' — separate the phases into
methods, so this last step changes no behavior at all: it puts the code where
a reader expects it.

```abap
CLASS zcl_app_walkthrough DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_invoice,
        product  TYPE string,
        supplier TYPE string,
        quantity TYPE string,
      END OF ty_s_invoice.

    DATA t_invoices TYPE STANDARD TABLE OF ty_s_invoice WITH EMPTY KEY.
    DATA s_edit     TYPE ty_s_invoice.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_event.
    METHODS view_display.
    METHODS popup_edit_display.

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

    CASE client->get( )-event.
      WHEN `EDIT`.
        s_edit = VALUE #( t_invoices[ product = client->get_event_arg( ) ] OPTIONAL ).
        popup_edit_display( ).
      WHEN `SAVE`.
        t_invoices[ product = s_edit-product ]-quantity = s_edit-quantity.
        client->popup_destroy( ).
        client->message_toast_display( |{ s_edit-product } updated.| ).
      WHEN `CANCEL`.
        client->popup_destroy( ).
    ENDCASE.

  ENDMETHOD.


  METHOD view_display.

    t_invoices = VALUE #(
        ( product = `Pineapple`    supplier = `ACME`          quantity = `21` )
        ( product = `Milk`         supplier = `Green Growers` quantity = `4`  )
        ( product = `Canned Beans` supplier = `Corner Deli`   quantity = `3`  )
        ( product = `Salad`        supplier = `Green Growers` quantity = `2`  )
        ( product = `Bread`        supplier = `Corner Deli`   quantity = `1`  ) ).

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Shell`
                )->ele( `Page`
                    )->a( n = `title` v = `Walkthrough - Step 8`

                    )->ele( `List`
                        )->a( n = `headerText` v = `Invoices`
                        )->a( n = `items`      v = client->_bind( t_invoices )

                        )->ele( `items`
                            )->tag( `StandardListItem`
                                )->a( n = `title`       v = `{PRODUCT}`
                                )->a( n = `description` v = `{SUPPLIER}`
                                )->a( n = `info`        v = `{QUANTITY}`
                                )->a( n = `type`        v = `Active`
                                )->a( n = `press`       v = client->_event( val   = `EDIT`
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
                        )->a( n = `text` v = `Quantity`
                    )->tag( `Input`
                        )->a( n = `value` v = client->_bind( s_edit-quantity )

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

ENDCLASS.
```

## The Structure

- **`main` is a pure dispatcher.** It stashes `client` in a protected
  attribute — so the handler methods can use it without passing it around —
  and routes each roundtrip to the method for its phase. `check_on_event( )`
  without an argument is true for *any* event; the `CASE` in `on_event`
  decides which one.
- **State stays public, everything else protected.** Public attributes are
  the serialized, browser-visible model — bound data and nothing more. The
  `client` reference and the methods are implementation.
- **One method per screen.** `view_display` for the page, `popup_edit_display`
  for the dialog. When an app grows, this is the seam it grows along.

## Where to Go From Here

You have seen everything an abap2UI5 app is made of. Three places continue
from here:

- the [Full Example](/get_started/full_example) — the same structure applied
  to a realistic selection-screen app with a table and database access,
- the [Cookbook](/cookbook/overview) — every topic of this walkthrough as a
  reference chapter, from [value helps](/cookbook/expert_more/value_help) to
  [navigation between apps](/cookbook/event_navigation/navigation),
- the [sample catalogues](https://abap2ui5.github.io/samples/) — complete,
  tested apps for nearly every pattern, each one class like here.
