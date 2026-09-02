# #24 ABAP Unit for a Screen

An abap2UI5 app is a global class, and a global class has a *Test Classes*
include. That is the whole story of testing here, and the reason it stays short
is a decision in the app rather than in the framework: **the logic does not
touch `client`.**

`main( )` dispatches. The methods it dispatches to read data, decide, and
change attributes. Only `view_display( )` and the message calls need the
client, so a test calls the other methods directly and looks at the attributes
afterwards:

```abap
CLASS zcl_app_overdue DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_invoice,
        invoice TYPE string,
        due     TYPE d,
        open    TYPE abap_bool,
      END OF ty_s_invoice.
    TYPES ty_t_invoice TYPE STANDARD TABLE OF ty_s_invoice WITH EMPTY KEY.

    DATA t_invoices TYPE ty_t_invoice.
    DATA t_overdue  TYPE ty_t_invoice.
    DATA key_date   TYPE d.

    METHODS data_read.
    METHODS overdue_calc.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.
    METHODS view_display.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_app_overdue IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    CASE abap_true.
      WHEN client->check_on_init( ).
        key_date = sy-datum.
        data_read( ).
        overdue_calc( ).
        view_display( ).
      WHEN client->check_on_navigated( ).
        view_display( ).
      WHEN client->check_on_event( `RECALC` ).
        overdue_calc( ).
    ENDCASE.

  ENDMETHOD.

  METHOD data_read.
    " in your system: SELECT FROM the invoice table
    t_invoices = VALUE #( ( invoice = `4711` due = `20260801` open = abap_true )
                          ( invoice = `4712` due = `20260930` open = abap_true )
                          ( invoice = `4713` due = `20260701` open = abap_false ) ).
  ENDMETHOD.

  METHOD overdue_calc.
    t_overdue = VALUE #( FOR inv IN t_invoices
                         WHERE ( open = abap_true AND due < key_date )
                         ( inv ) ).
  ENDMETHOD.

  METHOD view_display.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->a( n = `title` v = `Overdue`

                )->tag( `DatePicker`
                    )->a( n = `value`         v = client->_bind( key_date )
                    )->a( n = `valueFormat`   v = `yyyyMMdd`
                    )->a( n = `displayFormat` v = `medium`
                    )->a( n = `change`        v = client->_event( `RECALC` )

                )->ele( `List`
                    )->a( n = `items` v = client->_bind( t_overdue )
                    )->ele( `items`
                        )->tag( `StandardListItem`
                            )->a( n = `title` v = `{INVOICE}`
                            )->a( n = `info`  v = `{DUE}` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

The test lives in the include, and nothing in it is abap2UI5:

```abap
CLASS ltcl_overdue DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS only_open_and_past_due FOR TESTING.

ENDCLASS.


CLASS ltcl_overdue IMPLEMENTATION.

  METHOD only_open_and_past_due.

    DATA(cut) = NEW zcl_app_overdue( ).
    cut->key_date = `20260901`.
    cut->data_read( ).

    cut->overdue_calc( ).

    cl_abap_unit_assert=>assert_equals(
        act = lines( cut->t_overdue )
        exp = 1
        msg = `4711 is the only open invoice past the key date` ).

  ENDMETHOD.

ENDCLASS.
```

No client, no HTTP, no browser. The test sets attributes, calls a method,
reads attributes — the same test it would be for a class with no screen at all,
because up to `view_display( )` it *is* a class with no screen at all.

Two things are worth keeping that way on purpose. Handler methods take their
input from attributes rather than from `client->get( )`, so a test can set the
input. And `data_read( )` is its own method, so a test can either run the real
`SELECT` against test data or fill the table by hand and test the logic alone.

The view itself has a different kind of check. The [linter](/advanced/linter)
reconstructs the XML the chain builds and holds it against the UI5 metadata —
unknown control, misspelled property, wrong type — without a system. Logic in
ABAP Unit, view in the linter, and the roundtrip in a browser.

**Keep the client out of the logic, and the logic is testable the way any
ABAP class is.**

Happy ABAPing! 🦖🦕🦣
