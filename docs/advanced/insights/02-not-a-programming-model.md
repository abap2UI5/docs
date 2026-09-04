# #2 abap2UI5 Is Not a Programming Model

When you pick up a framework for building screens, the first question is
usually what it can do. The more useful one is what it expects from you: a
structure to follow, a lifecycle to fit into, layers to fill in. Most
frameworks have a good answer to that, and the answer is usually a few pages
long.

The one from abap2UI5 fits in a code block, so here it is in full:

```abap
INTERFACE z2ui5_if_app PUBLIC.
  INTERFACES if_serializable_object.

  METHODS main
    IMPORTING
      client TYPE REF TO z2ui5_if_client.

ENDINTERFACE.
```

One interface, one method. The framework calls `main( )` on every roundtrip,
your class decides what to display and how to react, and that is the end of the
contract.

## What Is Not in It

The more interesting half is what the interface does not ask for. No data
model. No behavior definition. No service, no binding, no annotations. No BSP
application per app, no frontend artefact to transport. You activate the class,
call the ICF endpoint, and the app is there.

That is a statement about scope rather than about size, and it has one pleasant
side effect: because abap2UI5 never asks where your data comes from, it can
come from wherever it already lives.

## The Same Class Around Three Different Backends

Here is a small edit screen writing through a RAP business object:

<!-- playground: no Run button — writes through a RAP business object, which only a system has -->
```abap
CLASS zcl_travel_edit DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA travel_id   TYPE string.
    DATA description TYPE string.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.
    METHODS set_view.
    METHODS on_save.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_travel_edit IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->get_event( ) = `SAVE`.
      on_save( ).
    ENDIF.
    set_view( ).

  ENDMETHOD.

  METHOD set_view.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->a( n = `title` v = `Travel`

                )->tag( `Input`
                    )->a( n = `value` v = client->_bind( travel_id )

                )->tag( `Input`
                    )->a( n = `value` v = client->_bind( description )

                )->tag( `Button`
                    )->a( n = `text`  v = `Save`
                    )->a( n = `type`  v = `Emphasized`
                    )->a( n = `press` v = client->_event( `SAVE` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD on_save.

    MODIFY ENTITIES OF z_i_travel
      ENTITY travel
        UPDATE FIELDS ( description )
        WITH VALUE #( ( %key-travel_id = travel_id
                        description    = description ) )
      FAILED DATA(failed).

    IF failed IS INITIAL.
      COMMIT ENTITIES.
      client->message_toast_display( `Saved` ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
```

Nothing in the save handler is abap2UI5 except the toast at the end. The
business object does not notice anything unusual either — its validations,
determinations, authorizations and draft handling all still run, because EML
does not care who makes the call.

The handler is also the only part of the class that knows what is behind the
screen. Writing straight to a database table, it looks like this:

```abap
  METHOD on_save.

    DATA(row) = VALUE ztravel( travel_id   = travel_id
                               description = description ).

    MODIFY ztravel FROM @row.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      client->message_toast_display( `Saved` ).
    ENDIF.

  ENDMETHOD.
```

And against a BAPI you have had in the system for twenty years — a different
object, a different decade, the same class around it:

```abap
  METHOD on_save.

    DATA bapi_return TYPE STANDARD TABLE OF bapiret2.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING salesdocument    = order_id
                order_header_in  = VALUE bapisdh1( purch_no_c = po_number )
                order_header_inx = VALUE bapisdh1x( updateflag = 'U'
                                                    purch_no_c = abap_true )
      TABLES    return           = bapi_return.

    IF line_exists( bapi_return[ type = 'E' ] ).
      client->message_box_display( text = |{ bapi_return[ type = 'E' ]-message }|
                                   type = `error` ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = abap_true.

    client->message_toast_display( `Saved` ).

  ENDMETHOD.
```

None of the three examples is the point in itself. The point is that abap2UI5
sees the same thing in all of them: a method that ran and returned. It never
looks inside. The same handler could call your EWM delivery classes, a proxy to
another system, or whatever SAP releases next year — none of that has to be
taught to abap2UI5, because abap2UI5 never asks what is behind the screen.

That is the whole advantage here, and it is a modest one: a framework that only
asks for one method cannot reorganise your architecture, simply because it
never learns enough about it to try. What you have already built stays where it
is and keeps its own rules.

## And What You Do Not Get

The other side, just as plainly: no data model, no transactional buffer, no
generated user interface. You write the view yourself. An application that
needs those things is better served by a framework that provides them — RAP and
Fiori Elements do exactly that, and they do it well.

So abap2UI5 is not a replacement for anything. It is one more option next to
what you already run, for the cases where one method really is all you need.

Happy ABAPing! 🦖🦕🦣
