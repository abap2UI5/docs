# #2 abap2UI5 Is Not a Programming Model

Before you use a framework, you want to know what it expects from you: a
structure to follow, a lifecycle to fit into, layers to fill in.

For abap2UI5 it is one interface with one method:

```abap
INTERFACE z2ui5_if_app PUBLIC.
  INTERFACES if_serializable_object.

  METHODS main
    IMPORTING
      client TYPE REF TO z2ui5_if_client.

ENDINTERFACE.
```

The framework calls `main( )` on every roundtrip, your class decides what to
display and how to react. That is the whole contract.

## What Is Not in It

No data model. No behavior definition. No service, no binding, no annotations.
No BSP application per app, no frontend artefact to transport. You activate the
class, call the ICF endpoint, and the app is there.

That is about scope, not about size — and it has a pleasant side effect. Since
abap2UI5 just serves a UI5 screen and never asks where your data comes from, it
can come from wherever it already lives.

## The Same Class Around Three Backends

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

Nothing in the save handler is abap2UI5 except the toast. The business object
does not notice anything unusual — validations, determinations, authorizations
and draft handling all still run, because EML does not care who calls it.

That handler is also the only place in the class that knows what is behind the
screen. Straight to a database table:

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

Or against a BAPI you have had in the system for twenty years — different
object, different decade, same class around it:

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

abap2UI5 sees the same thing in all three: a method that ran and returned. It
never looks inside. The same handler could call your EWM delivery classes, a
proxy to another system, or whatever SAP releases next year — none of it has to
be taught to abap2UI5.

And that is the advantage, a modest one: a framework that asks for one method
cannot reorganise your architecture. It never learns enough about it to try.
What you have already built stays where it is and keeps its rules.

## And What You Do Not Get

No data model, no transactional buffer, no generated user interface. RAP gives
you all of that. For an application with a straightforward use case, done the
standard way, that is where you want to be.

So this is not an either-or. abap2UI5 runs in the same system, under the same
authorizations, in the same launchpad, and it reaches your business logic
however you like. It is one more option next to what you already run, for the
screen that would otherwise not get built at all.

One interface, one method, and no opinion about what is behind the screen.

Happy ABAPing! 🦖🦕🦣
