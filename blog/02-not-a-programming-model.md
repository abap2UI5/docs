# abap2UI5 is not a Programming Model

*abap2UI5 Know-How #2 — draft*

The useful question about a UI framework is not what it can do. It is what it
wants from you: a structure to follow, a lifecycle to fit into, layers to fill
in.

abap2UI5 fits its answer on a page, so here it is, complete:

```abap
INTERFACE z2ui5_if_app PUBLIC.
  INTERFACES if_serializable_object.

  METHODS main
    IMPORTING
      client TYPE REF TO z2ui5_if_client.

ENDINTERFACE.
```

One interface, one method. The framework calls `main( )` on every roundtrip,
the application decides what to display and how to react, and it ends there.

The more interesting half is what the contract does **not** contain. No data
model. No behavior definition. No service, no binding, no annotations. No BSP
application per app, no frontend artefact to transport. Activating the class
and calling the ICF endpoint is the deployment.

That is a statement about scope, not size — and it is why the data behind the
screen can come from wherever it already lives:

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

Nothing in the handler is abap2UI5 except the toast. The business object is
untouched — its validations, determinations, authorizations and draft handling
all still run, because EML does not care who makes the call.

The handler is also the only part of the class that knows what is behind the
screen. Against a database table:

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

And against a BAPI — a different object, a different decade, the same class
around it:

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

All three are only examples, and none of them is the point. The framework sees
the same thing every time: a method that ran and returned. It never looks
inside. The same handler could call the EWM delivery classes, a proxy to another
system, or whatever SAP releases next — none of it has to be taught to abap2UI5,
because abap2UI5 never asks what is behind the screen.

**A framework that asks for one method cannot reorganise an architecture. It
never learns enough about it to try.**

What it is not, plainly: no data model, no transactional buffer, no generated
user interface. Applications needing those need something that provides them.

Happy ABAPing! 🦖🦕🦣

---

## LinkedIn teaser post

Plain text — LinkedIn renders no markdown.

> The useful question about a UI framework is not what it can do. It is what it
> wants from you: a structure to follow, a lifecycle to fit into, layers to fill
> in.
>
> abap2UI5 fits its answer on a page — one interface, one method. No data model,
> no service, no binding, no annotations, no BSP per app, no frontend artefact
> to transport.
>
> Which is why it composes instead of competing. The new article shows one app
> and three save handlers: EML against a business object, MODIFY against a
> table, and a BAPI call. The framework never learns which — it could just as
> well be the EWM classes, or whatever SAP releases next.
>
> New article 🎉
>
> What does your UI framework ask of your architecture?
>
> #ABAP #SAP #UI5
