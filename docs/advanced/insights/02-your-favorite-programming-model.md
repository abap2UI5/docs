# #2 abap2UI5 in Your Favorite Programming Model

Before you use a framework, you want to know what it expects from you: a
structure to follow, a lifecycle to fit into, layers to fill in. Usually that
list is long, and most of it exists before anything shows up on a screen.

With abap2UI5 you start by writing an ABAP class, the way you used to start a
report with a selection screen. The only thing asked of you is to implement one
interface with one method:

```abap
INTERFACE z2ui5_if_app PUBLIC.
  INTERFACES if_serializable_object.

  METHODS main
    IMPORTING
      client TYPE REF TO z2ui5_if_client.

ENDINTERFACE.
```

abap2UI5 calls `main( )` on every roundtrip; your class decides what to display
and how to react. That is the whole contract. And getting it on the screen is
about as short as pressing F8: activate the class, append `?app_start=zcl_my_app`
to your service URL, and there it is.

Beyond that, there are no rules. No service to define, no binding to maintain,
no annotations, nothing to transport but the class itself. And it is entirely up
to you whether you build one huge app of 10,000 lines, split it across several
apps, or design an architecture of your own out of parent and child classes.

abap2UI5 makes no demands on how you write your code or design your model — use
whichever programming model you like behind it.

This has a pleasant side effect. Since abap2UI5 just serves a UI and never asks
where your data comes from, the data can come from wherever it already lives.

## The Same Class, Three Programming Models

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

*The entity is invented — use your favorite RAP object instead.*

Nothing in the save handler is abap2UI5's responsibility, and the RAP object
does not notice anything unusual — validations, determinations and
authorizations all still run. EML does not care who calls it.

That handler belongs to the consuming app, and it is the only place in the class
that knows what is behind the screen. abap2UI5 does not.

So the same class can leave RAP out of it and write straight to a database
table, the classic way:

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

*The table is invented — use your favorite database table instead.*

Or a classic BAPI you have been using in the system for the last twenty years —
different object, different decade, same UI around it:

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

*The BAPI is just an example — use your favorite BAPI instead.*

Three programming models, one unchanged UI class. abap2UI5 sees the same thing
in all three: a method that ran and returned. It never looks inside. The same
app could call your EWM delivery classes, a proxy to another system, or whatever
SAP releases next year.

## Conclusion

There are plenty of use cases where a strict programming model is exactly what
you want. But there are also the ones where a developer needs more freedom —
customers who are not on the newest release, and logic that never quite fits the
shape a programming model has in mind.

So this is not an either-or. abap2UI5 runs in the same system, under the same
authorizations, in the same launchpad, and it reaches your business logic
however you like. It is one more option next to what you already run, for the
screen that would otherwise not get built at all.

One interface, one method, and no opinion about what is behind the screen.

Happy ABAPing! 🦖🦕🦣
