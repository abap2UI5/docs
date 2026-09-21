# #3 One Class, No Programming Model

To use a framework, you always have to fulfill certain requirements: these can
be a structure to follow, a lifecycle to fit into, layers to fill in — most of
them in place before anything reaches a screen.

Here is the whole list for abap2UI5. You write an ABAP class — the way you used
to start a report with a selection screen — and implement one interface with
one method:

```abap
INTERFACE z2ui5_if_app PUBLIC.
  INTERFACES if_serializable_object.

  METHODS main
    IMPORTING
      client TYPE REF TO z2ui5_if_client.

ENDINTERFACE.
```

That is it. abap2UI5 calls `main( )` on every roundtrip, and your class decides
what to display and how to react. Activate it, append `?app_start=zcl_my_app`
to your service URL, and there it is — about as ceremonious as pressing F8.

No service to define, no binding to maintain, no annotations, nothing to
transport but the class. And no programming model either: `main( )` is an
ordinary ABAP method, so the demands end at its signature. Whatever your ABAP
can call, your app can call — so let's look at what that makes possible:

## One Screen, Three Decades of ABAP

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

*The entity is just an example — use your favorite RAP object instead.*

The RAP object does not notice anything unusual — validations, determinations
and authorizations all still run. EML does not care who calls it.

`on_save( )` is also the only place in the class that knows what is behind the
screen. So let it know something else — straight to a database table, the
classic way:

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

*The table is just an example — use your favorite table instead.*

Or a BAPI that has been running in production there for twenty years —
different object, different decade, same screen in front of it:

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

## No Model to Fit Into

Three save handlers, one unchanged UI class. abap2UI5 never looks inside any of
them — the only thing it takes back out of your class is a view.

This could be called *programming model agnostic* — except RAP is a programming
model, and a `MODIFY` is not, a BAPI is not, and neither is the EWM delivery
class or the function module somebody wrote in the 2000s that has run every
night since. What the three handlers have in common is not a model. It is that
they are ABAP statements in a method — and that is the entire requirement.

Which is the useful property, because a grown SAP system was never written in
one model but in all of them at once, by decade — and the layers do not line up
with screens: a single click routinely reads a CDS view, checks a function
module and writes through EML.

So nothing has to be released first, and nothing has to move. When the table
does become a business object, `on_save( )` changes and the rest of the class
does not. That is the difference between a screen that gets built this week and
one that waits for a modeling exercise.

## Conclusion

Where a strict programming model fits, use it — picking one deliberately is
good engineering. abap2UI5 is for the rest: the customer not on the newest
release, the logic that never quite fits the shape a model has in mind, and the
screen from [the last article](/advanced/insights/02-the-cost-of-a-screen) that
would otherwise not get built at all.

One class, no programming model, and no opinion about what is behind the
screen.

Give abap2UI5 a try. 🦖🦕🦣
