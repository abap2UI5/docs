# #3 Whatever Your ABAP Can Call

Before you use a framework, you want to know what it expects from you: a
structure to follow, a lifecycle to fit into, layers to fill in, names to get
right. Usually that list is long, and most of it has to exist before anything
shows up on a screen.

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

That is it. That is the contract. abap2UI5 calls `main( )` on every roundtrip,
and your class decides what to display and how to react. Getting it on screen
is about as ceremonious as pressing F8: activate the class, append
`?app_start=zcl_my_app` to your service URL, and there it is.

Beyond that, there are no rules. No service to define, no binding to maintain,
no annotations, nothing to transport but the class itself.

And because `main( )` is an ordinary ABAP method, the demands end there — in
particular, the framework never asks where your data comes from. Whatever your
ABAP can call, your app can call. Which is worth showing rather than claiming.

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

*The entity is invented — use your favorite RAP object instead.*

Nothing in that save handler is abap2UI5's responsibility, and the RAP object
does not notice anything unusual — validations, determinations and
authorizations all still run. EML does not care who calls it.

`on_save( )` is also the only place in the class that knows what is behind the
screen. So let it know something else. Straight to a database table, the
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

*The table is invented — use your favorite database table instead.*

Or a BAPI you have been calling in that system since before the phone in your
pocket existed — different object, different decade, same screen in front of
it:

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

## The Framework Never Looks Inside

Three save handlers, one unchanged UI class. abap2UI5 sees the same thing in
all three: a method that ran and returned. It has no opinion about what
happened in between, because it never looks — the only thing it takes back out
of your class is a view.

This gets called *programming model agnostic*, and that phrase is too small
for it. RAP is a programming model. A `MODIFY` is not. A BAPI is not, and
neither is the EWM delivery class, the proxy to a neighboring system, or the
function module somebody wrote in 1998 that has run every night since. What
the three handlers have in common is not a model. It is that they are ABAP
statements in a method — and that is the entire requirement.

Which is the useful property, because a grown SAP system was never written in
one model. It was written in all of them at once, in layers, by decade, and
the layers do not line up with screens. A single click routinely crosses
several: read a CDS view, check something in a function module, write through
EML, then put a message on screen out of a `bapiret2` table. That is an
ordinary ABAP method — and an awkward thing to express as soon as the UI
framework has a preferred way in.

So the practical question is not *which programming model does abap2UI5
support*. It is what you have to do **first**, before the screen can exist at
all:

- **Nothing has to be released.** No OData service, no projection view, no
  behavior definition, no wrapper around the function module to make it
  presentable to a UI layer.
- **Nothing has to move.** The logic stays in the class, the module pool
  include or the function group where it already is, at whatever release level
  it already has. The screen is new; nothing behind it has to be.
- **Nothing is locked in.** When the table does become a business object and
  the BAPI a released API, `on_save( )` changes and the rest of the class does
  not. The view never knew which one it was.

## Conclusion

There are plenty of use cases where a strict programming model is exactly what
you want, and picking one deliberately is good engineering. But there are also
the ones where a developer needs more freedom — customers who are not on the
newest release, logic that never quite fits the shape a model has in mind, and
the screen from [the last article](/advanced/insights/02-the-cost-of-a-screen)
that would otherwise not get built at all.

abap2UI5 runs in the same system, under the same authorizations, in the same
launchpad as all your other UI5 and RAP apps. It reaches your business logic
the way any other ABAP class would: by calling it.

One interface, one method, and no opinion about what is behind the screen.
