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
the application decides what to display and how to react, and the conversation
ends there.

The more interesting half is what the contract does **not** contain. No data
model. No behavior definition. No service, no binding, no annotations. No BSP
application per app, no frontend artefact to transport. Activating the class
and calling the ICF endpoint is the deployment.

That is a statement about scope, not size — and it is why the data behind the
screen can come from wherever it already lives. Here is a complete application
that writes through a business object:

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
all still run, because they sit behind EML and EML does not care who makes the
call.

Which means the same skeleton reaches anything else ABAP can reach. Swap the
handler for a function module and the rest of the class does not move:

```abap
CLASS zcl_open_items DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_item,
        document TYPE string,
        due      TYPE string,
        amount   TYPE string,
      END OF ty_s_item.

    DATA customer TYPE string.
    DATA items    TYPE STANDARD TABLE OF ty_s_item WITH EMPTY KEY.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.
    METHODS set_view.
    METHODS read_items.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_open_items IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->get_event( ) = `READ`.
      read_items( ).
    ENDIF.
    set_view( ).

  ENDMETHOD.

  METHOD set_view.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->a( n = `title` v = `Open Items`

                )->tag( `Input`
                    )->a( n = `value`  v = client->_bind( customer )
                    )->a( n = `submit` v = client->_event( `READ` )

                )->ele( `List`
                    )->a( n = `items` v = client->_bind( items )

                    )->ele( `items`
                        )->tag( `StandardListItem`
                            )->a( n = `title` v = `{DOCUMENT}`
                            )->a( n = `info`  v = `{AMOUNT}` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD read_items.

    CALL FUNCTION 'Z_GET_OPEN_ITEMS'
      EXPORTING iv_customer = customer
      IMPORTING et_items    = items
      EXCEPTIONS not_found  = 1
                 OTHERS     = 2.

    IF sy-subrc <> 0.
      client->message_box_display( text = `No items found`
                                   type = `error` ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
```

Two applications, one shape. A business object in the first, a decades-old
function module in the second, and the framework never learns which — it asked
for `main( )` and got it.

Deployed as an ICF node, either app registers in the Fiori launchpad next to
the tiles already there, and users cannot tell it apart from them.

**A framework that asks for one method cannot reorganise an architecture. It
never learns enough about it to try.**

What it is not, plainly: no data model, no transactional buffer, no generated
user interface. Applications that need those need something that provides them.

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
> Which is why it composes instead of competing. The new article shows the same
> class twice: once writing through a RAP business object with EML, once calling
> an old function module. Same shape, and the framework never learns which.
>
> New article 🎉
>
> What does your UI framework ask of your architecture?
>
> #ABAP #SAP #UI5
