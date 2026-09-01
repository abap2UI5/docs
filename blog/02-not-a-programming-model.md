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
screen can come from wherever you already keep it. The event handler is
ordinary ABAP:

```abap
MODIFY ENTITIES OF z_i_travel
  ENTITY travel
    UPDATE FIELDS ( description )
    WITH VALUE #( ( %key-travel_id = ms_travel-travel_id
                    description    = ms_travel-description ) )
  FAILED   DATA(lt_failed)
  REPORTED DATA(lt_reported).

IF lt_failed IS INITIAL.
  COMMIT ENTITIES.
  client->message_toast_display( `Saved` ).
ENDIF.
```

Nothing there is abap2UI5 except the toast. The business object is untouched —
its validations, determinations, authorizations and draft handling all still
run, because they sit behind EML and EML does not care who makes the call.

Deployed as an ICF node, the app then registers in the Fiori launchpad next to
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
> Which is why it composes instead of competing: the event handler is ordinary
> ABAP, so it calls your RAP business object through EML like any other
> consumer. The BO never learns that a different UI is in front of it.
>
> New article 🎉
>
> What does your UI framework ask of your architecture?
>
> #ABAP #SAP #UI5
