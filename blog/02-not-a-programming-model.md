# abap2UI5 is not a Programming Model

*abap2UI5 Know-How #2 — draft*

> **This article and #3 answer different questions.** This one is structural:
> what does abap2UI5 ask of the architecture you already have? (Nothing.)
> [#3, *The Cost of a Screen*](03-the-cost-of-a-screen.md), is economic: why do
> so many small screens never get built? Neither argument needs the other.

Frameworks that put a screen in front of ABAP tend to ask for a great deal in
return: a structure to follow, a lifecycle to fit into, layers to fill in. The
useful question about any of them is therefore not "what can it do" but "what
does it want from me".

For abap2UI5 the answer fits on a page, so here it is in full.

## The whole contract

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
ends there. There is nothing else to implement, extend, register or configure.

Equally important is the list of things that are *not* in that contract. No
data model to declare. No behavior definition. No service to define, no binding
to create, no annotation model. No BSP application per app, and no frontend
artefact to transport. Activating the class and calling the ICF endpoint with
`?app_start=zcl_my_app` is the deployment.

That is what "minimally invasive" means here, and it is a statement about
scope rather than about size: the framework never learns what your application
is, only what it wants on screen.

## Everything below the screen is untouched

Because the contract covers only the screen, an abap2UI5 application makes no
claim on where its data comes from or what happens to it afterwards. The event
handler is ordinary ABAP, so it calls whatever the system already offers — a
`SELECT`, a function module, a class that has existed since 2011, or a modern
business object:

```abap
METHOD on_event.

  CASE client->get_event( ).

    WHEN `SAVE`.

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
      ELSE.
        client->message_box_display( text = `Save failed`
                                     type = `error` ).
      ENDIF.

  ENDCASE.

ENDMETHOD.
```

The business object in that snippet is untouched by abap2UI5. Its validations,
determinations, authorizations and draft handling all still run, because they
sit behind the call and the call does not care who makes it. The same is true
of a function module, a BAdI, or thirty lines in a local class.

A framework that asks for one method cannot reorganise an architecture,
because it never learns enough about it to try.

## Nothing here is exotic, on either side

The frontend is a freestyle UI5 app: `sap.m` controls, XML views, a
`JSONModel`, two-way binding. No proprietary control library, no template
language.

The backend is a global ABAP class — in a package, travelling in a transport,
opening in ADT like any other class, compiling on ABAP Standard and ABAP Cloud
alike. Ordinary ABAP OO, so it is unit-testable, and there is no generated
repository object anywhere.

And the deployment is an ICF node, which means the app registers in the Fiori
launchpad next to the tiles already there. A user cannot tell it apart from
them.

## What it is not

abap2UI5 has no data model, no behavior definition, no transactional buffer,
and no generated user interface. It will not derive a screen from annotations,
it will not manage drafts, and it will not publish anything a foreign system
can consume. Applications that need those things need something that provides
them, and abap2UI5 is not a candidate — it has never been trying to be one.

What it is, is a way for ABAP code to put a UI5 screen in front of itself
without first becoming a different kind of application.

Whether that is worth doing for any particular screen is a question about
cost, and that is [the next article](03-the-cost-of-a-screen.md).

---

## LinkedIn teaser post

> The useful question about a UI framework is not what it can do. It is what it
> wants from you: a structure to follow, a lifecycle to fit into, layers to
> fill in.
>
> For abap2UI5 the answer is one interface with one method. No data model to
> declare, no service to define, no binding, no annotations, no BSP per app, no
> frontend transport.
>
> Which is why it composes instead of competing: the event handler is ordinary
> ABAP, so it calls your RAP business object through EML like any other
> consumer. The BO never learns that a different UI is in front of it.
>
> New article on what that small contract does and does not cover 🎉
>
> What does your UI framework ask of your architecture?
>
> #ABAP #SAP #UI5
