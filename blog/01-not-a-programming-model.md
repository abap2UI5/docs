# abap2UI5 is not a Programming Model

*abap2UI5 Know-How #1*

An ABAP team needs a screen. Not an application — a screen. A maintenance view
for a customising table nobody wants to explain in SM30. A cockpit that shows
what last night's job actually did. An approval step for one department. A test
harness for three people.

The business logic behind such a screen is often thirty lines. The cost of
putting a user interface in front of those thirty lines is not proportional to
them, and it does not scale down: a data model, a service, a binding, frontend
artefacts, a deployment, and something that now has to be maintained and
eventually deprecated. So the screen does not get built, or it gets built as a
selection screen and an ALV grid, and everyone agrees to stop thinking about
it.

abap2UI5 addresses that gap, and the way it does so is worth stating precisely,
because it is unusual: it does not add a way to build applications. It adds a
way to draw a screen from ABAP, and then gets out of the way.

## The whole contract

A programming model tells an application how to be built — its structure, its
lifecycle, its layers, its transactional behaviour. abap2UI5 does none of that.
This is the entire contract it asks an application to fulfil:

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

What is equally important is the list of things that are *not* in that
contract. There is no data model to declare, no service to define, no binding
to create, no annotation model, no BSP application per app, and no frontend
artefact to transport. Activating the class and calling the ICF endpoint with
`?app_start=zcl_my_app` is the deployment.

## Everything below the screen is untouched

Because the contract covers only the screen, an abap2UI5 application makes no
claim on where its data comes from. It can run a `SELECT`, call a function
module, use a class that has existed since 2011, or drive a modern business
object. The event handler is ordinary ABAP, so it calls whatever the system
already offers:

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
        client->message_box_display( val  = `Save failed`
                                     type = `error` ).
      ENDIF.

  ENDCASE.

ENDMETHOD.
```

The business object in that snippet is untouched by abap2UI5. Its validations,
determinations, authorizations and draft handling all still run, because they
sit behind the call and the call does not care who makes it. The same is true
of a function module, a BAdI, or thirty lines in a local class: abap2UI5 sees a
screen and an event, and the layers underneath keep whatever design they
already had.

That is what "minimally invasive" means in practice. A framework that asks for
one method cannot reorganise an architecture, because it never learns enough
about it to try.

## What the small contract buys

The obvious gain is the screens that were previously not worth building. One
class, no service, nothing published, nothing to deprecate — the cost finally
scales down to match a thirty-line problem.

The less obvious gain is what happens when such a tool grows up. Because
abap2UI5 never asked for the business logic, the business logic was never in
the UI layer. If the maintenance view turns out to be a real application that
deserves a governed service, the part worth keeping is already sitting where it
belongs, behind an interface, ready to be called by something else.

And because the framework is MIT-licensed and installs on any ABAP release from
7.02 up to ABAP Cloud, finding out whether it fits costs an installation and a
class rather than a project.

## What it is not

abap2UI5 has no data model, no behavior definition, no transactional buffer,
and no generated user interface. It will not derive a screen from annotations,
it will not manage drafts, and it will not publish anything a foreign system
can consume. Applications that need those things need something that provides
them, and abap2UI5 is not a candidate — it has never been trying to be one.

What it is, is a way for ABAP code to put a UI5 screen in front of itself
without first becoming a different kind of application. For the screens
described at the top of this article, that turns out to be the only thing that
was missing.

---

## LinkedIn teaser post

> An ABAP team needs a screen. Not an application — a maintenance view, a job
> monitor, an approval step for one department. The logic behind it is thirty
> lines; the cost of putting a UI in front of those thirty lines is not, and it
> does not scale down. So the screen never gets built.
>
> abap2UI5 asks an application for exactly one interface with one method, and
> makes no claim on anything underneath it — the data access, the business
> logic and the persistence keep whatever design they already had.
>
> New article on what that small contract buys, with code: [link]
>
> Which screen in your system has stayed a selection screen and an ALV grid
> purely because a proper UI was never worth the effort?
>
> #ABAP #SAP #UI5
