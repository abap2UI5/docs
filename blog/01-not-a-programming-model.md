# abap2UI5 is not a Programming Model

*abap2UI5 Know-How #1*

Whenever abap2UI5 comes up in a conversation about ABAP UI development, the
first question tends to be some form of "so, abap2UI5 or RAP?". It is a
reasonable question to ask, and I think it is the wrong one — not because one
of the two wins, but because the two are not the same kind of thing. RAP is a
programming model. abap2UI5 is not. Everything interesting about how they
relate follows from that.

## What a programming model prescribes

A programming model tells you *how* to build your application. RAP does that,
and it does it thoroughly: a CDS data model, a behavior definition, a behavior
implementation, a projection layer, a service definition, a service binding.
Structure, lifecycle, the transactional buffer, draft handling — the model
prescribes all of it, and in return it hands you a great deal for free. Locking,
ETags, draft persistence, a typed OData service, and a Fiori Elements UI that
you never wrote a line of.

That is a genuinely good trade for the applications RAP was designed for. It is
also, unavoidably, a commitment: you build the application the way the model
says, from the database up to the UI.

## What abap2UI5 prescribes

One thing. This is the entire contract:

```abap
INTERFACE z2ui5_if_app PUBLIC.
  INTERFACES if_serializable_object.

  METHODS main
    IMPORTING
      client TYPE REF TO z2ui5_if_client.

ENDINTERFACE.
```

One interface, one method. That is the whole surface an application touches.
The framework calls `main( )` on every roundtrip, the app decides what to
display and how to react, and the conversation ends there.

Everything else stays yours: where the data comes from, how you persist it, how
you authorize, how you structure your classes, whether you sit on a BOPF
object, a RAP business object, a function module, or a plain `SELECT`. abap2UI5
has no opinion about any of it, because it is not a model — it is a UI layer.

That is what "minimally invasive" means concretely. There is no data model to
declare, no service to define, no binding to create, no BSP application per
app, and no frontend artefact to transport. You activate a class and call the
ICF endpoint with `?app_start=zcl_my_app`.

## The consequence: it composes

Because abap2UI5 makes no claim on the layers underneath it, nothing stops an
abap2UI5 app from being a RAP consumer. The event handler calls EML the way any
other ABAP code would:

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

Your business object stays your business object. Its validations,
determinations, authorizations and draft handling all still run — they sit
behind EML, and EML does not care who calls it. The investment you made in the
model is untouched.

What changed is only the thing in front of it. Instead of a service binding, an
OData V4 metadata document and an annotation model, there is an ABAP class that
draws a screen. For a Fiori Elements app that is a bad trade — you would be
giving up generated UI to write it by hand. For a screen that Fiori Elements
cannot express, it is the difference between having the screen and not having
it.

## Where the small contract earns its place

There is a category of screen that a full programming model is expensive for.
An internal maintenance tool. A migration cockpit. A monitoring view for a
background job. A one-off approval screen for one department. A test harness
that three people use. The business logic in these is often thirty lines; the
ceremony of publishing them as a governed service is not.

For those, the size of the contract *is* the feature. One class, no service,
nothing to deprecate later. And when such a tool grows up and deserves a proper
service, the business logic never lived in the UI layer to begin with — so it
moves.

## The part I want to be explicit about

This is not an argument that abap2UI5 is better than RAP, and I would rather
not have that conversation at all. RAP is the right answer for a published,
typed, governed, transactional business service. It is very good at that, and
abap2UI5 is not attempting to be a second one — it has no data model, no
behavior, no transactional buffer, and no ambition to acquire them.

The claim is narrower and, I think, more useful: because abap2UI5 asks for one
interface method and nothing else, it cannot take anything away from the
architecture you already have. It sits next to RAP, on top of RAP, or nowhere
near RAP, depending on the screen. It is MIT-licensed and installs on any ABAP
release from 7.02 up to ABAP Cloud, so trying it costs an installation and a
class.

A framework that asks for one method cannot take much away from you. That is
the point.

---

## LinkedIn teaser post

> The first question people ask about abap2UI5 is usually "abap2UI5 or RAP?".
> I think that is the wrong question — not because one of them wins, but
> because they are not the same kind of thing. RAP is a programming model:
> it tells you how to build the application, from the data model up. abap2UI5
> asks for one interface with one method, and has no opinion about anything
> else. Which is why an abap2UI5 app can quite happily call your RAP business
> object through EML and leave it exactly as it is.
>
> I wrote up what that difference means in practice, with code: [link]
>
> Curious about the other direction though — has anyone here put an abap2UI5
> screen in front of an existing RAP BO, and where did it get awkward?
>
> #ABAP #SAP #UI5
