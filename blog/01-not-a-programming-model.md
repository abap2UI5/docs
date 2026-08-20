# abap2UI5 Know-How #1 — abap2UI5 is not a Programming Model

> **Status:** draft · **Target:** LinkedIn native post + code carousel
> **Frame:** not a comparison with RAP. The EML snippet is the argument.

## Hook (the ~200 characters visible before "see more")

> RAP is a programming model. abap2UI5 is not.
>
> That is not a criticism — it is a category difference, and it decides whether
> the two compete or combine.

## Body

A programming model tells you *how* to build your application. RAP does that,
and it does it well: a CDS data model, a behavior definition, a behavior
implementation, a projection layer, a service definition, a service binding.
Structure, lifecycle, transactional buffer, draft handling — the model
prescribes them, and in return it gives you a great deal for free.

abap2UI5 prescribes exactly one thing. This is the entire contract:

```abap
INTERFACE z2ui5_if_app PUBLIC.
  INTERFACES if_serializable_object.

  METHODS main
    IMPORTING
      client TYPE REF TO z2ui5_if_client.

ENDINTERFACE.
```

One interface. One method. That is the whole surface an application touches.

Everything else stays yours: where the data comes from, how you persist it, how
you authorize, how you structure your classes, whether you use a BOPF object, a
RAP business object, a function module, or a plain `SELECT`. abap2UI5 has no
opinion about any of it, because it is not a model — it is a UI layer.

### The interesting consequence: it composes with RAP

Nothing stops an abap2UI5 app from being a RAP consumer. The event handler
calls EML like any other ABAP code would:

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
behind EML, and EML does not care who calls it. What changed is only the thing
in front: instead of a service binding, an OData V4 metadata document and a
Fiori Elements annotation model, there is an ABAP class that draws a screen.

### Why that matters in practice

There is a class of screens a full programming model is expensive for. An
internal maintenance tool. A migration cockpit. A monitoring view for a
background job. A one-off approval screen for one department. The business
logic is thirty lines; the ceremony of publishing it as a service is not.

For those, minimal invasiveness is the entire value: activate a class, call the
ICF endpoint, done. No service definition, no binding, no BSP application per
app, no transport of frontend artefacts. And when the tool grows up and
deserves a proper service, the business logic never lived in the UI layer
anyway — so it moves.

### The framing, stated plainly

This is not a "which one is better" post. RAP is the right answer for a
published, typed, governed, transactional business service. It is genuinely
good at that, and abap2UI5 is not trying to be a second one.

abap2UI5 is MIT-licensed and installs into any ABAP release from 7.02 to ABAP
Cloud. It is one more option in the toolbox, next to what you already have —
not a replacement for it.

*A framework that asks for one interface method cannot take much away from you.
That is the point.*

---

**Closing question:** Where would a screen be worth building if it cost you one
class and no service?

**Series line:** `abap2UI5 Know-How — #1 Not a Programming Model · #2 RTTI (next)`

**Hashtags:** `#ABAP #SAP #UI5 #Fiori #RAP #OpenSource`

**First comment:** link to https://github.com/abap2UI5/abap2UI5

## Carousel outline (6 pages)

1. **abap2UI5 is not a programming model** — and that is the point
2. **What a programming model prescribes** — CDS, BDEF, projection, service
   definition, service binding
3. **What abap2UI5 prescribes** — the `z2ui5_if_app` interface, in full, as one
   slide. The whole punchline is that it fits
4. **So it composes** — the EML snippet
5. **Where it earns its place** — maintenance tools, migration cockpits,
   monitoring, one-off approvals
6. **Not a comparison** — RAP for published services, abap2UI5 for the screen in
   front. MIT, 7.02 to ABAP Cloud
