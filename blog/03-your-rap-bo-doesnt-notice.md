# Your RAP Business Object Doesn't Notice

*abap2UI5 Know-How #3 — draft*

[Last time](02-the-whole-contract.md): abap2UI5 asks an application for one
interface with one method, and for nothing else. Which raises the obvious
question — if the framework does not define a data model, where does the data
behind the screen come from?

From wherever you already keep it. The event handler is ordinary ABAP, so it
calls whatever the system offers: a `SELECT`, a function module, a class that
has existed since 2011, or a modern business object.

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

Nothing in that snippet is abap2UI5 except the two message calls. The business
object is untouched: its validations, determinations, authorizations and draft
handling all still run, because they sit behind EML and EML does not care who
makes the call.

The same holds in the other direction. The app is deployed as an ICF node, so
it registers in the Fiori launchpad next to the tiles that are already there —
and a user cannot tell it apart from them.

So the honest description is not "an alternative". It is one more consumer of
the logic you already have, with a screen in front of it that you wrote by
hand. What that costs is the next article.

Happy ABAPing! 🦖🦕🦣

---

## LinkedIn teaser post

Plain text — LinkedIn renders no markdown.

> If a UI framework defines no data model, where does the data behind the screen
> come from?
>
> From wherever you already keep it. An abap2UI5 event handler is ordinary ABAP,
> so it calls your RAP business object through EML like any other consumer — and
> the BO never learns that a different UI is in front of it. Its validations,
> determinations, authorizations and draft handling all still run, because they
> sit behind the call and the call does not care who makes it.
>
> The app deploys as an ICF node, so its tile sits in the launchpad next to the
> Fiori Elements ones, and users cannot tell them apart.
>
> New article 🎉
>
> Has anyone here put a hand-written screen in front of an existing RAP BO —
> and where did it get awkward?
>
> #ABAP #SAP #UI5
