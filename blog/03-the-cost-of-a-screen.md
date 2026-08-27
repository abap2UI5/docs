# The Cost of a Screen

*abap2UI5 Know-How #3 — draft*

> **This article and #2 answer different questions.**
> [#2, *abap2UI5 is not a Programming Model*](02-not-a-programming-model.md),
> is structural: what does abap2UI5 ask of your architecture? This one is
> economic: why do so many small screens never get built at all? You can read
> either without the other.

An ABAP team needs a screen. Not an application — a screen. A maintenance view
for a customising table nobody wants to explain in SM30. A cockpit that shows
what last night's job actually did. An approval step for one department. A test
harness for three people.

The business logic behind such a screen is often thirty lines. The cost of
putting a user interface in front of those thirty lines is not, and it does not
scale down with them.

## What a screen costs

Count what stands between the thirty lines and a user, on the standard path: a
data model to declare, a service to define, a binding to create, an
annotation model to maintain, a frontend artefact to build, a deployment to
run, and — the item nobody prices in — an object that now exists forever, has
to be transported, has to survive an upgrade, and one day has to be
deprecated by someone who has never heard of the department that asked for it.

None of that is waste. It is exactly what makes a real application dependable,
and for a real application it is worth every step. It is simply a fixed cost,
and a fixed cost is brutal to a small thing.

So the screen does not get built. Or it gets built as a selection screen and an
ALV grid, and everyone agrees to stop thinking about it. Every system has a
`Z` package full of those.

## What it costs the other way

Here is the job monitor. Not an excerpt — the whole application:

```abap
CLASS zcl_job_monitor DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_run,
        job    TYPE string,
        finish TYPE string,
        state  TYPE string,
      END OF ty_s_run.

    DATA runs TYPE STANDARD TABLE OF ty_s_run WITH EMPTY KEY.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.
    METHODS set_view.
    METHODS model_init.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_job_monitor IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_init( ).
      model_init( ).
    ENDIF.
    set_view( ).

  ENDMETHOD.

  METHOD set_view.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->a( n = `title` v = `Last night`

                )->ele( `Table`
                    )->a( n = `items` v = client->_bind( runs )

                    )->ele( `columns`
                        )->ele( `Column`
                            )->tag( `Text`
                                )->a( n = `text` v = `Job`
                        )->end(
                        )->ele( `Column`
                            )->tag( `Text`
                                )->a( n = `text` v = `Finished`
                        )->end(
                        )->ele( `Column`
                            )->tag( `Text`
                                )->a( n = `text` v = `State`
                        )->end(
                    )->end(

                    )->ele( `items`
                        )->ele( `ColumnListItem`
                            )->ele( `cells`
                                )->tag( `Text`
                                    )->a( n = `text` v = `{JOB}`
                                )->tag( `Text`
                                    )->a( n = `text` v = `{FINISH}`
                                )->tag( `ObjectStatus`
                                    )->a( n = `text`  v = `{STATE}`
                                    )->a( n = `state` v = `{STATE}` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD model_init.
    runs = VALUE #( ( job = `ZFI_POSTING`  finish = `03:14`  state = `Success` )
                    ( job = `ZMM_REORG`    finish = `03:41`  state = `Warning` )
                    ( job = `ZSD_INVOICES` finish = `04:02`  state = `Error` ) ).
  ENDMETHOD.

ENDCLASS.
```

Activate it, call the ICF endpoint with `?app_start=zcl_job_monitor`, and it is
on screen. No service, no binding, no frontend artefact, no deployment step,
nothing published, and nothing to deprecate later. Replace `model_init( )` with
the `SELECT` that reads your job log and it is finished.

That is the whole argument: the price finally scales down to the size of the
problem.

## The part that shows up two years later

The less obvious gain is what happens when such a tool grows up.

Because none of the business logic lives in the UI layer — the class draws a
screen and nothing else — the part worth keeping is already sitting where it
belongs. If the job monitor turns out to matter enough to deserve a real
service, the `SELECT` and the rules around it move, and the throwaway screen is
the only thing thrown away.

A cheap screen is not the same as a cheap decision. This one is cheap to
reverse, which is the property that actually matters for a tool nobody is sure
about yet.

## What you pay instead

You write the view by hand. Nothing generates it from annotations, nothing
lays out a list report for you, nothing gives you filter bars, variants,
personalisation or export for free.

For the four screens at the top of this article, that is a rounding error —
they are a table and two buttons. For a list-report application with fifteen
filters, it is the wrong trade by a wide margin, and Fiori Elements will beat
this by a distance. Knowing which of the two you are looking at is most of the
skill.

abap2UI5 is open source and installs on any ABAP release from 7.02 up to ABAP
Cloud, so finding out what a screen costs you takes an installation and a
class.

Happy ABAPing! 🦖🦕🦣

---

## LinkedIn teaser post

> A maintenance view for a customising table. A cockpit showing what last
> night's job did. An approval step for one department.
>
> The logic behind each is about thirty lines. The cost of putting a UI in
> front of thirty lines is not thirty lines, and it does not scale down: a data
> model, a service, a binding, a frontend artefact, a deployment — and an
> object that has to be transported, survive upgrades, and one day be
> deprecated by someone who never met the department that asked for it.
>
> So the screen never gets built. Every system has a Z package full of the ones
> that became a selection screen and an ALV grid instead.
>
> New article 🎉 with a job monitor as a complete abap2UI5 app — and an honest
> note on where this is the wrong trade.
>
> Which screen in your system stayed an ALV grid because a proper UI was never
> worth the effort?
>
> #ABAP #SAP #UI5
