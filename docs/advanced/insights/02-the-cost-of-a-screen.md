# #2 The Cost of a Screen

An ABAP team needs a screen. Not an application — a screen. A maintenance view
for a customizing table nobody wants to explain in SM30 again. A cockpit
showing what last night's job actually did. An approval step for one
department, used by four people, twice a week.

The logic behind such a screen is often thirty lines. The cost of putting a
user interface in front of those thirty lines is not, and — this is the part
that hurts — it does not scale down with them: a data model to declare, a
service to define, a binding, an annotation model, a frontend artifact, a
deployment. And at the end of it an object that now exists forever, has to be
transported, has to survive an upgrade, and one day has to be deprecated by
someone who never met the department that asked for it.

None of that is waste. It is what makes a real application dependable. It is
simply a *fixed* cost, and a fixed cost is brutal to a small thing.

So the screen does not get built. Or it becomes a selection screen and an ALV
grid, and everyone agrees to stop thinking about it. Every system has a `Z`
package full of those, and every one of them was a reasonable decision at the
time.

That is the gap this whole series is about. Here is the job monitor instead —
not an excerpt, not a snippet, the whole application:

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
                                    )->a( n = `state` v = `{STATE}` ).

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
on screen — a Fiori-styled table with colored status, in the launchpad theme,
on a phone if that is where you open it. Replace `model_init( )` with the
`SELECT` that reads your job log and it is finished. Nothing published, nothing
to deprecate, nothing anybody has to un-build in three years.

That is the trade in one sentence: **the ceremony is gone, and so is the
generosity.** You write the view by hand. Nothing generates it from
annotations, nothing hands you filter bars, variants or export for free. For
the screens above that is a rounding error — a table and two buttons. For a
list report with fifteen filters, sorting, personalization and an Excel export,
it is the wrong trade by a wide margin, and Fiori Elements will beat this by a
distance.

Knowing which of the two you are looking at is most of the skill. The rest of
this series is mostly about sharpening that judgment — starting with what is
actually behind such a screen, which the framework
[has no opinion about at all](/advanced/insights/03-your-favorite-programming-model).

Happy ABAPing! 🦖🦕🦣
