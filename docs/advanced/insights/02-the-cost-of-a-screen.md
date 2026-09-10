# #2 The Cost of a Screen

An ABAP team needs a screen. A maintenance view for a customizing table nobody
wants to explain in SM30 again. A cockpit showing what last night's job
actually did. An approval step for one department. Some of these are used by
four people, twice a year. Some run during a go-live or a migration and are
never started again after that.

The logic behind such a screen might be thirty lines. The user interface in
front of it is not, and — this is the part that hurts — it does not scale down
with the logic: a data model, a service, a binding, an annotation model, a
frontend artifact, a deployment. Every one of those objects then exists
forever, unless somebody remembers to delete it.

None of that is waste. It is what makes a real application dependable. It is
simply a *fixed* cost, and a fixed cost is brutal to a small thing.

So the screen does not get built. Or it becomes a selection screen and an ALV
grid, and everyone agrees to stop thinking about it. Every system has a `Z`
package full of those, and every one of them was a reasonable decision at the
time.

Here is the job monitor as a UI5 app instead — not an excerpt, the whole
application:

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
on screen. About as much work as an ALV — except this one also starts on your
phone following the Fiori design guidelines.

Replace `model_init( )` with the `SELECT` that reads your job log, and it
is finished. Nothing published, nothing to deprecate, nothing anybody has to
un-build in three years.

abap2UI5 is not a replacement for your RAP or UI5 apps. It is an addition
to the UI solutions you already run — especially for small programs, and for
the screen somebody needs exactly once.

And it runs on the UI5 and the ABAP your system already has. Install it with
abapGit and give it a try!

Happy ABAPing! 🦖🦕🦣
