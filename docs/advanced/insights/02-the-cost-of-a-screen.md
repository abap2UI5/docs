---
description: The smallest complete abap2UI5 app - one class, one interface, one URL parameter - and what has to exist beside it in the system, which is nothing.
---
# #2 The Cost of a Screen

What does a screen cost in artifacts? For a UI5 app on an ABAP stack the
answer is usually a list: a data model, a service, a binding, an annotation
model, a frontend artifact, a deployment. Each of those objects is named,
transported and reviewed, and each belongs to exactly one application. The
list does not shrink when the logic behind the screen is thirty lines.

Here is what the list looks like for abap2UI5. A job monitor, not an excerpt
but the whole application:

```abap
CLASS zcl_sample_job_monitor DEFINITION PUBLIC.

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


CLASS zcl_sample_job_monitor IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_navigated( ).
      model_init( ).
      set_view( ).
    ENDIF.

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

Activate it, call the ICF endpoint with `?app_start=zcl_sample_job_monitor`,
and it is on screen, laid out by the UI5 controls it names, on a phone as well
as on a desktop.

Three things in the class carry the whole mechanism, and the rest of the
series takes them apart one at a time. `z2ui5_if_app` is the one interface
the framework asks for, with one method — [#3](/advanced/insights/03-one-class-no-programming-model).
`check_on_navigated( )` is true on the first call and on every return from
another app, so the view is built exactly when the browser needs one —
[#16](/advanced/insights/16-one-click-one-request). `_bind( runs )` hands the
attribute to the view by reference; what the table shows is the internal
table, and what comes back from the browser lands in the same attribute —
[#15](/advanced/insights/15-where-the-selection-screen-went).

Replace `model_init( )` with the `SELECT` that reads your job log and the
class is finished. Nothing is published beside it: no service, no annotation,
no frontend artifact, no deployment step. The class travels in a transport
like any other, and deleting it deletes the application.

That is the technical side of the cost. What it means for which screens get
built at all — the fixed cost that a small requirement cannot amortize — is
not a question for a developer manual; the case is made on
[What it costs](https://abap2ui5.github.io/why/#what-it-costs){target="_self"}.
