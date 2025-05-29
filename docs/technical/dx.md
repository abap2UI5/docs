# ABAP Thinking, UI5 Results

_abap2UI5 - A Developer-Centric Approach_

abap2UI5 was born from the everyday experience of ABAP developers. It tackles common challenges like deployment, caching, debugging, and tooling — while keeping the coding style close to familiar ABAP and SAP GUI patterns such as selection screens and ALV. The goal: to make working with abap2UI5 as familiar and intuitive as possible for ABAPers. This page takes a closer look at some of the key influences of the framework.
´
### Simple Output with if_oo_adt_classrun

One of the most fundamental development tasks is outputting data. In ABAP, the quickest way to do this is with the `if_oo_adt_classrun` interface. It provides a simple, class-based entry point for outputting data directly in ADT:

```abap
CLASS zcl_app_adt DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.

CLASS zcl_app_adt IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    out->write( `Hello World` ).
  ENDMETHOD.
ENDCLASS.
```

Why this is great for developers:
- Single-class design: quick to create and easy to debug
- Minimal boilerplate
- Fully abapGit-ready

This simplicity also inspired the entry point for abap2UI5 apps. For simple output, you don't need more than this:
```abap
CLASS zcl_app_ui5 DEFINITION PUBLIC CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_ui5 IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    client->message_toast_display( `Hello World` ) ).
  ENDMETHOD.
ENDCLASS.
```
What abap2UI5 adds is the ability to run in the browser without ADT, using a UI5 frontend that fully adheres to SAP Fiori design guidelines — and is ready to be shown to your consultant colleagues right away.

### Input Handling Inspired by Selection Screens

Input handling was never a challenge in classic ABAP — just define a REPORT with PARAMETERS and SELECT-OPTIONS, and the UI is generated automatically. Although the term didn’t exist back then, this gave you a "fullstack" app in just a few lines:

```abap
REPORT zre_app_input.
  PARAMETERS pa_arbgb TYPE t100-arbgb DEFAULT 'MDG_TR'.
START-OF-SELECTION.
  MESSAGE |Input: { pa_arbgb }| type 'I'.
```
Why this is great for developers:
- Rapid prototyping with minimal code
- Built-in input handling and event processing
- Fullstack behavior with no setup
 
abap2UI5 mirrors this classic selection screen behavior in the browser. Use Z2UI5_CL_XML_VIEW to define simple views and exchange data with the client using _bind_edit methods:

```abap
CLASS zcl_app_input DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA pa_arbgb TYPE string VALUE 'MDG_TR'.
ENDCLASS.

CLASS zcl_app_input IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
      client->view_display(
        z2ui5_cl_xml_view=>factory(
            )->input( client->_bind_edit( pa_arbgb )
            )->button( text  = 'post' press = client->_event( 'POST' ) ) ).
      RETURN.
    ENDIF.

    client->message_box_display( |Input: { pa_arbgb }| ).

  ENDMETHOD.
ENDCLASS.
```
Easy to test: reload the page, enter input, and press the button — all within a single class and without any external UI tooling.

### Table Output like ALV

Table output is one of the most commonly used features by ABAP developers, and ALV is an iconic tool in this context. CL_SALV_TABLE makes generating tabular output straightforward:

```abap
REPORT zre_app_alv.

SELECT FROM t100
 FIELDS *
 INTO TABLE @DATA(gt_t100)
 UP TO 10 ROWS.

cl_salv_table=>factory(
  IMPORTING
    r_salv_table   = DATA(go_salv)
  CHANGING
    t_table        = gt_t100 ).
go_salv->display( ).
```

Why this is great for developers:
- Generates full UI from internal tables
- No external annotations or CDS needed
- Ideal for admin tools and quick overviews

abap2UI5 follows a similar pattern. Just bind the internal table to a UI5 table control — no dictionary artifacts or design-time definitions needed:

```abap
CLASS zcl_app_alv DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_alv IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    SELECT FROM t100
     FIELDS *
     INTO TABLE @DATA(gt_t100)
     UP TO 10 ROWS.

    DATA(tab) = z2ui5_cl_xml_view=>factory(
        )->table( client->_bind_local( gt_t100 ) ).

    DATA(lo_col) = tab->columns( ).
    lo_col->column( )->text( `ARBGB` ).
    lo_col->column( )->text( `MSGNR` ).
    lo_col->column( )->text( `TEXT`  ).

    DATA(lo_cell) = tab->items( )->column_list_item( ).
    lo_cell->text( `{ARBGB}` ).
    lo_cell->text( `{MSGNR}` ).
    lo_cell->text( `{TEXT}` ).

    client->view_display( tab ).

  ENDMETHOD.
ENDCLASS.
```

From here, it's just a small step to generate everything dynamically using RTTI — similar to modern SALV techniques (but still old now). And unlike SALV, this runs in the browser.


### Classic Popups, Modern Events

Anyone who has browsed SE37 for POPUP_TO_* knows the charm of classic ABAP popups. Function modules like POPUP_TO_CONFIRM are simple yet powerful:

```abap
REPORT zre_app_alv.

DATA event TYPE string.
CALL FUNCTION 'POPUP_TO_CONFIRM'
  EXPORTING
    titlebar      = 'Title'
    text_question = 'Do you like dinosaurs?'
  IMPORTING
    answer        = event.

CASE event.
  WHEN '1'.
    MESSAGE `the result is YES` TYPE 'I'.
  WHEN '2'.
    MESSAGE `the result is NO` TYPE 'I'.
ENDCASE.
```
Why this is great for developers:
* Centralized event logic
* Simple and readable program flow
* Fully encapsulated and reusable

abap2UI5 provides a similar experience, enabling apps to call each other and encapsulate dialog logic in reusable classes:

```abap
CLASS zcl_app_alv_event DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_alv_event IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
      client->nav_app_call( z2ui5_cl_pop_to_confirm=>factory(
        i_question_text = 'Do you like dinosaurs?'
        i_title         = `Title`
        i_event_confirm = `YES`
        i_event_cancel  = `NO` ) ).
      RETURN.
    ENDIF.

    CASE client->get( )-event.
      WHEN `YES`.
        client->message_box_display( `the result is YES` ).
      WHEN `NO`.
        client->message_box_display( `the result is NO` ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

While browser-based roundtrips require slightly different flow control, the overall approach remains intuitive for ABAP developers.

### More

Beyond the code snippets shown above, the Over-the-Wire approach of abap2UI5 — based entirely on backend development — offers several additional advantages.

##### Zero-Setup Deployment

In abap2UI5, apps are just ABAP classes — deployment is as simple as activating the class. Transport to production happens via the standard transport system familiar from traditional ABAP development:

- No separate frontend build or deployment pipelines  
- Code changes can be instantly tested by developers or consultants  
- Every app is abapGit-compatible — no separate artifacts required  

##### No Caching Issues

A common frustration in frontend development is caching — especially with BSP or Fiori Elements apps. You make a change, but nothing happens due to cached files. abap2UI5 avoids this problem entirely, as the UI is dynamically generated on every request:

- No need to clear browser or server caches  
- Fast development iteration — edit the code, refresh the browser, see results  

##### Develop in Any ABAP IDE

There's often debate about which IDE to use — but why not let everyone choose the tool they prefer? abap2UI5 apps are developed entirely in ABAP. You can work in ADT, rely on the time-tested SE80, or even explore integration with VS Code:

- No additional setup required — works in any IDE  
- Ideal for teams with different tooling preferences

##### Pure ABAP Debugging

Frontend-heavy applications often require switching between browser dev tools, JavaScript logs, and network inspectors. With abap2UI5, the UI is pure ABAP — no JavaScript, no additional layers. Just set a breakpoint in your ABAP method:

- Backend-only debugging with the classic SE80 debugger or ADT
- No need for browser debugging tools

##### Easy Code Sharing

Sharing your apps or code is simple. Since abap2UI5 apps are standard ABAP classes, they can be shared via abapGit or even copy-pasted. No configuration files, manifests, or build tools required:

- Easy to clone and test other apps
- Great for collaboration and code reviews
- Encourages modular, reusable components

### Summary

abap2UI5 adapts classic ABAP constructs for browser-based UI development. Concepts like WRITE, selection screens, ALV tables, and modal popups are reimplemented using UI5 technologies — but fully defined and executed in ABAP classes.

The framework requires no JavaScript, no OData, and no separate build tooling. Deployment consists of activating a class; the rendered UI is delivered dynamically on each request, avoiding frontend caching issues. abap2UI5 applications are abapGit-compatible, transportable via standard mechanisms, and can be developed using SE80, ADT, or any ABAP IDE.

The backend-only architecture simplifies development, accelerates iteration, and brings the UI closer to the ABAP developer’s core skill set.

Happy ABAPing! ❤️🦖🦕🦣
