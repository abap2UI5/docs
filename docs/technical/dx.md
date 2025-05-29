# ABAP Thinking, UI5 Results

_abap2UI5 - A Developer-Centric Approach_

This framework was born from the everyday experience of ABAP developers. It tackles common challenges like deployment, caching, debugging, and tooling — while keeping the coding style close to familiar ABAP and SAP GUI patterns such as selection screens and ALV. The goal: to make working with abap2UI5 as familiar and intuitive as possible for ABAPers. This page takes a closer look at some of the key influences of the framework.
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

This simplicity also inspired the entry point for abap2UI5 apps. For a simple output you don't need more than this:
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

What abap2UI5 adds:
- Runs in the browser, no ADT needed
- With UI5 Frontend, its conform to SAP Fiori Design guidelines

### Input Handling Inspired by Selection Screens

Inputting data was never a big topic for ABAP developers, just create a report and `parameters` and `select-options`give you a UI fro free. Although this term was not used at that time, you get with 4 lines of code an app which today is called `fullstack` application:
```abap
REPORT zre_app_input.
  PARAMETERS pa_arbgb TYPE t100-arbgb DEFAULT 'MDG_TR'.
START-OF-SELECTION.
  MESSAGE |Input: { pa_arbgb }| type 'I'.
```
Why this is great for developers:
- Rapid prototyping with minimal code
- Built-in input functionaity and events
- Fullstack behavior with no setup
 
abap2UI5 mirrors this classic selection logic but browser based. Just use the class  `z2ui5_cl_xml_view` to create simple views and exchange data with the client via `_bind_edit` methods:
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
It is easy to test: reload the page, enter input, press the button while everything is still in a single class — no external UI tooling needed.

### Tabular Output like ALV

Table output is one of the most used features for ABAP developers, and using ALV for that is just iconic. `CL_SALV_TABLE` helped make tabular output effortless:

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

abap2UI5 creates outputs completely based an internal tables, no ddictionry artifacts are needed, no designtime definitions have to be made. just throw the actual table into the binding use a ui5 table control to display the data:

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
From here it is just a small step to make it even more dynamic and create everything with RTTI at runtime. Just how it is done with the newer (but still old now) SALV. While the ALV onbly works with sapgui, these snippets are the exttrem quick way to bring your table into your browser.


### Classic Popups, Modern Events

How often have you been to transaction `SE37` and search with `POPUP_TO*` for a popup matching wxactly what you need. yes function modules are old but still very often used and extremely practicable, giving you some event logic with just a serveal lines of code. Lets take a look the the `POPUP_TO_CONFIRM`:
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
* event logic at a central place
* easy to understand program flow
* popups are encapsulated in abap code and can be reused

abap2UI5 offers a matching approach giving you the chance to create multiple apps, which can call each other. Encapsulate your popup funcitonality in a separated class and call it whenever you want. Check these snippet mimicing the popup to confirm:
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
Browsr based roundtrip need a sligthly different program flow but the approach is still close and should feel familiar for ABAP developers.

### Zero-Setup Deployment

One aspect of Developer Experience is deployment. Even a beautifully written app is frustrating if it’s hard to ship. In abap2UI5, apps are just ABAP classes — deployment is as simple as activating the class. Transport to production happens via the standard TOC system known from traditional ABAP workflows.

Why this improves Developer Experience:
- No separate frontend build or deployment pipelines
- Code changes can be instantly tested by developers or consultants
- Every app is abapGit-compatible — no separate artifacts required
  
### No Caching Issues

A common frustration in SAP frontend development is UI caching — especially with BSP or Fiori Elements apps. You make a change, but nothing happens due to cached files. abap2UI5 avoids this problem entirely by not caching any UI definitions. The UI is dynamically generated on every request.

Why this improves Developer Experience:
- No need to clear browser or server caches
- Fast development iteration — edit the code, refresh the browser, see results

### Develop in Any ABAP IDE

Great Developer Experience means freedom of tooling. abap2UI5 apps are developed entirely in ABAP — whether in SE80, ADT, or your favorite ABAP IDE.

Why this improves Developer Experience:
- No additional setup required — works in any ABAP system
- Ideal for teams already experienced with ABAP

### Pure ABAP Debugging

Frontend-heavy applications often require jumping between browser dev tools, JavaScript logs, and network inspectors. With abap2UI5, the UI is pure ABAP — no JavaScript, no additional layers. Set a breakpoint in the ABAP method and you’re done.

Why this improves Developer Experience:
- Backend-only debugging with the classic debugger or ADT
- No need for browser debugging tools

### Easy Code Sharing

Sharing your apps or code is easy. Since abap2UI5 apps are ABAP classes, they can be shared via abapGit or simply copy-pasted. No configuration files, manifests, or build tools involved.

Why this improves Developer Experience:
- Easy to clone and test other apps
- Great for collaboration and code reviews
- Encourages modular, reusable components

### Summary

abap2UI5 adapts classic ABAP constructs for browser-based UI development. Concepts like WRITE, selection screens, ALV tables, and modal popups are reimplemented using UI5 technologies — but fully defined and executed in ABAP classes.

The framework requires no JavaScript, no OData, and no separate build tooling. Deployment consists of activating a class; the rendered UI is delivered dynamically on each request, avoiding frontend caching issues. abap2UI5 applications are abapGit-compatible, transportable via standard mechanisms, and can be developed using SE80, ADT, or any ABAP IDE.

The backend-only architecture simplifies development, accelerates iteration, and brings the UI closer to the ABAP developer’s core skill set.

Happy ABAPing! ❤️🦖🦕🦣
