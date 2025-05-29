# ABAP Thinking, UI5 Results

_A Developer-Centric Approach_

If you’re an ABAP developer wondering how abap2UI5 fits into your day-to-day work, this article is for you. It shows how the framework brings familiar ABAP patterns — like WRITE, selection screens, and ALV — into modern browser-based UIs using nothing but backend code. You'll see how abap2UI5 streamlines the developer experience by avoiding boilerplate, external tools, or JavaScript. This page is all about how abap2UI5 feels like classic ABAP development — just with UI5 results.

### Simple Output with if_oo_adt_classrun

One of the most fundamental development tasks is outputting data. In ABAP, the cleanest way to do this is with the `if_oo_adt_classrun` interface. It offers a simple, class-based approach to immediately output data in ADT:

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

Why it matters:
- Single-class design is easy to debug and version
- Minimal boilerplate, cloud-compatible
- Fully abapGit-ready

This simplicity inspired the entry point for abap2UI5 apps:
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

Additional in abap2UI5:
- Runs in the browser, no ADT needed
- End-user ready without extra deployment
- Conform to SAP Fiori Design guidelines

### Input Handling Inspired by Selection Screens

Classic ABAP supports user input through selection screens:

```abap
REPORT zre_app_input.
  PARAMETERS pa_arbgb TYPE t100-arbgb DEFAULT 'MDG_TR'.
START-OF-SELECTION.
  MESSAGE |Input: { pa_arbgb }| type 'I'.
```
Why it matters:
- Rapid prototyping with minimal code
- Built-in input validation and events
- Fullstack behavior with no setup

abap2UI5 brings this idea into the browser using an XML view builder and data binding:
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
Why this improves Developer Experience:
- Mirrors classic ABAP selection logic, making it familiar
- Easy to test: reload the page, enter input, press the button
- Everything is still in a single class — no external UI tooling needed

### Tabular Output like ALV

ALV tools such as `CL_SALV_TABLE` helped make tabular output effortless:

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

Why it matters:
- Generates full UI from internal tables
- No external annotations or CDS needed
- Ideal for admin tools and quick overviews

abap2UI5 brings this to the browser:

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
From here to make an outing output with salv is a small step  to make rtti salv like

Additional Benefits in abap2UI5:
- Fully works in browser and on any device, no SAP GUI dependencies

### Classic Popups, Modern Events
Classic ABAP offered a straightforward way to ask user decisions:

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
Benefits:
* event logic at a central place
* easy to understand program flow
* popups are encapsulated in abap code and can be reused

abap2UI5 offers a matching approach using z2ui5_cl_pop_to_confirm:
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

Why it matters:
- Dialog logic stays class-based and readable
- UI and logic stay in sync
- The flow mimics classic ABAP screen logic with modern UI5 behavior

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
