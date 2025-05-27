# Developer Experience: Keeping It Simple

Developer Experience describes how efficient, and intuitive it is for developers to work with tools, frameworks, and systems.

Good Developer Experience means writing code feels natural, testing changes is fast, and the tools stay out of your way. It includes minimal boilerplate, easy debugging, straightforward deployment, and code that’s simple to understand, reuse, and share.

This page explores familiar ABAP technologies and coding patterns that offer a smooth developer experience — and shows how these ideas have influenced the design of abap2UI5.

### Output - if_oo_adt_classrun

One of the most fundamental tasks in any development environment is to output data. In ABAP, the cleanest and cloud-compatible way to do this is via the IF_OO_ADT_CLASSRUN interface. It provides a simple, class-based entry point that works in both ABAP Cloud and on-premise systems.

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
Why this improves Developer Experience:
- The entire app lives in a single file — fast to navigate, easy to debug
- No boilerplate: just a class and one method
- Works identically in all modern ABAP environments
- Integrates perfectly with abapGit for versioning and sharing

This simple design directly inspired abap2UI5. Here's the equivalent app implemented in abap2UI5:

```abap
CLASS zcl_app_ui5 DEFINITION PUBLIC CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_ui5 IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    client->view_display( z2ui5_cl_xml_view=>factory( )->text( `Hello World` ) ).
  ENDMETHOD.
ENDCLASS.
```
Additional DX Benefits in abap2UI5:
- Runs in any browser — no ADT installation required
- Accessible by end users with proper authorization
- Output uses UI5 components that conform to SAP Fiori guidelines

This example sets the tone for the rest of the framework: minimal setup, full backend control, and great compatibility.

### Input - Selection Screen

A key part of developer productivity is how quickly one can gather user input. Traditional ABAP offers a dead-simple approach with selection screens:

```abap
REPORT zre_app_input.
  PARAMETERS pa_arbgb TYPE t100-arbgb DEFAULT 'MDG_TR'.
START-OF-SELECTION.
  MESSAGE |Input: { pa_arbgb }| type 'I'.
```
abap2UI5 brings this idea into the browser with a view-based UI:
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
    ELSEIF client->get( )-event = 'POST'.
      client->message_box_display( |Input: { pa_arbgb }| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
```
Why this improves Developer Experience:
- Mirrors classic ABAP selection logic, making it familiar
- Easy to test: reload the page, enter input, press the button
- Everything is still in a single class — no external UI tooling needed


### Output Tables - ABAP List Viewer

In classical ABAP, CL_SALV_TABLE was a major step forward in developer productivity:
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
Fifteen lines, a single file, and ready for productive use. Unfortunately, it's not cloud-ready. abap2UI5 offers a functionally similar alternative that’s cloud-ready:
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
Why this improves Developer Experience:
- Data binding is straightforward and localized
- Table layout and content are configured inline
- Fully works in browser and on any device, no GUI dependencies

### popup_to_confirm
Classical ABAP uses POPUP_TO_CONFIRM to ask users for simple confirmations:
```abap
REPORT zre_app_alv.

DATA event TYPE string.
CALL FUNCTION 'POPUP_TO_CONFIRM'
  EXPORTING
    titlebar      = 'Title'
    text_question = 'Do you have a great day?'
  IMPORTING
    answer        = event.

CASE event.
  WHEN '1'.
    MESSAGE `the result is YES` TYPE 'I'.
  WHEN '2'.
    MESSAGE `the result is NO` TYPE 'I'.
ENDCASE.
```
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
        i_question_text = `Do you have a great day?`
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

Why this improves Developer Experience:
- Dialog logic stays class-based and readable
- UI and logic stay in sync
- The flow mimics classic ABAP screen logic with modern UI5 behavior


### Deployment – Transport Without Pain

One of the most overlooked aspects of Developer Experience is deployment. Even a beautifully written app is frustrating to work with if it’s hard to deploy.

Classic ABAP deployments often rely on transport requests and manual steps through SE01, which can become tedious and error-prone — especially for small, iterative apps.

**abap2UI5 improves this by being fully abapGit-compatible.** All apps are just classes and can be committed, versioned, and transported via Git-based workflows. This is fully supported in both ABAP Cloud and classic systems.

**Why this improves Developer Experience:**
- No Z-transaction creation, Web Dynpro configuration, or ICF setup required
- Code changes are instantly visible via browser without deployment steps
- Clean Git history, easier code reviews, and fully scriptable CI/CD possible

### Cache – Reliable During Iteration

A common frustration in SAP development is the UI cache, especially when working with BSP applications or Fiori Elements apps. You make a change — and nothing happens due to cached artifacts.

**abap2UI5 eliminates this problem** by not caching UI definitions at all. The UI is dynamically built in each request from ABAP code. Every refresh reflects the latest code.

**Why this improves Developer Experience:**
- No need to clear browser cache or invalidate server caches
- True WYSIWYG iteration — edit the code, refresh, and test
- Enables rapid experimentation without hidden state issues

### Tools – You Already Know Them

Great DX means you don’t have to learn new tools just to be productive.

With abap2UI5, development happens entirely within **SE80**, **ADT**, or any **ABAP IDE** of your choice. No Node.js, no Web IDE, no special CLI tools.

**Why this improves Developer Experience:**
- Works out of the box in any ABAP system — no extra setup
- Familiar tooling: SE80, class editor, debugger
- Smooth transition for teams already comfortable with ABAP


### Debugging – It Just Works

Debugging frontend-heavy applications often requires navigating between browser tools, JavaScript breakpoints, and network logs — not ideal for ABAP developers.

In **abap2UI5**, the UI is just a projection of ABAP code. There is no JavaScript to debug. If you want to see what happens when a button is pressed, set a breakpoint in the ABAP class — that’s it.

**Why this improves Developer Experience:**
- Backend-only debugging using the classic ABAP debugger
- No browser dev tools needed
- Reproducible, testable flows — event handlers run like regular ABAP methods

---

### Sharing – Built for abapGit

Sharing solutions across systems or with other developers is essential — especially in open source or multi-system landscapes.

Since **abap2UI5 apps are just classes**, they are naturally compatible with **abapGit**. There are no special configuration files, manifests, or bundling steps.

**Why this improves Developer Experience:**
- Easy to clone and test others’ apps
- Ideal for team collaboration and code review
- Encourages modular, reusable components through shared repositories


### Summary

abap2UI5 brings back the simplicity of classic ABAP workflows with a modern, browser-based UI. By preserving familiar ABAP patterns (like output via WRITE, selection screens, ALV display, and popups) and wrapping them in clean, class-based interfaces, abap2UI5 delivers a developer experience that is:
- Fast to start — apps are created in minutes
- Simple to debug — logic and UI are together
- Easy to share — via abapGit and browser-based access
- Cloud-compliant — works in Steampunk and on-prem

Each code snippet on this page shows how classic patterns are reimagined for modern ABAP development. That’s what great Developer Experience is about.
