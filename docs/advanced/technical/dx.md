# Developer Experience: Keeping Things Simple

Developer Experience refers to how intuitive and productive it feels for developers to work with a framework and its tooling.

Good Developer Experienc means writing code is fast, testing changes is instant, and the tooling never gets in your way. It involves minimal boilerplate, easy debugging, simple deployment, and code that’s easy to understand, reuse, and share.

This page explores familiar ABAP frameworks and and snippets serving a nice developer experience — and shows how these ideas have influenced APIs and its usage of abap2UI5.

### API I: Output with if_oo_adt_classrun

One of the most fundamental development tasks is outputting data. In ABAP, the cleanest way to do this is with the IF_OO_ADT_CLASSRUN interface. It offers a simple, class-based approach immediatly outputting data into ADT:

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
- Works identically in `ABAP Cloud` and `Standard ABAP`
- abapGit compatible for versioning and sharing

This simple design inspired abap2UI5. Here's the equivalent functionality implemented in abap2UI5:
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

Additional Benefits in abap2UI5:
- Runs in any browser — no ADT installation required
- Accessible by end users
- Output uses UI5 beeing conform to SAP Fiori Design guidelines


### API II: Input with Selection Screen

Traditional ABAP offers a fast way to collect user input using selection screens:

```abap
REPORT zre_app_input.
  PARAMETERS pa_arbgb TYPE t100-arbgb DEFAULT 'MDG_TR'.
START-OF-SELECTION.
  MESSAGE |Input: { pa_arbgb }| type 'I'.
```
abap2UI5 brings this concept into the browser:
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

### API III: Output Tables with ABAP List Viewer

CL_SALV_TABLE brought major productivity gains to classic ABAP:

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
While this is compact and effective, it's not cloud-ready. abap2UI5 provides a browser-based, cloud-compatible alternative:
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
- Fully works in browser and on any device, no SAP GUI dependencies

### API IV: popup_to_confirm
Classic ABAP uses `POPUP_TO_CONFIRM` for simple decisions:

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


### Deployment

One overlooked aspects of Developer Experience is deployment. Even a beautifully written app is frustrating to work with if it’s hard to deploy. But in abap2UI5 all apps are just classes and can be deployed via activation, transport into production goes over the TOC system known from the rest of the abap world.

Why this improves Developer Experience:
- No separated Frontend deployment and build pipelines
- Code changes are instantly deployed and can be tested by consultants
- Every app is an abapGit fully compatible Project without separated frontend artifacts
  
### Cache

A common frustration in SAP development is the UI cache, especially when working with BSP applications or Fiori Elements apps. There are a lot of different cahe incalvidation transactions etc. You make a change — and nothing happens due to cached artifacts. abap2UI5 eliminates this problem by not caching UI definitions at all. The UI is dynamically built in each request from ABAP code. Every browser refresh always reflects the latest code.

Why this improves Developer Experience:
- No need to clear browser cache or invalidate server caches
- Fast dewvelopment iteration — edit the code, refresh, and test

### Tools

Great Developer Experience also means you can use the tools you like and are not forced into certain tooling. With abap2UI5, development happens entirely within **SE80**, **ADT**, or any IDE of your choice. It only based on ABAP classes giving you the freedom of choice and has a complete compatibility to all tools.

Why this improves Developer Experience:
- Works out of the box in any ABAP system — no extra setup
- Smooth transition for teams already comfortable with ABAP

### Debugging

Debugging frontend-heavy applications often requires navigating between browser tools, JavaScript breakpoints, and network logs — not ideal for ABAP developers. In abap2UI5, the UI is comes fully out of ABAP code. There is no JavaScript to debug. If you want to see what happens when a button is pressed, set a breakpoint in the ABAP class — that’s it.

Why this improves Developer Experience:
- Backend-only debugging using the classic ABAP debugger or ADT
- No browser dev tools needed

### Sharing

Sharing solutions across systems or with other developers is essential — especially in open source or multi-system landscapes. Since abap2UI5 apps are just classes, they are naturally compatible with abapGit or can be just copy & pasted. There are no special configuration files, manifests, or bundling steps.

Why this improves Developer Experience:
- Easy to clone and test others’ apps
- Ideal for team collaboration and code review
- Encourages modular, reusable components through shared repositories

### Summary

abap2UI5 was a lot inspired by the classics like output via WRITE, selection screens, ALV display, and popups and for the rest it got very lucky that the ui5 framework can that easy be gesteuert with abap only leveringing a lot of additonal benefits like Deployement, cahce, debugging, tooling or sharing giving abap2UI5 a smooth Developer expereince.
