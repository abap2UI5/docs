# ABAP Thinking, UI5 Results

_A Developer-Centric Approach_

abap2UI5 was built from the everyday experiences of ABAP developers. It tackles common challenges in the development process — deployment, caching, debugging, and tooling — while keeping a coding style that's close to familiar ABAP and SAP GUI patterns like Selection Screens and ALV. The goal: make working with abap2UI5 as familiar and intuitive as possible for ABAPers. This page takes a closer look at the key influences behind the framework.

### Simple Output with IF_OO_ADT_CLASSRUN

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
Benefits:
- Single-class design: quick to create and easy to debug
- runs out of the box
- Minimal boilerplate, easy to copy&paste

This simplicity also inspired the entry point for abap2UI5 apps. For simple output, you don't need more than this:
```abap
CLASS zcl_app_ui5 DEFINITION PUBLIC CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_ui5 IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    client->message_toast_display( `Hello World` ).
  ENDMETHOD.
ENDCLASS.
```
What abap2UI5 adds: the ability to run in the browser without ADT, using a UI5 frontend that fully follows SAP Fiori design guidelines — ready to show your colleagues right away.

###  Classic Input Handling with Selection Screens

Input handling was never a challenge in classic ABAP — just define a Report with `PARAMETERS` and `SELECT-OPTIONS`, and the UI gets generated automatically. The term didn't exist back then, but this gave you a "fullstack" app in just a few lines:

```abap
REPORT zre_app_input.
  PARAMETERS pa_arbgb TYPE t100-arbgb DEFAULT 'MDG_TR'.
START-OF-SELECTION.
  MESSAGE |Input: { pa_arbgb }| type 'I'.
```
Benefits:
- Rapid prototyping with minimal code
- Built-in input handling and event processing
- Fullstack behavior with no setup
 
abap2UI5 mirrors this classic selection screen behavior in the browser. Use `Z2UI5_CL_XML_VIEW` to define simple views and exchange data with the `_bind_edit` method:

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
Plus, you can test code changes instantly — just reload the page, enter input, and press the button — all within a single class, no external UI tooling needed.

### ALV-Style Table Output in the Browser

Table output is one of the most commonly used features by ABAP developers, and ALV is an iconic tool in this context. `CL_SALV_TABLE` makes generating tabular output straightforward:

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

Benefits:
- Generates a full UI from internal tables
- No external annotations, CDS views, or DDIC artifacts required
- Ideal for admin tools and quick overviews

abap2UI5 follows a similar pattern. Just bind the internal table to a UI5 table control:

```abap
CLASS zcl_app_alv DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA gt_t100 TYPE STANDARD TABLE OF t100 WITH EMPTY KEY.
ENDCLASS.

CLASS zcl_app_alv IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    SELECT FROM t100
     FIELDS *
     INTO TABLE @gt_t100
     UP TO 10 ROWS.

    DATA(tab) = z2ui5_cl_xml_view=>factory(
        )->table( client->_bind( gt_t100 ) ).

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

From here, it's just a small step to generate everything dynamically with RTTI — similar to SALV functionality, but running directly in the browser.

### Classic Popups, Modern Events

Anyone who has browsed SE37 for POPUP_TO_* knows the charm of classic ABAP popups. Function modules like `POPUP_TO_CONFIRM` are simple yet powerful:

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

Browser-based roundtrips need slightly different flow control, but the overall approach stays intuitive for ABAP developers.

### More Developer-Friendly Advantages

Beyond the code snippets above, the Over-the-Wire approach of abap2UI5 — based entirely on backend development — brings several additional advantages.

##### Zero-Setup Deployment

In abap2UI5, apps are just ABAP classes — deployment is as simple as activating the class. Transport to production happens via the standard transport system you know from traditional ABAP development:
- No separate frontend build or deployment pipelines  
- Code changes can be instantly tested by other developers or consultants
- Every app is abapGit-compatible — no separate frontend artifacts required  

##### No Caching Issues

A common frustration in frontend development: caching — especially with BSP or Fiori Elements apps. You make a change, but nothing happens due to cached files, unless you manually trigger several cache invalidation transactions in the SAP backend. abap2UI5 sidesteps this entirely, as the UI gets dynamically generated on every request:
- No need to clear browser or server caches  
- Fast development iteration — edit the code, refresh the browser, see results  

##### Develop in Any IDE

There's often debate about which IDE to use — but why not let everyone pick their favorite? abap2UI5 apps are developed entirely in ABAP. Work in ADT, SE80, or even explore VS Code integration:
- No additional setup required — works in any IDE
- Ideal for teams with different tooling preferences

##### Pure ABAP Debugging

Frontend-heavy apps often need switching between browser dev tools, JavaScript logs, and network inspectors. With abap2UI5, the UI is pure ABAP — no JavaScript, no additional layers. Just set a breakpoint in your ABAP method:
- Backend-only debugging with the classic SE80 debugger or ADT
- No need for browser debugging tools

##### Easy Code Sharing

Sharing your apps or code is simple. Since abap2UI5 apps are standard ABAP classes, they can be shared via abapGit, transports or even copy-pasted:
- Easy to clone and test other apps
- Great for collaboration and code reviews
- Encourages modular, reusable components

### Conclusion

abap2UI5 brings back familiar ABAP development patterns. It reuses proven concepts like Selection Screens, ALV tables, and popup dialogs — but renders them as UI5 apps, all defined and executed in ABAP.
The result: backend-driven UI development with minimal tooling and fast iterations. Whether you're prototyping or building full business apps, abap2UI5 keeps development close to what ABAP developers know best.

Happy ABAPing! ❤️🦖🦕🦣
