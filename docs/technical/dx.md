---
outline: [2, 4]
---
# ABAP Thinking, UI5 Results

_A Developer-Centric Approach_

abap2UI5 grew out of the everyday experiences of ABAP developers. It tackles common challenges in the development process — deployment, caching, debugging, and tooling — while keeping a coding style close to familiar ABAP and SAP GUI patterns like Selection Screens and ALV. The goal: make working with abap2UI5 as familiar and intuitive as possible for ABAPers. This page takes a closer look at the key influences behind the framework.

### Simple Output with IF_OO_ADT_CLASSRUN

One of the most fundamental development tasks is outputting data. In ABAP, the quickest way is the `if_oo_adt_classrun` interface — a simple, class-based entry point for outputting data directly in ADT:

```abap
CLASS zcl_app_adt DEFINITION PUBLIC.
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
- Single-class design — quick to create and easy to debug
- Runs out of the box
- Minimal boilerplate, easy to copy and paste

This simplicity also inspired the entry point for abap2UI5 apps. For simple output, you don't need more than this:
```abap
CLASS zcl_app_ui5 DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_ui5 IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    client->message_toast_display( `Hello World` ).
  ENDMETHOD.
ENDCLASS.
```
What abap2UI5 adds: the ability to run in the browser without ADT, through a UI5 frontend that fully follows SAP Fiori design guidelines — ready to share with your colleagues right away.

### Classic Input Handling with Selection Screens

Input handling was never a challenge in classic ABAP — just define a Report with `PARAMETERS` and `SELECT-OPTIONS`, and the UI generates automatically. The term didn't exist back then, but this gave you a "full-stack" app in just a few lines:

```abap
REPORT zre_app_input.
  PARAMETERS pa_arbgb TYPE t100-arbgb DEFAULT `MDG_TR`.
START-OF-SELECTION.
  MESSAGE |Input: { pa_arbgb }| TYPE `I`.
```
Benefits:
- Fast prototyping with minimal code
- Built-in input handling and event processing
- Full-stack behavior with no setup

abap2UI5 mirrors this classic selection screen behavior in the browser. Use `Z2UI5_CL_XML_VIEW` to define simple views and exchange data with the `_bind_edit` method:

```abap
CLASS zcl_app_input DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA pa_arbgb TYPE string VALUE `MDG_TR`.
ENDCLASS.

CLASS zcl_app_input IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
      client->view_display(
        z2ui5_cl_xml_view=>factory(
            )->input( client->_bind_edit( pa_arbgb )
            )->button( text  = `post` press = client->_event( `POST` ) ) ).
      RETURN.
    ENDIF.

    IF client->check_on_event( `POST` ).
      client->message_box_display( |Input: { pa_arbgb }| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
```
Plus, you can test code changes instantly — reload the page, enter input, and press the button — all within a single class, with no external UI tooling.

### ALV-Style Table Output in the Browser

Table output is one of the most common tasks for ABAP developers, and ALV is the iconic tool here. `CL_SALV_TABLE` makes generating tabular output straightforward:

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
- No external annotations, CDS Views, or DDIC artifacts needed
- Ideal for admin tools and quick overviews

abap2UI5 follows a similar pattern — bind the internal table to a UI5 table control:

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

    DATA(columns) = tab->columns( ).
    columns->column( )->text( `ARBGB` ).
    columns->column( )->text( `MSGNR` ).
    columns->column( )->text( `TEXT`  ).

    DATA(cells) = tab->items( )->column_list_item( ).
    cells->text( `{ARBGB}` ).
    cells->text( `{MSGNR}` ).
    cells->text( `{TEXT}` ).

    client->view_display( tab->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

From here, it's a small step to generate everything dynamically with RTTI — similar to SALV, but running directly in the browser.

### Classic Popups, Modern Events

Anyone who has browsed SE37 for POPUP_TO_* knows the charm of classic ABAP popups. Function modules like `POPUP_TO_CONFIRM` feel simple yet powerful:

```abap
REPORT zre_app_popup.

DATA event TYPE string.
CALL FUNCTION `POPUP_TO_CONFIRM`
  EXPORTING
    titlebar      = `Title`
    text_question = `Do you like dinosaurs?`
  IMPORTING
    answer        = event.

CASE event.
  WHEN `1`.
    MESSAGE `the result is YES` TYPE `I`.
  WHEN `2`.
    MESSAGE `the result is NO` TYPE `I`.
ENDCASE.
```
Benefits:
- Centralized event logic
- Simple and readable program flow
- Encapsulated and reusable

abap2UI5 delivers a similar experience: apps can call each other and encapsulate dialog logic in reusable classes:

```abap
CLASS zcl_app_alv_event DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_alv_event IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
      client->nav_app_call( z2ui5_cl_pop_to_confirm=>factory(
        i_question_text = `Do you like dinosaurs?`
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

Browser-based roundtrips need slightly different flow control, but the overall approach remains intuitive for ABAP developers.

### More Developer-Friendly Advantages

Beyond the code snippets above, the Over-the-Wire approach of abap2UI5 — based entirely on backend development — brings several more benefits.

#### Zero-Setup Deployment

In abap2UI5, apps are just ABAP classes — deployment is as simple as activating the class. Transport to production goes through the standard transport system from traditional ABAP development:
- No separate frontend build or deployment pipeline
- Other developers or consultants can test code changes instantly
- Every app is abapGit-compatible — no separate frontend artifacts required

#### No Caching Issues

A common frustration in frontend development: caching — especially with BSP or Fiori Elements apps. You make a change, but nothing happens because of cached files, unless you manually trigger several cache-invalidation transactions in the SAP backend. abap2UI5 sidesteps this entirely, generating the UI dynamically on every request:
- No need to clear browser or server caches
- Fast development iteration — edit the code, refresh the browser, see results

#### Develop in Any IDE

There's often debate about which IDE to use — but why not let everyone pick their favorite? You build abap2UI5 apps entirely in ABAP. Work in ADT, SE80, or explore VS Code integration:
- No extra setup required — works in any IDE
- Ideal for teams with different tooling preferences

#### Pure ABAP Debugging

Frontend-heavy apps often require switching between browser dev tools, JavaScript logs, and network inspectors. With abap2UI5, the UI is pure ABAP — no JavaScript, no extra layers. Just set a breakpoint in your ABAP method:
- Backend-only debugging with the classic SE80 debugger or ADT
- No browser debugging tools needed

#### Easy Code Sharing

Sharing your apps or code is simple. Since abap2UI5 apps are standard ABAP classes, share them through abapGit, transports, or even copy-paste:
- Easy to clone and test other apps
- Great for collaboration and code reviews
- Encourages modular, reusable code

### Conclusion

abap2UI5 brings back familiar ABAP development patterns. It reuses proven concepts like Selection Screens, ALV tables, and popup dialogs — but renders them as UI5 apps, all defined and run in ABAP.
The result: backend-driven UI development with minimal tooling and fast iteration. Whether prototyping or building full business apps, abap2UI5 keeps development close to what ABAP developers know best.

Happy ABAPing!
