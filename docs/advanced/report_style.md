---
outline: [2, 4]
description: Apps written the way you write a classic ABAP report - selection screen, WRITE list, ALV and the event blocks as methods - with the abap-cloud-gui add-on, on ABAP Cloud and down to 7.02, and where its samples run in the browser.
---
# Apps Written Like Reports

ABAP Cloud has no selection screen, no `WRITE` list and no `CL_SALV_TABLE`.
The [abap-cloud-gui](https://github.com/abap2UI5-addons/abap-cloud-gui)
add-on brings the programming model of a classic report to abap2UI5
instead: a class inheriting from `z2ui5_cl_cgui_report` has the event
blocks as methods, `PARAMETERS` and `SELECT-OPTIONS` as attributes, and
`write( )`, `alv( )` and `message( )` as calls. It runs as an abap2UI5 app,
on ABAP Cloud as well as on NetWeaver down to 7.02, and no UI5 view is
written by hand.

It suits a quick report or tool on ABAP Cloud, a first step into abap2UI5
in familiar terms, and the classic reports a company moves off the SAP GUI -
[Migrating Classic Reports](/advanced/report_migration) converts their user
interface with report2UI5.

## Installing It

With abapGit, after abap2UI5: the release
[v1.0.0](https://github.com/abap2UI5-addons/abap-cloud-gui/releases/tag/v1.0.0)
of the repository, or the zip attached to it -
`abap-cloud-gui-1.0.0-702.zip` is the copy downported to 7.02. The package
holds classes, two interfaces and the two tables of the variants and the
layouts; nothing else is set up.

## A First Report

```abap
CLASS zcl_hello DEFINITION PUBLIC
  INHERITING FROM z2ui5_cl_cgui_report
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA p_name  TYPE string.
    DATA p_times TYPE i.

  PROTECTED SECTION.
    METHODS initialization REDEFINITION.
    METHODS selection_screen REDEFINITION.
    METHODS start_of_selection REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_hello IMPLEMENTATION.

  METHOD initialization.

    set_title( `Hello World Report` ).
    p_times = 3.

  ENDMETHOD.

  METHOD selection_screen.

    screen->parameter( val        = p_name
                       text       = `Your name`
                       obligatory = abap_true
        )->parameter( val  = p_times
                      text = `Lines` ).

  ENDMETHOD.

  METHOD start_of_selection.

    write( |Hello { p_name }!| )->uline( ).
    DO p_times TIMES.
      write( |Line { sy-index }| )->new_line( ).
    ENDDO.

  ENDMETHOD.

ENDCLASS.
```

Start it like any abap2UI5 app, `?app_start=zcl_hello`: the selection
screen comes up, **Execute** (F8) runs `start_of_selection( )` and shows
the list, **Back** (F3) returns to the selection screen.

## The Parts of a Report

| Classic | In the class |
| --- | --- |
| `PARAMETERS`, `SELECT-OPTIONS` | public attributes, laid out in `selection_screen( )` with `screen->parameter( )`, `select_option( )`, `block_begin( )`, `radiobutton( )`, `checkbox( )`, `button( )` |
| `INITIALIZATION`, `AT SELECTION-SCREEN [OUTPUT, ON field]` | `initialization( )`, `at_selection_screen( )`, `at_selection_screen_output( )`, `at_selection_screen_on( field )` |
| `START-OF-SELECTION`, `END-OF-SELECTION` | `start_of_selection( )`, `end_of_selection( )` |
| `WRITE`, `ULINE`, `SKIP`, `HOTSPOT`, `HIDE` | `write( )` and the list it returns |
| `CL_SALV_TABLE` | `alv( table )` - columns from RTTI and the DDIC, sort, filter, totals, layouts, Excel export |
| `AT LINE-SELECTION`, `AT USER-COMMAND` | `at_line_selection( )`, `at_user_command( ucomm )` |
| `ON VALUE-REQUEST`, `ON HELP-REQUEST` | `at_value_request( field )`, `at_selection_screen_on_help( field )`; the F4 of the DDIC, search helps and CDS value helps come by themselves |
| `MESSAGE`, `POPUP_TO_CONFIRM` | `message( )`, `popup_to_confirm( )` |

Beside these, the runtime brings selection variants on the server, the last
entries of a field, a draft of a selection screen left without executing,
`SET` / `GET PARAMETER ID`, `SUBMIT` and `CALL TRANSACTION` between report
classes, dynpros of fields, stand-ins of the SAP GUI controls and intent
navigation in the Fiori launchpad. The repository's
[README](https://github.com/abap2UI5-addons/abap-cloud-gui#readme) is the
reference for all of it.

## Trying It in the Browser

The samples of the add-on run on its own page,
[abap2ui5-addons.github.io/abap-cloud-gui](https://abap2ui5-addons.github.io/abap-cloud-gui/):
each classic report beside the class report2UI5 writes of it and that
class running as an abap2UI5 app, in a copy of the abap2UI5 playground that
carries the add-on. A report of your own can be pasted there, converted and
started the same way. The
[abap2UI5 playground](https://abap2ui5.github.io/playground/) itself runs
the framework alone and does not carry the add-on.

In a system, the samples are part of the package:
`?app_start=z2ui5_cl_cgui_sample_05` starts a complete flight report with a
selection screen, an ALV grid, a classic list and a drilldown.
