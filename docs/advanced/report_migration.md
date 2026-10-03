---
outline: [2, 4]
description: Converting a classic ABAP report - selection screen, event blocks, WRITE list, ALV - into an abap2UI5 app of the abap-cloud-gui add-on with its converter report2cloud, from the command line or through the MCP server's migrate_report tool, and what is left to do for ABAP Cloud.
---
# Migrating Classic Reports

::: info Preview — coming with the next release
The converter report2cloud and the MCP server's `migrate_report` tool are
finished on development branches of
[abap-cloud-gui](https://github.com/abap2UI5-addons/abap-cloud-gui) and of the
[MCP server](/advanced/mcp_server), and are not in a release of either yet.
Details can still change until then.
:::

ABAP Cloud has no `REPORT`, no `PARAMETERS`, no `WRITE` and no ALV, and a
company with a few thousand Z reports cannot rewrite them by hand. The
[abap-cloud-gui](https://github.com/abap2UI5-addons/abap-cloud-gui) add-on
keeps the programming model of a report instead: a class inheriting from
`z2ui5_cl_cgui_report` has the event blocks as methods and `write( )`,
`alv( )` and `message( )` as calls, and runs as a UI5 app on ABAP Cloud as
well as on NetWeaver down to 7.02. That makes the step from a report to a
class mechanical, and **report2cloud** does it.

The converter parses the report with `@abaplint/core`, the parser the whole
ecosystem lints with. It is **deterministic** — the same report gives the same
class, byte for byte — and it **refuses rather than guesses**: a statement that
has no counterpart in a browser app is reported with file, line and column,
and no class is written. Everything else in the report, its logic, is copied
as it is written.

## Running it

### From the command line

report2cloud lives in the add-on's repository, beside the runtime it writes
against, and is not published on npm:

```sh
git clone https://github.com/abap2UI5-addons/abap-cloud-gui
cd abap-cloud-gui && npm ci
npm run report2cloud -- zflights.prog.abap --out src/02
```

`--class` names the class (default: `zcl_` and the report name), `--texts`
points at the report's `.prog.xml` when it does not lie beside the source,
`--check` lints the result with abaplint for 7.50 and for ABAP Cloud, and
`--partial` writes the class even when statements were refused, with those
statements marked. The exit code is 0 when the report was converted and 2
when something was refused.

What it writes is abapGit format, ready to pull:

| File | What it is |
| --- | --- |
| `<class>.clas.abap` | the report class |
| `<class>.clas.xml` | its sidecar, described with the title of the report |
| `<class>.clas.locals_def.abap`, `.locals_imp.abap` | the local classes and interfaces of the report, if it has any |
| `<class>.migration.md` | the migration report — see [below](#the-migration-report) |

The selection texts, the text symbols and the title live in the text pool,
not in the source. abapGit writes them into `<report>.prog.xml`; with that
file the class gets the real texts, without it placeholders and a TODO each.

### From an AI agent: `migrate_report`

The [MCP server](/advanced/mcp_server) runs the same converter as a tool, from
an abap-cloud-gui checkout (`ABAP_CLOUD_GUI_HOME`, or a sibling
`../abap-cloud-gui`, with `npm ci` done):

| Input | What it is |
| --- | --- |
| `source` | the report source, as in `<report>.prog.abap` |
| `texts_xml` | optional: the report's `.prog.xml`, for the real selection texts and text symbols |
| `class_name` | optional: the class to generate |
| `partial` | optional: on refusals, still return the draft class with the refused statements marked |
| `deploy` | optional: also deploy the class into the local sandbox and start it |

The answer carries the generated files, the migration report and the
refusals with `file:row:col`. With `deploy: true` the class is written into
the sandbox together with the add-on's runtime and the popups it calls, the
backend is built, and the answer gains the
[agent snapshot](/advanced/agents#what-an-agent-sees-the-snapshot) of the
selection screen; `app_act` with `CGUI_EXECUTE` runs the report from there.
The database tables the report reads are not in the local backend: the
selection screen runs, a run that reads them does not.

## Before and after

A short flight list with a select-option, a drilldown and a message:

```abap
REPORT zr2c_02_flights MESSAGE-ID zr2c.
TABLES sflight.
DATA: gt_flight TYPE STANDARD TABLE OF ty_flight, gs_flight TYPE ty_flight.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_carrid LIKE sflight-carrid OBLIGATORY DEFAULT 'LH'.
SELECT-OPTIONS: s_fldate FOR sflight-fldate.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  s_fldate-sign = 'I'. s_fldate-option = 'BT'.
  s_fldate-low = sy-datum. s_fldate-high = sy-datum + 90.
  APPEND s_fldate.

START-OF-SELECTION.
  SELECT carrid connid fldate FROM sflight INTO TABLE gt_flight
    WHERE carrid = p_carrid AND fldate IN s_fldate.
  LOOP AT gt_flight INTO gs_flight.
    WRITE: / gs_flight-carrid HOTSPOT, gs_flight-connid, gs_flight-fldate.
    HIDE: gs_flight-carrid, gs_flight-connid.
  ENDLOOP.

AT LINE-SELECTION.
  MESSAGE i003 WITH gs_flight-carrid gs_flight-connid.
```

becomes, abbreviated (the full class is in the converter's
[test snapshots](https://github.com/abap2UI5-addons/abap-cloud-gui/tree/e754afd0d60569c20e96d2f01b61c67255b50fff/tools/report2cloud/test/snapshots/zr2c_02_flights)):

```abap
CLASS z2ui5_cl_cgui_r2c_02 DEFINITION PUBLIC
  INHERITING FROM z2ui5_cl_cgui_report
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " global data of the report
    DATA:
      gt_flight TYPE STANDARD TABLE OF ty_flight WITH DEFAULT KEY,
      gs_flight TYPE ty_flight.

    " selection screen
    DATA p_carrid TYPE sflight-carrid.
    DATA s_fldate TYPE RANGE OF sflight-fldate.
  ...
  METHOD initialization.

    DATA ls_s_fldate LIKE LINE OF s_fldate.

    set_title( `Flights of an Airline` ).
    p_carrid = 'LH'.

    ls_s_fldate-sign   = 'I'.
    ...
    APPEND ls_s_fldate TO s_fldate.

  ENDMETHOD.

  METHOD selection_screen.

    screen->block_begin( `Flights`
        )->parameter( val        = p_carrid
                      obligatory = abap_true
        )->select_option( val  = s_fldate
                          text = `Flight date`
        )->block_end( ).

  ENDMETHOD.

  METHOD start_of_selection.

    " every run starts with the global data of a fresh start - the classic report restarted after its list
    CLEAR: gt_flight,
           gs_flight.

    SELECT carrid, connid, fldate
      FROM sflight
      INTO TABLE @gt_flight
      WHERE carrid = @p_carrid
        AND fldate IN @s_fldate.
    LOOP AT gt_flight INTO gs_flight.
      list( )->new_line(
          )->write( val     = gs_flight-carrid
                    hotspot = abap_true
                    hide    = |{ gs_flight-carrid }\t{ gs_flight-connid }|
          )->write( gs_flight-connid
          )->write( gs_flight-fldate ).
    ENDLOOP.

  ENDMETHOD.

  METHOD at_line_selection.

    " HIDE - the fields the clicked line was written with
    SPLIT hide AT |\t| INTO TABLE DATA(lt_hide).
    gs_flight-carrid = VALUE #( lt_hide[ 1 ] OPTIONAL ).
    gs_flight-connid = VALUE #( lt_hide[ 2 ] OPTIONAL ).

    MESSAGE i003(zr2c) WITH gs_flight-carrid gs_flight-connid INTO DATA(lv_message).
    message( text = lv_message
             type = `I` ).

  ENDMETHOD.
```

The texts come from the text pool, the `SELECT` is in the strict syntax ABAP
Cloud requires, `HIDE` travels with the hotspot and comes back in
`at_line_selection( )`, and the short `MESSAGE i003` names its message class,
because a class has no `MESSAGE-ID`. The migration report then says what is
left: `SFLIGHT` is not released on ABAP Cloud — the hint is `/DMO/FLIGHT` or a
CDS view of your own — and the message class `ZR2C` must exist in the target
system.

## What it converts

The whole mapping, construct by construct, is the table in the converter's
[README](https://github.com/abap2UI5-addons/abap-cloud-gui/blob/e754afd0d60569c20e96d2f01b61c67255b50fff/tools/report2cloud/README.md#the-mapping).
In short:

| Classic | abap-cloud-gui |
| --- | --- |
| `PARAMETERS`, `SELECT-OPTIONS`, `RANGES` | public attributes and `screen->parameter( )` / `select_option( )`, with `OBLIGATORY`, `DEFAULT`, `MODIF ID`, checkboxes and radio button groups |
| `SELECTION-SCREEN` blocks, lines, comments, push buttons | `block_begin( )`, `line_begin( )`, `comment( )`, `button( )` |
| `INITIALIZATION`, `AT SELECTION-SCREEN [OUTPUT / ON / ON VALUE-REQUEST]`, `START-OF-SELECTION`, `END-OF-SELECTION`, `TOP-OF-PAGE`, `AT LINE-SELECTION`, `AT USER-COMMAND` | methods named after them — `initialization( )`, `at_selection_screen_output( )`, `at_value_request( )`, `start_of_selection( )`, `at_line_selection( )`, … |
| `LOOP AT SCREEN` … `MODIFY SCREEN` in `AT SELECTION-SCREEN OUTPUT` | `screen->loop_at_screen( )` and `modify_screen( )` |
| `WRITE`, `FORMAT`, `ULINE`, `SKIP`, `NEW-PAGE`, `HIDE` | the list: `write( )`, `new_line( )`, `uline( )`, colors, hotspots |
| `CL_SALV_TABLE`, `REUSE_ALV_GRID_DISPLAY` with a field catalog | `alv( )` with title, column texts, hidden columns and line selection |
| `MESSAGE` in all its forms | `message( )`, with the message class still the source of the text |
| `F4IF_INT_TABLE_VALUE_REQUEST` in an F4 | `value_help_popup( )` |
| `FORM` / `PERFORM` | private methods; a `USING` parameter the FORM writes to becomes `CHANGING` |
| global data | public attributes, cleared at the start of every run, as the classic report restarted after its list |
| ABAP SQL | strict mode, host variables escaped with `@` |
| everything else | copied as written, comments included |

## What it refuses

Each of these stops the conversion with `file:row:col` and the reason. All of
them are collected, so one run lists everything there is to do:

| Construct | Why |
| --- | --- |
| `CALL SCREEN`, `MODULE`, `SET SCREEN`, `LEAVE TO SCREEN`, `CALL SUBSCREEN`, `SET CURSOR`, `CONTROLS` | dynpros have no counterpart in a browser app |
| `CALL SELECTION-SCREEN`, selection screens of their own, tabbed blocks, function keys | only the standard selection screen is converted; a push button replaces a toolbar function |
| `AT SELECTION-SCREEN ON BLOCK / RADIOBUTTON GROUP / HELP-REQUEST / END OF / EXIT-COMMAND` | not supported by abap-cloud-gui yet |
| `CALL TRANSACTION`, batch input | no SAP GUI transaction can be started; call the released API instead |
| `SUBMIT` | convert the other report too and navigate to its class |
| `EXEC SQL` | native SQL is not available on ABAP Cloud |
| page, cursor and print statements: `END-OF-PAGE`, `AT PFnn`, `READ LINE`, `MODIFY LINE`, `SCROLL LIST`, `NEW-PAGE PRINT ON`, … | the list has no pages, cursor, function keys or printing |
| `LEAVE TO LIST-PROCESSING`, `LEAVE PROGRAM` | the list is shown after `start_of_selection( )`; there is nothing to leave |
| `NODES`, `GET`, `REJECT` | logical databases |
| `INCLUDE`, macros | the include is not part of the input and macros are not expanded — inline them first |
| tables with header line, `SELECT` without `INTO` | not allowed in classes |
| `PERFORM` into another program, a `PERFORM` whose parameters do not match the FORM | only the report's own FORMs become methods |
| `POPUP_TO_CONFIRM`, `POPUP_TO_DECIDE`, `POPUP_GET_VALUES` | these wait for the answer, and a popup in abap2UI5 does not: `popup_to_confirm( )` and `at_user_command( )` instead |
| `GUI_DOWNLOAD`, `GUI_UPLOAD`, `cl_gui_frontend_services`, `cl_gui_alv_grid` and the other SAP GUI controls | there is no SAP GUI frontend and no dynpro container |
| other `REUSE_ALV_*` and `LVC_*` modules, `DYNP_VALUES_READ`, `BDC_*` | no counterpart |
| a local class that reads the report's globals, calls `PERFORM` or writes to the list | a local class cannot reach the attributes, methods or list of the generated class |
| `GENERATE SUBROUTINE POOL`, `INSERT REPORT`, OLE | not available |

The refusals stay with a person. They are the places where the report does
something a browser app does not do, and the answer is a design decision, not
a translation.

## The migration report

Next to the class, `<class>.migration.md` is the work list: the refusals, the
TODOs, the objects to check for ABAP Cloud, abaplint's findings (with
`--check`), what was not carried over, and every mapped construct with its
line in the report. The release check names every database table, DDIC type,
function module, class and message class the class uses, with its first
position in the generated class and, for the well-known ones, a released
successor as a hint — `MARA` → `I_Product`, `KNA1` → `I_Customer`, `BKPF` →
`I_JournalEntry`, `SFLIGHT` → `/DMO/FLIGHT`.

## What is left to you

A converted class compiles against abap-cloud-gui on premise. That is not the
same as a class that runs on ABAP Cloud, and the converter says so rather than
hiding it:

- **Unreleased tables and APIs need a successor from SAP.** The report's logic
  still reads the tables and calls the APIs it read and called before.
  Replacing them is not a syntax transformation: a released CDS view has other
  field names, other keys, sometimes another granularity. That is the job of a
  person, or of an AI model with the migration report as its work list — and
  the successor in the report is a hint, not a guarantee.
- **Some behavior is approximated.** The positions and lengths of `WRITE` are
  dropped and its formats are not all kept; the classic screen's conversion to
  upper case is not carried over; a `TOP-OF-PAGE` header is written once and
  after each `NEW-PAGE`, not at every page break; `STOP` outside
  `START-OF-SELECTION` becomes a `RETURN` with a TODO; ALV cells show dates and
  times as the model carries them. The migration report lists, per class,
  what was not carried over.
- **It has not run on a real system yet.** The evidence is the converter's own
  tests: every class it generates from its corpus of classic reports compiles
  with abaplint
  for 7.50 (no finding), for 7.02 after the downport, and against the abap2UI5
  linter, and its ABAP Cloud findings are exactly the unreleased objects the
  migration report lists. Every class also runs — transpiled with the add-on
  against abap2UI5's Node runtime, with seeded flight tables, and operated the
  way a user operates it: fill the selection screen, Execute, click a hotspot
  or a grid row, pick in the F4 popup, read the message popover, go back, run
  again — against expectations written by hand from what the classic report
  prints. None of that is an SAP system, and none of the classes has been
  activated on one.

## The loop for an AI model

The conversion is deterministic so that the step after it can be iterative:

1. report2cloud writes the class and the migration report — the work list.
2. The model replaces one unreleased object, or works one TODO, at a time.
3. Three checkers answer, without an SAP system: abaplint with the ABAP Cloud
   configuration (released APIs, strict syntax), the
   [abap2UI5 linter](/advanced/linter), and the transpiled backend — ABAP Unit,
   or the report operated through its screen with the
   [agent tools](/advanced/agents).
4. Repeat until all three are clean and the work list is empty.

## Next Steps

- [abap-cloud-gui](https://github.com/abap2UI5-addons/abap-cloud-gui) — the
  add-on the converted class runs on, and how to install it
- [MCP Server](/advanced/mcp_server) — `migrate_report` and the sandbox it
  deploys into
- [Agent-Operable Apps](/advanced/agents) — operating the converted report
  through its screen
- [#29 When the API Is Not Released](/advanced/insights/29-when-the-api-is-not-released)
  — what to do about the objects the migration report lists
