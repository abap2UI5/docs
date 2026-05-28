---
outline: [2, 4]
---
# AI-Assisted Development

This page is the entry point for any AI assistant (Claude, Copilot, Cursor, …) that should build abap2UI5 apps. It collects, in one place, every source of information the model needs and points to the canonical references in the three project repositories.

If you are pasting context into an AI tool, **start here**, then follow the links — the linked files (especially the `CLAUDE.md` files in the source repositories) are kept up to date with every release and are the source of truth.

## The Three Repositories

abap2UI5 lives in three repositories. Each carries information an AI assistant should know:

| Repository | What's in it | Why an AI needs it |
|---|---|---|
| **[abap2UI5/abap2UI5](https://github.com/abap2UI5/abap2UI5)** | Core framework — interfaces, view builder, runtime | The public API (`z2ui5_if_app`, `z2ui5_if_client`, `z2ui5_cl_xml_view`, `z2ui5_cl_util_xml`) the app interacts with |
| **[abap2UI5/samples](https://github.com/abap2UI5/samples)** | 250+ working demo apps + app-development guide | Canonical patterns and a copy-pastable example for almost every UI5 control and feature |
| **[abap2UI5/docs](https://github.com/abap2UI5/docs)** | This documentation site | Conceptual background, lifecycle, cookbook recipes |

## Primary AI Briefings — The `CLAUDE.md` Files

Two `CLAUDE.md` files in the source repositories are written specifically as briefings for AI assistants. They are the **first** thing to read:

- **[abap2UI5/CLAUDE.md](https://github.com/abap2UI5/abap2UI5/blob/main/CLAUDE.md)** — framework-level briefing
  - Project overview, architecture, the request/response roundtrip
  - Repository layout (`src/00` utilities, `src/01` engine, `src/02` public API)
  - Coding rules, naming, style guide (Clean ABAP exceptions)
  - **Deprecated UI5 controls** to avoid
  - Important rules for AI assistants (do/don't list)

- **[samples/CLAUDE.md](https://github.com/abap2UI5/samples/blob/main/CLAUDE.md)** — app-development briefing
  - Canonical app structure (`on_init`, `on_event`, `view_display`, `data_read`, `data_update`)
  - Formatting rules (blank lines, indentation, parameter alignment)
  - When to use inline vs. handler methods
  - Navigation patterns and event dispatching
  - `z2ui5_cl_xml_view` vs. `z2ui5_cl_util_xml` style rules

If an AI assistant only reads two files in this project, these are the two.

## Quick Briefing — The Mental Model

The minimum an AI must know to write working apps:

#### 1. One interface, one method

Every app implements `z2ui5_if_app` and has a single `main` method. The framework calls `main` on every HTTP roundtrip — initial load *and* every user interaction:

```abap
CLASS zcl_my_app DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    " everything happens here
  ENDMETHOD.
ENDCLASS.
```

#### 2. Dispatch with `CASE abap_true`

`main` is a dispatcher. Three lifecycle checks decide what to do:

```abap
CASE abap_true.
  WHEN client->check_on_init( ).        " first call
    " load data + render view
  WHEN client->check_on_event( `POST` ). " user fired event 'POST'
    " handle it
  WHEN client->check_on_navigated( ).   " returned from sub-app / popup
    " refresh state
ENDCASE.
```

→ [Life Cycle](/cookbook/event_navigation/life_cycle)

#### 3. State lives in **public** class attributes

Anything bound to the view must be `PUBLIC`. The framework serializes public attributes between roundtrips automatically — no session handling needed.

→ [Binding → Bound Attributes Must Be Public](/cookbook/model/binding#bound-attributes-must-be-public)

#### 4. `client` is the only framework API

Views, events, binding, navigation, messages — everything goes through the `client` reference (`z2ui5_if_client`):

| Category | Methods |
|---|---|
| Views | `view_display`, `view_destroy`, `view_model_update` |
| Popups / popovers | `popup_display`, `popup_destroy`, `popover_display`, `popover_destroy` |
| Binding | `_bind( var )` (read-only), `_bind_edit( var )` (two-way) |
| Events | `_event( 'NAME' )`, `check_on_event( 'NAME' )`, `get_event_arg( i )` |
| Navigation | `nav_app_call( app )`, `nav_app_leave( )`, `get_app_prev( )` |
| Messages | `message_box_display`, `message_toast_display` |
| Lifecycle | `check_on_init`, `check_on_event`, `check_on_navigated` |

Read the full interface at [`z2ui5_if_client.intf.abap`](https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_if_client.intf.abap).

#### 5. Views are XML — built with one of two builders

Views are UI5 XML strings passed to `client->view_display( ... )`. The framework ships two builders, both fully supported:

- **`z2ui5_cl_util_xml`** — generic XML builder. Element name + attributes as strings → maps **1:1** to the UI5 SDK. **Recommended for AI-generated code.**
- **`z2ui5_cl_xml_view`** — fluent builder with one ABAP method per UI5 control (~446 methods). Convenient for humans, but has naming inconsistencies and incomplete coverage → harder for an AI to use correctly.

→ [View Definition](/cookbook/view/definition)

## Recommended Builder for AI: `z2ui5_cl_util_xml`

Translate any UI5 XML example from the [UI5 SDK](https://sapui5.hana.ondemand.com/sdk) **1:1** into ABAP — no wrapper, no abstraction:

| UI5 XML | ABAP with `z2ui5_cl_util_xml` |
|---|---|
| `<Button text="Send" />` | `->__( n = `Button` a = `text` v = `Send` )` |
| `press="onPress"` | `v = client->_event( `BUTTON_POST` )` |
| `value="{/name}"` | `v = client->_bind_edit( name )` |
| `<FeedInput><actions>…</actions></FeedInput>` | `->_( `FeedInput` )->_( `actions` )->__( … )` |

Builder methods:

- `_( n, ns, p )` — add child, **go into** it (next call adds a grandchild)
- `__( n, ns, a, v, p )` — add child, **stay** at current level (leaf / sibling)
- `p( n, v )` — add a single attribute to the current node
- `n( 'Name' )` / `n( )` — navigate to named ancestor / parent
- `stringify( indent = abap_true )` — serialize to XML

::: warning Boolean attribute values
Always pass `'true'` or `'false'` as **string literals** — never `abap_true` / `abap_false`. `abap_false` (space) is silently dropped; `abap_true` (`'X'`) produces an invalid XML attribute value.
:::

Complete worked example: see [Quickstart](/get_started/quickstart), [Hello World](/get_started/hello_world), [Full Example](/get_started/full_example), and the `CLAUDE.md` in [abap2UI5](https://github.com/abap2UI5/abap2UI5/blob/main/CLAUDE.md#building-views--two-supported-apis).

## Canonical App Template

For anything beyond ~50 lines in `main`, extract `on_init` and `on_event` first, then add helpers (`view_display`, `data_read`, `data_update`) only when needed. Class names lowercase, no `FINAL`, definition order `TYPES` → `DATA` → `METHODS`.

```abap
CLASS zcl_app_xxx DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    " bound data (public DATA attributes used by _bind / _bind_edit)

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.        " first call: load data, render view
    METHODS on_event.       " user triggered an event
    METHODS on_navigation.  " returned from sub-app or popup
    METHODS view_display.   " build and render the view
    METHODS data_read.      " SELECT
    METHODS data_update.    " INSERT / UPDATE / DELETE

  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_app_xxx IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_init( ).
      on_init( ).
    ELSEIF client->check_on_navigated( ).
      on_navigation( ).
    ELSEIF client->check_on_event( ).
      on_event( ).
    ENDIF.

  ENDMETHOD.

  " on_init / on_event / view_display / data_read / data_update …

ENDCLASS.
```

The full template (with formatting and blank-line rules) lives in [samples/CLAUDE.md](https://github.com/abap2UI5/samples/blob/main/CLAUDE.md#app-structure).

## Sample Apps — 250+ Working Examples

The fastest way for an AI to ground a generation in working code is to look up a similar app in the [samples repository](https://github.com/abap2UI5/samples/tree/main/src). Each `z2ui5_cl_demo_app_*` class is a self-contained, abapGit-installable app demonstrating a single feature or control.

Browse them online (no system needed): <https://abap2ui5.github.io/web-abap2ui5-samples/>

## Built-In Popups — Do Not Reinvent

Before building a custom dialog, check whether one of the built-in popup classes already covers the case:

`Z2UI5_CL_POP_TO_CONFIRM`, `Z2UI5_CL_POP_TO_INFORM`, `Z2UI5_CL_POP_TO_SELECT`, `Z2UI5_CL_POP_FILE_UL`, `Z2UI5_CL_POP_FILE_DL`, `Z2UI5_CL_POP_TABLE`, `Z2UI5_CL_POP_TEXTEDIT`, `Z2UI5_CL_POP_PDF`, `Z2UI5_CL_POP_HTML`, `Z2UI5_CL_POP_MESSAGES`, `Z2UI5_CL_POP_ERROR`, `Z2UI5_CL_POP_GET_RANGE`, `Z2UI5_CL_POP_GET_RANGE_M`, `Z2UI5_CL_POP_INPUT_VAL`, `Z2UI5_CL_POP_JS_LOADER`, `Z2UI5_CL_POP_ITAB_JSON_DL`

→ [Built-in popups](/cookbook/popup_popover/built_in)

## Deprecated UI5 Controls — Do Not Generate

Whole libraries to **never** use: `sap.ui.commons.*`, legacy `sap.viz.ui5.*` charts.

Individual deprecated items: `sap.m.MultiEditField`, `sap.f.Avatar` (use `sap.m.Avatar`), `sap.ui.core.XMLComposite`, `HTMLView`, `JSONView`, `JSView`, `TemplateView`, `sap.ui.table.ColumnHeader`, `sap.f.routing.*`, deprecated `Avatar*` enums, Belize / Blue Crystal themes, …

The authoritative list is at <https://ui5.sap.com/#/api/deprecated>. A condensed, AI-friendly version is in the [abap2UI5 CLAUDE.md → Deprecated UI5 Controls](https://github.com/abap2UI5/abap2UI5/blob/main/CLAUDE.md#deprecated-ui5-controls--do-not-use) section.

## Documentation Map — What to Read When

When an AI needs deeper information than this page provides:

| Topic | Best reference |
|---|---|
| Architecture, request roundtrip | [Concept](/technical/concept), [How It All Works](/technical/how_it_all_works) |
| Lifecycle and `CASE abap_true` pattern | [Cookbook → Life Cycle](/cookbook/event_navigation/life_cycle) |
| Building views, control choice | [Cookbook → View Definition](/cookbook/view/definition) |
| Data binding (`_bind`, `_bind_edit`) | [Cookbook → Binding](/cookbook/model/binding) |
| Tables and trees | [Cookbook → Tables](/cookbook/model/tables), [Trees](/cookbook/model/trees) |
| Events, actions, exceptions | [Cookbook → Event, Navigation](/cookbook/event_navigation/life_cycle) |
| Popups and popovers | [Cookbook → Popup, Popover](/cookbook/popup_popover/popup) |
| Messages, toasts, i18n | [Cookbook → Translation, Messages](/cookbook/translation_messages/message) |
| Browser interaction (focus, scroll, URL) | [Cookbook → Browser Interaction](/cookbook/browser_interaction/title) |
| Device features (camera, geo, files) | [Cookbook → Device Capabilities](/cookbook/device_capabilities/info) |
| RAP / CDS / EML integration | [Cookbook → EML, CDS, SQL](/cookbook/eml_cds_sql/rap) |
| Snippets and recurring patterns | [Cookbook → Snippets](/cookbook/expert_more/snippets) |
| Installation / HTTP handler | [Quickstart](/get_started/quickstart) |

## A Ready-Made Prompt for AI Assistants

A prompt that gives an AI assistant enough context to produce a working app:

> You are building an abap2UI5 app. Before writing any code, read these two briefings:
>
> 1. <https://github.com/abap2UI5/abap2UI5/blob/main/CLAUDE.md> — framework, API, rules
> 2. <https://github.com/abap2UI5/samples/blob/main/CLAUDE.md> — app structure, formatting
>
> Then consult <https://abap2ui5.github.io/docs/get_started/ai.html> for the documentation map and <https://github.com/abap2UI5/samples/tree/main/src> for 250+ working examples. Use `z2ui5_cl_util_xml` as the view builder. Look up any UI5 control at <https://ui5.sap.com/#/api> and translate the XML 1:1 to ABAP. Avoid any control listed in the "Deprecated UI5 Controls" section of the framework `CLAUDE.md`.

## Hard Rules (Cheat Sheet)

| Rule | Why |
|---|---|
| Implement `z2ui5_if_app` with a single `main` method | The only entry point the framework calls |
| Bound attributes must be `PUBLIC` | The framework binds via dynamic ASSIGN; `PROTECTED`/`PRIVATE` are silently ignored |
| Use `CASE abap_true` with `check_on_init` / `check_on_event( 'NAME' )` / `check_on_navigated` | Canonical dispatcher pattern |
| Use `z2ui5_cl_util_xml` for AI-generated views | Maps 1:1 to UI5 SDK; no wrapper to learn |
| Pass XML boolean attributes as string literals `'true'` / `'false'` | `abap_true` / `abap_false` produce invalid or empty attributes |
| Use backtick string literals (`` ` ``), not single quotes | Project-wide convention enforced by abaplint |
| Never use a deprecated control | See list in framework `CLAUDE.md` |
| Use built-in popups (`z2ui5_cl_pop_*`) before building custom ones | Tested, consistent, less code |
| Run `npx abaplint` on every change in the framework / samples repos | Primary quality gate; must report 0 issues |

That's the briefing. Hand the link to this page (or the two `CLAUDE.md` files it points to) to any AI tool and it has everything it needs to build a working abap2UI5 app.
