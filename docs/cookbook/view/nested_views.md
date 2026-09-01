---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_065
  - z2ui5_cl_smp_app_104
  - z2ui5_cl_smp_app_097
  - z2ui5_cl_smp_app_098
  - z2ui5_cl_smp_app_176
---
# Nested Views

A **nested view** in abap2UI5 is a separate XML view fragment that you inject into a *placeholder* inside another view. The main view stays on screen; only the nested fragment is rendered (and later re-rendered or refreshed) independently. This is the standard pattern for master-detail screens, side panels, tab content, and anywhere you want one part of the UI to update without rebuilding the whole page.

If you know SAPUI5's [nested views](https://sapui5.hana.ondemand.com/sdk/#/topic/df8c9c3d6f2a4d728ba7d6f4cb6c6d35) (`<mvc:XMLView viewName="..."/>`), the goal is the same — split the UI into independently managed pieces. In abap2UI5 the wiring is done from ABAP at runtime: instead of referencing a static view file, you build the nested view's XML in ABAP and tell the client to plug it into a named slot.

### The Basic Pattern

Two ingredients are needed:

1. **An anchor in the main view** — any control with an `id`. The nested view will be inserted *into* this control.
2. **A nested view + a `nest_view_display` call** — builds the fragment and ships it to the named anchor.

```abap
CLASS z2ui5_cl_sample_nest DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mv_input_nest TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_sample_nest IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    " 1) Main view with an anchor
    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Shell`
                )->ele( `Page`
                    )->a( n = `title` v = `Main View`
                    )->a( n = `id`    v = `test`        " <-- the anchor id

                    )->ele( `content`
                        )->tag( `Button`
                            )->a( n = `text`  v = `Re-render only the nested view`
                            )->a( n = `press` v = client->_event( `NEST` ) ).

    " 2) Nested view, built like any other view
    DATA(nested) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->a( n = `title` v = `Nested View`

                )->tag( `Input`
                    )->a( n = `value` v = client->_bind( mv_input_nest )
                )->tag( `Button`
                    )->a( n = `text`  v = `event`
                    )->a( n = `press` v = client->_event( `TEST` ) ).

    IF client->check_on_navigated( ).
      client->view_display( view->stringify( ) ).
    ENDIF.

    CASE client->get( )-event.
      WHEN `NEST`.
        client->nest_view_display(
            val            = nested->stringify( )
            id             = `test`                " target the anchor
            method_insert  = `addContent`          " UI5 mutator on that control
            method_destroy = `removeAllContent` ). " and the one that clears it first
      WHEN `TEST`.
        client->message_toast_display( |nested input: { mv_input_nest }| ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

What happens at runtime: `view_display` paints the main view; the page with `id="test"` sits on screen. When the user clicks the button, `nest_view_display` ships the nested XML to the client, which calls `removeAllContent( )` and then `addContent( ... )` on the control with that id. The nested fragment appears inside the page — without re-rendering the page itself.

Press **Run** and then the button: the main view stays exactly where it is, the nested page appears inside it, and typing in the nested input and pressing *event* proves the two halves share one model — the toast reads back what the nested view bound.

The full pattern (re-render everything vs. main only vs. nested only) is in `Z2UI5_CL_SMP_APP_065`.

### `nest_view_display` Parameters

| Parameter        | Meaning                                                                                            |
| ---------------- | -------------------------------------------------------------------------------------------------- |
| `val`            | The nested view's XML, produced by `stringify( )`.                                                 |
| `id`             | The id of the anchor control in the main view.                                                     |
| `method_insert`  | UI5 mutator method called on the anchor to add the nested view (e.g. `addContent`).                |
| `method_destroy` | Optional. UI5 mutator method that removes the previous nested content before inserting the new one. |

`method_insert` and `method_destroy` are plain UI5 control methods — pick whichever the anchor exposes. The choice depends on the anchor's aggregation:

| Anchor control          | Typical `method_insert`     | Typical `method_destroy`        |
| ----------------------- | --------------------------- | ------------------------------- |
| `Page`, `VBox`, generic | `addContent`                | `removeAllContent`              |
| `FlexibleColumnLayout`  | `addMidColumnPage`          | `removeAllMidColumnPages`       |
| `FlexibleColumnLayout`  | `addEndColumnPage`          | `removeAllEndColumnPages`       |
| `FlexibleColumnLayout`  | `addBeginColumnPage`        | `removeAllBeginColumnPages`     |

Always pass `method_destroy` when the nested view is going to be replaced over the lifetime of the app; otherwise consecutive calls stack new fragments on top of the old ones.

### Independent Re-rendering

The whole point of nested views is to re-render only what changed. Four calls cover the common needs:

| Call                              | What it does                                                                                  |
| --------------------------------- | --------------------------------------------------------------------------------------------- |
| `client->view_display( ... )`     | Replaces the main view's XML. The anchor is recreated, so any nested content is lost too.    |
| `client->nest_view_display( ... )`| Replaces only the nested view. The main view stays on screen.                                |
| `client->view_model_update( )`    | Pushes current ABAP data values into **all already-rendered views**. No re-render.           |
| `client->nest_view_destroy( )`    | Removes the nested view from the frontend without touching the main view.                    |

The main view has the matching `client->view_destroy( )`; the second nested slot has `nest2_view_display( )` and `nest2_view_destroy( )`.

A rule of thumb:

- **Layout changed** (different controls, new columns, new sections) → `view_display` / `nest_view_display`.
- **Only the data changed** (a flag flipped, a row added to a bound table) → `view_model_update`.

`Z2UI5_CL_SMP_APP_065` shows the difference between the three options in a single screen with one button per call.

### Master-Detail with `FlexibleColumnLayout`

The most common real-world use: a master list on the left, detail content on the right. `sap.f.FlexibleColumnLayout` is the standard container; abap2UI5 nests the detail view into its middle column.

```abap
CLASS z2ui5_cl_sample_nest_fcl DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_row,
        title    TYPE string,
        selected TYPE abap_bool,
      END OF ty_s_row.

    DATA t_tab     TYPE STANDARD TABLE OF ty_s_row WITH EMPTY KEY.
    DATA t_tab2    TYPE STANDARD TABLE OF ty_s_row WITH EMPTY KEY.
    DATA mv_layout TYPE string VALUE `OneColumn`.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS view_display_master.
    METHODS view_display_detail.

  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_sample_nest_fcl IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    CASE abap_true.
      WHEN client->check_on_init( ).
        t_tab = VALUE #( ( title = `Order 4711` )
                         ( title = `Order 4712` )
                         ( title = `Order 4713` ) ).
        view_display_master( ).
      WHEN client->check_on_navigated( ).
        view_display_master( ).
      WHEN client->check_on_event( `SELCHANGE` ).
        DATA(ls_row) = VALUE #( t_tab[ selected = abap_true ] OPTIONAL ).
        t_tab2 = VALUE #( ( title = |{ ls_row-title } - item 10| )
                          ( title = |{ ls_row-title } - item 20| ) ).
        mv_layout = `TwoColumnsMidExpanded`.
        view_display_detail( ).
    ENDCASE.

  ENDMETHOD.

  METHOD view_display_master.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
            )->a( n = `xmlns:f`   v = `sap.f`

            )->ele( `Page`
                )->a( n = `title` v = `abap2UI5 - Master Detail`

                )->ele( n = `FlexibleColumnLayout` ns = `f`
                    )->a( n = `layout` v = client->_bind( mv_layout )
                    )->a( n = `id`     v = `test`                     " anchor

                    )->ele( n = `beginColumnPages` ns = `f`
                        )->ele( `List`
                            )->a( n = `mode`            v = `SingleSelectMaster`
                            )->a( n = `items`           v = client->_bind( t_tab )
                            )->a( n = `selectionChange` v = client->_event( `SELCHANGE` )

                            )->ele( `items`
                                )->tag( `StandardListItem`
                                    )->a( n = `title`    v = `{TITLE}`
                                    )->a( n = `selected` v = `{SELECTED}` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD view_display_detail.

    DATA(nested) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->a( n = `title` v = `Nested View`

                )->ele( `List`
                    )->a( n = `items` v = client->_bind( t_tab2 )

                    )->ele( `items`
                        )->tag( `StandardListItem`
                            )->a( n = `title` v = `{TITLE}` ).

    client->nest_view_display(
        val            = nested->stringify( )
        id             = `test`
        method_insert  = `addMidColumnPage`
        method_destroy = `removeAllMidColumnPages` ).

  ENDMETHOD.

ENDCLASS.
```

Press **Run** and pick a row: the master list stays exactly where it is, the
FCL slides its middle column in, and only the detail view was built and sent.
The `id="test"` on the `FlexibleColumnLayout` is the anchor — the whole
difference from the pattern above is which mutator the detail is inserted with,
`addMidColumnPage` instead of `addContent`.

Two details the demo depends on and that are easy to leave out:

- **`mode="SingleSelectMaster"` on the list.** A `sap.m.List` defaults to
  `mode="None"`, where nothing is selectable and `selectionChange` never fires
  at all — the click does nothing and there is no error to go on.
- **`view_display_master( )` under `check_on_navigated( )` as well.** `check_on_init( )`
  is true exactly once per app instance; a value help or a sub-app returning
  raises `check_on_navigated( )` alone, and without that branch the screen keeps
  showing whatever the other app left there. See
  [Life Cycle](/cookbook/event_navigation/life_cycle#returning-from-a-sub-app-hits-check-on-navigated-not-check-on-init).

The layout is bound editable (`mv_layout`), so events like *full-screen mode* or *close detail* simply update `mv_layout` — the changed model reaches the client with the response by itself. The FCL transitions itself; no view is rebuilt.

End-to-end samples:

- `Z2UI5_CL_SMP_APP_097` — list master, `sap.ui.table.Table` in the detail with sort/filter/row actions.
- `Z2UI5_CL_SMP_APP_098` — the three-column FCL: list, detail and detail-of-detail, with the navigation that opens each one.

### Refreshing After Data Changes

All bound data lives in a **single client-side model**, regardless of which view a binding was built in — `client->_bind( ... )` always writes to that one root model. Nothing has to be pushed by hand: when the ABAP data changes, the framework sends the new values to every rendered view — main, nested, second nested — with the response of the roundtrip that changed them.

```abap
DELETE t_tab2 WHERE title = ls_arg-title.
client->view_model_update( ).   " push the new data into all rendered views
```

::: tip `nest_view_model_update` and the `view` parameter of `_bind` are obsolete
Earlier releases kept a separate model per view: bindings were tagged with `view = client->cs_view-...` and each view had its own refresh call (`nest_view_model_update( )`, `nest2_view_model_update( )`). That separation is gone — there is now one root model. The old per-view refresh methods still exist as compatibility aliases and behave like `view_model_update( )`, and the `view` parameter of `_bind` / `_bind_edit` is an inert no-op. In new code, omit the parameter and use `view_model_update( )` only.
:::

### Two Levels of Nesting

The middle column can itself host another nested view in the end column — useful for master / detail / detail-of-detail flows. abap2UI5 exposes a second method for this level:

```abap
METHOD view_display_detail_detail.
  DATA(nested) = z2ui5_cl_ui5_view_builder=>factory(
      )->ele( n = `View` ns = `mvc`
          )->a( n = `xmlns`     v = `sap.m`
          )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

          )->ele( `Page`
              )->a( n = `title` v = `Nested View`

              )->tag( `Text`
                  )->a( n = `text` v = client->_bind( mv_title ) ).

  client->nest2_view_display(
    val            = nested->stringify( )

    id             = `test`
    method_insert  = `addEndColumnPage`
    method_destroy = `removeAllEndColumnPages` ).
ENDMETHOD.
```

`nest2_view_display` works exactly like `nest_view_display` but targets the second level — typically the FCL's *end* column. `Z2UI5_CL_SMP_APP_098` walks through all three columns: a list selects a row, a row-action navigates to the end column, the layout switches to `ThreeColumnsEndExpanded`.

### When to Use Nested Views (and When Not To)

| Situation                                                       | Approach                                                                              |
| --------------------------------------------------------------- | ------------------------------------------------------------------------------------- |
| Different visual sections that update at different rates        | Nested views — re-render each piece on its own                                       |
| Master-detail, FCL columns, drill-down navigation               | Nested views — the canonical use case                                                |
| A side panel that toggles open/closed but keeps the page intact | Nested views                                                                          |
| Building one view from helper methods (still rendered as one)   | Plain ABAP composition — pass nodes between methods, no `nest_view_display` needed   |
| One full screen replacing another                               | `view_display` with the new view (or `nav_app_call` for a separate app)              |

Plain composition is the right starting point: keep helper methods that take a parent node and add children to it. Reach for nested views once the UI has clear sub-areas that need to update independently — otherwise you pay for ceremony you don't use.

### Tips

- The anchor id must be unique in the main view. The framework calls `byId` on the rendered view to find it; duplicate ids break the lookup.
- Always provide `method_destroy` when a nested slot will be replaced more than once. Forgetting it causes nested fragments to accumulate.
- Build the nested view in its own method (e.g. `view_display_detail`) and call it both from the initial render and from event handlers. Two call sites, one definition.
- If a nested view does not pick up a data change, the value did not change on the ABAP side of the roundtrip — the push is automatic (`view_model_update( )` has been an empty method since 1.143.0). If a control simply isn't there, you need `nest_view_display( )` again.
- For very large apps, look at `Z2UI5_CL_SMP_APP_104`, which loads each detail screen from a separate `z2ui5_if_app` class and renders it into the nested slot. It is an advanced pattern — start with the simpler form first.

See `Z2UI5_CL_SMP_APP_065`, `Z2UI5_CL_SMP_APP_097`, `Z2UI5_CL_SMP_APP_098`, `Z2UI5_CL_SMP_APP_104` and `Z2UI5_CL_SMP_APP_176` for runnable examples covering every variation above.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Basic Example (nest_view_display) | [`Z2UI5_CL_SMP_APP_065`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_065.clas.abap) |
| Embed Another App's View | [`Z2UI5_CL_SMP_APP_104`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_104.clas.abap) |
| Master-Detail with FlexibleColumnLayout | [`Z2UI5_CL_SMP_APP_097`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_097.clas.abap) |
| Three Columns with FlexibleColumnLayout | [`Z2UI5_CL_SMP_APP_098`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_098.clas.abap) |
| Dynamic Content in a Nested View | [`Z2UI5_CL_SMP_APP_176`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_176.clas.abap) |

<!-- samples:end -->
