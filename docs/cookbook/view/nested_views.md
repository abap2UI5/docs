---
outline: [2, 4]
---
# Nested Views

A **nested view** in abap2UI5 is a separate XML view fragment that you inject into a *placeholder* inside another view. The main view stays on screen; only the nested fragment is rendered (and later re-rendered or refreshed) independently. This is the standard pattern for master-detail screens, side panels, tab content, and anywhere you want one part of the UI to update without rebuilding the whole page.

If you know SAPUI5's [nested views](https://sapui5.hana.ondemand.com/sdk/#/topic/df8c9c3d6f2a4d728ba7d6f4cb6c6d35) (`<mvc:XMLView viewName="..."/>`), the goal is the same — split the UI into independently managed pieces. In abap2UI5 the wiring is done from ABAP at runtime: instead of referencing a static view file, you build the nested view's XML in ABAP and tell the client to plug it into a named slot.

#### The Basic Pattern

Two ingredients are needed:

1. **An anchor in the main view** — any control with an `id`. The nested view will be inserted *into* this control.
2. **A nested view + a `nest_view_display` call** — builds the fragment and ships it to the named anchor.

```abap
" 1) Main view with an anchor
DATA(lo_view) = z2ui5_cl_xml_view=>factory( ).
DATA(page)    = lo_view->shell(
    )->page( title = `Main View`
             id    = `test` ).        " <-- the anchor id

page->content(
  )->button( text  = `Re-render only the nested view`
             press = client->_event( `NEST` ) ).

" 2) Nested view, built like any other view
DATA(lo_view_nested) = z2ui5_cl_xml_view=>factory(
  )->page( `Nested View`
    )->input( client->_bind( mv_input_nest )
    )->button( text  = `event`
               press = client->_event( `TEST` ) ).

IF client->check_on_init( ).
  client->view_display( lo_view->stringify( ) ).
ENDIF.

CASE client->get( )-event.
  WHEN `NEST`.
    client->nest_view_display(
        val           = lo_view_nested->stringify( )
        id            = `test`               " target the anchor
        method_insert = `addContent` ).      " UI5 mutator on that control
ENDCASE.
```

What happens at runtime: `view_display` paints the main view; the page with `id="test"` sits on screen. When the user clicks the button, `nest_view_display` ships the nested XML to the client, which calls `addContent( ... )` on the control with that id. The nested fragment appears inside the page — without re-rendering the page itself.

The full pattern (re-render everything vs. main only vs. nested only) is in `Z2UI5_CL_DEMO_APP_065`.

#### `nest_view_display` Parameters

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

#### Independent Re-rendering

The whole point of nested views is to re-render only what changed. Four calls cover the common needs:

| Call                              | What it does                                                                                  |
| --------------------------------- | --------------------------------------------------------------------------------------------- |
| `client->view_display( ... )`     | Replaces the main view's XML. The anchor is recreated, so any nested content is lost too.    |
| `client->nest_view_display( ... )`| Replaces only the nested view. The main view stays on screen.                                |
| `client->nest_view_model_update( )` | Pushes current ABAP data values into the **already-rendered** nested view. No re-render.   |
| `client->nest_view_destroy( )`    | Removes the nested view from the frontend without touching the main view.                    |

The matching calls for the main view are `client->view_model_update( )` — push data changes into the rendered main view without re-rendering it — and `client->view_destroy( )`. The second nested slot has the same trio: `nest2_view_display( )`, `nest2_view_model_update( )` and `nest2_view_destroy( )`.

A rule of thumb:

- **Layout changed** (different controls, new columns, new sections) → `view_display` / `nest_view_display`.
- **Only the data changed** (a flag flipped, a row added to a bound table) → `view_model_update` / `nest_view_model_update`.

`Z2UI5_CL_DEMO_APP_065` shows the difference between the three options in a single screen with one button per call.

#### Master-Detail with `FlexibleColumnLayout`

The most common real-world use: a master list on the left, detail content on the right. `sap.f.FlexibleColumnLayout` is the standard container; abap2UI5 nests the detail view into its middle column.

```abap
" Master view — built once
DATA(page) = z2ui5_cl_xml_view=>factory(
  )->page( title = `abap2UI5 - Master Detail` ).

DATA(lr_master) = page->flexible_column_layout(
                       layout = client->_bind( mv_layout )
                       id     = `test`                            " anchor
                     )->begin_column_pages( ).

lr_master->list( items = client->_bind( t_tab )
                 selectionchange = client->_event( `SELCHANGE` )
  )->standard_list_item( title    = `{TITLE}`
                         selected = `{SELECTED}` ).

client->view_display( page->stringify( ) ).
```

When the user picks a row, a detail view is rendered into the middle column:

```abap
METHOD view_display_detail.
  DATA(lo_view_nested) = z2ui5_cl_xml_view=>factory( ).
  DATA(page) = lo_view_nested->page( `Nested View` ).

  page->ui_table( rows = client->_bind( t_tab2 ) ).
  " ...columns, toolbar, row actions...

  client->nest_view_display(
    val            = lo_view_nested->stringify( )
    id             = `test`
    method_insert  = `addMidColumnPage`
    method_destroy = `removeAllMidColumnPages` ).
ENDMETHOD.
```

The layout is bound editable (`mv_layout`), so events like *full-screen mode* or *close detail* simply update `mv_layout` and call `view_model_update` / `nest_view_model_update`. The FCL transitions itself; no view is rebuilt.

End-to-end samples:

- `Z2UI5_CL_DEMO_APP_069` — tree master, two interchangeable detail apps (`addMidColumnPage`).
- `Z2UI5_CL_DEMO_APP_097` — list master, `sap.ui.table.Table` in the detail with sort/filter/row actions.
- `Z2UI5_CL_DEMO_APP_085` — full master-detail with an `ObjectPageLayout` as the nested detail, including search, sort, and the FCL fullscreen toggle.

#### Refreshing the Right View

All bound data lives in a **single client-side model**, regardless of which view a binding was built in — `client->_bind( ... )` always writes to that one root model. What differs is *which rendered view you refresh* after the ABAP data changes: call the update method that matches the view.

```abap
DELETE t_tab2 WHERE title = ls_arg-title.
client->nest_view_model_update( ).   " push the new data into the nested view
```

| Rendered view | Refresh call                          |
| ------------- | ------------------------------------- |
| main          | `client->view_model_update( )`        |
| nested        | `client->nest_view_model_update( )`   |
| nested (2nd)  | `client->nest2_view_model_update( )`  |

::: tip `_bind`'s `view` parameter is obsolete
Earlier releases kept a separate model per view and asked you to tag each binding with `view = client->cs_view-...` so the framework knew which model to refresh. That separation is gone — there is now one root model — and the `view` parameter of `_bind` / `_bind_edit` is an inert no-op kept only for backward compatibility. Omit it, and pick the target view through the matching `..._view_model_update( )` call instead.
:::

#### Two Levels of Nesting

The middle column can itself host another nested view in the end column — useful for master / detail / detail-of-detail flows. abap2UI5 exposes a second method for this level:

```abap
METHOD view_display_detail_detail.
  DATA(lo_view_nested) = z2ui5_cl_xml_view=>factory( ).
  lo_view_nested->page( `Nested View`
    )->text( client->_bind( mv_title ) ).

  client->nest2_view_display(
    val            = lo_view_nested->stringify( )
    id             = `test`
    method_insert  = `addEndColumnPage`
    method_destroy = `removeAllEndColumnPages` ).
ENDMETHOD.
```

`nest2_view_display` works exactly like `nest_view_display` but targets the second level — typically the FCL's *end* column. `Z2UI5_CL_DEMO_APP_098` walks through all three columns: a list selects a row, a row-action navigates to the end column, the layout switches to `ThreeColumnsEndExpanded`.

#### When to Use Nested Views (and When Not To)

| Situation                                                       | Approach                                                                              |
| --------------------------------------------------------------- | ------------------------------------------------------------------------------------- |
| Different visual sections that update at different rates        | Nested views — re-render each piece on its own                                       |
| Master-detail, FCL columns, drill-down navigation               | Nested views — the canonical use case                                                |
| A side panel that toggles open/closed but keeps the page intact | Nested views                                                                          |
| Building one view from helper methods (still rendered as one)   | Plain ABAP composition — pass nodes between methods, no `nest_view_display` needed   |
| One full screen replacing another                               | `view_display` with the new view (or `nav_app_call` for a separate app)              |

Plain composition is the right starting point: keep helper methods that take a parent node and add children to it. Reach for nested views once the UI has clear sub-areas that need to update independently — otherwise you pay for ceremony you don't use.

#### Tips

- The anchor id must be unique in the main view. The framework calls `byId` on the rendered view to find it; duplicate ids break the lookup.
- Always provide `method_destroy` when a nested slot will be replaced more than once. Forgetting it causes nested fragments to accumulate.
- Build the nested view in its own method (e.g. `view_display_detail`) and call it both from the initial render and from event handlers. Two call sites, one definition.
- If a nested view does not pick up a data change, you probably need `nest_view_model_update( )`; if a control simply isn't there, you need `nest_view_display( )` again.
- For very large apps, look at `Z2UI5_CL_DEMO_APP_104`, which loads each detail screen from a separate `z2ui5_if_app` class and renders it into the nested slot. It is an advanced pattern — start with the simpler form first.

See `Z2UI5_CL_DEMO_APP_065`, `Z2UI5_CL_DEMO_APP_069`, `Z2UI5_CL_DEMO_APP_085`, `Z2UI5_CL_DEMO_APP_097`, `Z2UI5_CL_DEMO_APP_098`, and `Z2UI5_CL_DEMO_APP_104` for runnable examples covering every variation above.
