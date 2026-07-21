---
outline: [2, 4]
---
# XML Templating

XML Templating is a **UI5 preprocessor feature**, not an abap2UI5 invention. The UI5 runtime understands a small set of instructions in the `template` XML namespace — `template:repeat`, `template:if`, `template:then`, `template:else`, `template:with` — and expands them into plain XML *before* the control tree is created. abap2UI5 exposes these instructions through the fluent builder so you can drive the expansion from ABAP data.

See the official UI5 references for the underlying mechanics:
[XML Templating](https://sapui5.hana.ondemand.com/sdk/#/topic/5ee619fc1370463ea674ee04b65ed83b),
[`template:repeat`](https://sapui5.hana.ondemand.com/sdk/#/topic/512e545ba66f4214ba0de1eb56f319e1),
[`template:if`](https://sapui5.hana.ondemand.com/sdk/#/topic/fc185952184c48618ef46306a1517f8c).

#### How It Works

Templating happens **once**, at view instantiation, against a JSON model. The preprocessor walks the XML, evaluates each `template:` instruction against that model, and replaces the instruction with the resulting XML. After that the control tree is built from the expanded XML and normal data binding takes over.

This timing has two consequences:

- **Expansion is build-time.** A `template:repeat` over an internal table produces a fixed number of controls; the expanded XML is what UI5 renders.
- **Data changes do not re-template.** If the data driving the template changes, the existing expansion stays as is. To pick up the change you must rebuild the view (`view_display`) or the templated fragment (`nest_view_display`) — see [Re-rendering](#re-rendering) below.

abap2UI5 wires up the templating model for you. Every variable you bind with `client->_bind( ... )` or `client->_bind( ... )` is reachable inside templates via the `template>` model prefix:

| ABAP binding                       | Path inside templates       |
| ---------------------------------- | --------------------------- |
| `client->_bind( mt_layout )`       | `{template>/MT_LAYOUT}`     |
| `client->_bind( mv_flag )`    | `{template>/MV_FLAG}`    |

The `template>` model is the templating engine's view of the data — distinct from the default model used by runtime bindings like `{MT_DATA}`.

#### `template:repeat` — Loops

`template:repeat` clones its children once per row of the bound list. Use it when the **structure** of the view (e.g. which columns a table has) depends on data:

```abap
mt_layout = VALUE #( ( fname = `NAME` merge = `false` visible = `true`  )
                     ( fname = `DATE` merge = `false` visible = `true`  )
                     ( fname = `AGE`  merge = `false` visible = `false` ) ).
client->_bind( mt_layout ).

view->table( client->_bind( mt_data )
  )->columns(
    )->template_repeat( list = `{template>/MT_LAYOUT}`
                        var  = `L0`
      )->column( mergeduplicates = `{L0>MERGE}`
                 visible         = `{L0>VISIBLE}`
        )->text( `{L0>FNAME}` )->get_parent(
    )->get_parent( )->get_parent(
    )->items(
      )->column_list_item(
        )->cells(
          )->template_repeat( list = `{template>/MT_LAYOUT}`
                              var  = `L1`
            )->object_identifier( text = `{= '{' + ${L1>FNAME} + '}' }` ).
```

Notes on the snippet:

- `list` is the binding path that drives the loop; `var` is the alias used inside the loop body (here `L0` for the column headers, `L1` for the cells).
- Inside the loop, `{L0>FNAME}` is a templating-time read — it ends up as the literal string `NAME`/`DATE`/`AGE` in the expanded XML.
- `{= '{' + ${L1>FNAME} + '}' }` is an [expression binding](https://sapui5.hana.ondemand.com/sdk/#/topic/daf6852a04b44d118963968a1239d2c0) that **constructs another binding string at templating time**. With `L1>FNAME = NAME` it expands to `text="{NAME}"`, which becomes a normal runtime binding against the row of `mt_data`. This is the standard pattern for templated tables: outer loop builds the columns, inner loop builds the cells, expression binding wires each cell to the right field of the row.
- `template_repeat` accepts the same optional attributes as the UI5 instruction (`startIndex`, `length`) plus list-binding extras like sorters and filters.

The full sample is `Z2UI5_CL_DEMO_APP_173`.

#### `template:if` / `template:then` / `template:else` — Conditionals

`template:if` evaluates an expression against the templating model and keeps or drops its children accordingly. With a `template:then` / `template:else` pair you get a two-branch switch:

```abap
client->_bind( mv_flag ).

view->template_if( `{template>/MV_FLAG}`
  )->template_then(
    )->icon( src   = `sap-icon://accept`
             color = `green` )->get_parent(
  )->template_else(
    )->icon( src   = `sap-icon://decline`
             color = `red` ).
```

The test argument follows the same rules as in UI5: any binding expression is fine, and the string `"false"` is treated as boolean `false` (a UI5 convenience). For richer conditions use expression binding, e.g. `` `{= ${template>/MV_COUNT} > 0 }` ``.

`template:elseif` is also supported by UI5; check `Z2UI5_CL_XML_VIEW` for the corresponding fluent method or fall back to the generic builder (see [Definition](/cookbook/view/definition#the-fully-generic-builder)).

#### Re-rendering

Because templating runs once, the view must be rebuilt for changes in the templating model to take effect. There are two strategies:

**Rebuild the whole view** — simplest, works for most apps. After the user toggles a flag, render the view again:

```abap
CASE client->get( )-event.
  WHEN `CHANGE_FLAG`.
    view_display( ).   " builds and ships the view again
ENDCASE.
```

This is the pattern used in `Z2UI5_CL_DEMO_APP_173`: the switch fires `CHANGE_FLAG`, `view_display` runs, the new value of `mv_flag` flows through `template:if`, and the icon swaps.

**Rebuild only the templated fragment** — keeps a stable shell on screen and replaces a nested piece. `Z2UI5_CL_DEMO_APP_176` separates the main view from the templated table:

```abap
" Main view: built once, stays on screen
DATA(lo_view) = z2ui5_cl_xml_view=>factory( ).
lo_view->shell( )->page( title = `Main View` id = `test` ... ).
client->view_display( lo_view->stringify( ) ).

" Nested templated view: inserted into the main view by id
DATA(lo_view_nested) = z2ui5_cl_xml_view=>factory( ).
lo_view_nested->shell( )->page( `Nested View`
  )->table( client->_bind( mt_data )
  )->columns(
    )->template_repeat( list = `{template>/MT_LAYOUT}` var = `L0`
      )->column( ... ) " ...

client->nest_view_display( val           = lo_view_nested->stringify( )
                           id            = `test`
                           method_insert = `addContent` ).
```

`nest_view_display` targets a control in the existing view by id (`test`) and appends/replaces the nested view there. To refresh the templated piece on a data change, call `nest_view_display` again from the event handler — the main view is left untouched.

#### Templating vs ABAP-side Composition

The two demo apps both build *dynamic* views, but with different mechanics. Pick based on what the dynamic part actually is:

| Situation                                                            | Approach                                                                             |
| -------------------------------------------------------------------- | ------------------------------------------------------------------------------------ |
| Whether a control exists at all, decided once per render             | ABAP `IF` around the builder call — simpler, no `template:` namespace involved       |
| Number of controls comes from an internal table, computed once       | ABAP `LOOP` over the table, calling the builder for each row                         |
| Reusable XML fragment that UI5 itself should expand against metadata | `template:repeat` / `template:if` — keeps the templating logic in the view layer     |
| Cells of a table where the column set itself is data-driven          | `template:repeat` — UI5 expands columns and cell bindings in one pass, as in app 173 |

Plain ABAP control flow covers most cases and is easier to debug. Reach for `template:` when you want the expansion to live in the view (closer to standard UI5 patterns) or when you are mapping a metadata-style structure onto controls.

#### Tips

- Always bind the data that drives a template (`client->_bind`) **before** the builder call that references it. The binding registers the path that `{template>/...}` resolves against.
- Inside `template:repeat`, prefer `{var>FIELD}` over deeper paths — it keeps the body readable and lets you nest repeats with distinct `var` names (`L0`, `L1`, ...) without collisions.
- Use expression binding (`{= ... }`) when the value you need is a binding string itself. Templating-time expressions can read `${var>...}` and concatenate strings, which is how dynamic cell bindings are assembled.
- If a templated control does not update after a data change, you forgot to rebuild — call `view_display` or `nest_view_display` again.

See `Z2UI5_CL_DEMO_APP_173` for `template:repeat` + `template:if` in a single view and `Z2UI5_CL_DEMO_APP_176` for the stable-shell / templated-nested-view pattern.
