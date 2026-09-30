---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_173
  - z2ui5_cl_smp_app_176
---
# XML Templating

XML Templating is a **UI5 preprocessor feature**, not an abap2UI5 invention. The UI5 runtime understands a small set of instructions in the `template` XML namespace — `template:repeat`, `template:if`, `template:then`, `template:elseif`, `template:else`, `template:with` — and expands them into plain XML *before* the control tree is created. abap2UI5 exposes these instructions through the fluent builder so you can drive the expansion from ABAP data.

It is the technique behind every **metadata-driven** UI: the input of the expansion is a *meta model* — data about the view, such as which columns a table has or which fields of which type a form shows — rather than the data the controls display. A UI5 app hands the preprocessor the OData meta model of its service; in abap2UI5 the meta model is plain ABAP data you bind like any other attribute (see [Meta Model](#meta-model-a-form-from-a-field-catalog) below).

See the official UI5 references for the underlying mechanics:
[XML Templating](https://sapui5.hana.ondemand.com/sdk/#/topic/5ee619fc1370463ea674ee04b65ed83b),
[`template:repeat`](https://sapui5.hana.ondemand.com/sdk/#/topic/512e545ba66f4214ba0de1eb56f319e1),
[`template:if`](https://sapui5.hana.ondemand.com/sdk/#/topic/fc185952184c48618ef46306a1517f8c).

## How It Works

Templating happens **once**, at view instantiation, against a JSON model. The preprocessor walks the XML, evaluates each `template:` instruction against that model, and replaces the instruction with the resulting XML. After that the control tree is built from the expanded XML and normal data binding takes over.

This timing has two consequences:

- **Expansion is build-time.** A `template:repeat` over an internal table produces a fixed number of controls; the expanded XML is what UI5 renders.
- **Data changes do not re-template.** If the data driving the template changes, the existing expansion stays as is. To pick up the change you must rebuild the view (`view_display`) or the templated fragment (`nest_view_display`) — see [Re-rendering](#re-rendering) below.

abap2UI5 wires up the templating model for you. Every variable you bind with `client->_bind( ... )` is reachable inside templates via the `template>` model prefix:

| ABAP binding                       | Path inside templates       |
| ---------------------------------- | --------------------------- |
| `client->_bind( mt_layout )`       | `{template>/MT_LAYOUT}`     |
| `client->_bind( mv_flag )`    | `{template>/MV_FLAG}`    |

The `template>` model is the templating engine's view of the data — distinct from the default model used by runtime bindings like `{MT_DATA}`.

## `template:repeat` — Loops

`template:repeat` clones its children once per row of the bound list. Use it when the **structure** of the view (e.g. which columns a table has) depends on data:

```abap
mt_layout = VALUE #( ( fname = `NAME` merge = `false` visible = `true`  )
                     ( fname = `DATE` merge = `false` visible = `true`  )
                     ( fname = `AGE`  merge = `false` visible = `false` ) ).
client->_bind( mt_layout ).

view->ele( `Table`
    )->a( n = `items` v = client->_bind( mt_data )

    )->ele( `columns`
        )->ele( n = `repeat` ns = `template`
            )->a( n = `list` v = `{template>/MT_LAYOUT}`
            )->a( n = `var`  v = `L0`

            )->ele( `Column`
                )->a( n = `mergeDuplicates` v = `{L0>MERGE}`
                )->a( n = `visible`         v = `{L0>VISIBLE}`

                )->tag( `Text`
                    )->a( n = `text` v = `{L0>FNAME}`
            )->end(
        )->end(
    )->end(

    )->ele( `items`
        )->ele( `ColumnListItem`
            )->ele( `cells`
                )->ele( n = `repeat` ns = `template`
                    )->a( n = `list` v = `{template>/MT_LAYOUT}`
                    )->a( n = `var`  v = `L1`

                    )->tag( `ObjectIdentifier`
                        )->a( n = `text` v = `{= '{' + ${L1>FNAME} + '}' }` ).
```

Notes on the snippet:

- `list` is the binding path that drives the loop; `var` is the alias used inside the loop body (here `L0` for the column headers, `L1` for the cells). `template` is an element namespace like any other, so the builder needs nothing beyond `ns = template` on the element — the prefix itself is declared once on the view's root.
- Inside the loop, `{L0>FNAME}` is a templating-time read — it ends up as the literal string `NAME`/`DATE`/`AGE` in the expanded XML.
- `{= '{' + ${L1>FNAME} + '}' }` is an [expression binding](https://sapui5.hana.ondemand.com/sdk/#/topic/daf6852a04b44d118963968a1239d2c0) that **constructs another binding string at templating time**. With `L1>FNAME = NAME` it expands to `text="{NAME}"`, which becomes a normal runtime binding against the row of `mt_data`. This is the standard pattern for templated tables: outer loop builds the columns, inner loop builds the cells, expression binding wires each cell to the right field of the row.
- `list` is a list binding like any other, so it takes the binding-info form too: `{path: '...', startIndex: 0, length: 2}` cuts the loop to a slice of the table — see [Meta Model](#meta-model-a-form-from-a-field-catalog) below.

The full sample is `Z2UI5_CL_SMP_APP_173`.

## The Whole Thing, Runnable

The fragments above are two halves of one view. Press **Run** to see the
expansion happen — three rows of data, three columns built by the outer loop,
and each cell wired to its field by the inner one:

```abap
CLASS z2ui5_cl_sample_templating DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_layout,
        fname   TYPE string,
        merge   TYPE string,
        visible TYPE string,
      END OF ty_s_layout.

    TYPES:
      BEGIN OF ty_s_row,
        name TYPE string,
        date TYPE string,
        age  TYPE string,
      END OF ty_s_row.

    DATA mt_layout TYPE STANDARD TABLE OF ty_s_layout WITH EMPTY KEY.
    DATA mt_data   TYPE STANDARD TABLE OF ty_s_row    WITH EMPTY KEY.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_sample_templating IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      mt_layout = VALUE #( ( fname = `NAME` merge = `true`  visible = `true` )
                           ( fname = `DATE` merge = `false` visible = `true` )
                           ( fname = `AGE`  merge = `false` visible = `true` ) ).

      mt_data = VALUE #( ( name = `Alice` date = `2026-01-15` age = `34` )
                         ( name = `Bob`   date = `2026-02-03` age = `28` )
                         ( name = `Cleo`  date = `2026-03-21` age = `41` ) ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`          v = `sap.m`
              )->a( n = `xmlns:mvc`      v = `sap.ui.core.mvc`
              )->a( n = `xmlns:template` v = `http://schemas.sap.com/sapui5/extension/sap.ui.core.template/1`

              )->ele( `Page`
                  )->a( n = `title` v = `XML Templating`

                  )->ele( `Table`
                      )->a( n = `items` v = client->_bind( mt_data )

                      )->ele( `columns`
                          )->ele( n = `repeat` ns = `template`
                              )->a( n = `list` v = `{template>/MT_LAYOUT}`
                              )->a( n = `var`  v = `L0`

                              )->ele( `Column`
                                  )->a( n = `mergeDuplicates` v = `{L0>MERGE}`
                                  )->a( n = `visible`         v = `{L0>VISIBLE}`

                                  )->tag( `Text`
                                      )->a( n = `text` v = `{L0>FNAME}`

                              )->end(

                          )->end(

                      )->end(

                      )->ele( `items`
                          )->ele( `ColumnListItem`
                              )->ele( `cells`
                                  )->ele( n = `repeat` ns = `template`
                                      )->a( n = `list` v = `{template>/MT_LAYOUT}`
                                      )->a( n = `var`  v = `L1`

                                      )->tag( `ObjectIdentifier`
                                          )->a( n = `text` v = `{= '{' + ${L1>FNAME} + '}' }` ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

Flip a row's `visible` to `false` and re-run: the column is gone from the
expanded XML entirely, not merely hidden.

## `template:if` / `template:then` / `template:else` — Conditionals

`template:if` evaluates an expression against the templating model and keeps or drops its children accordingly. With a `template:then` / `template:else` pair you get a two-branch switch:

```abap
client->_bind( mv_flag ).

view->ele( n = `if` ns = `template`
    )->a( n = `test` v = `{template>/MV_FLAG}`

    )->ele( n = `then` ns = `template`
        )->tag( n = `Icon` ns = `core`
            )->a( n = `src`   v = `sap-icon://accept`
            )->a( n = `color` v = `green`
    )->end(

    )->ele( n = `else` ns = `template`
        )->tag( n = `Icon` ns = `core`
            )->a( n = `src`   v = `sap-icon://decline`
            )->a( n = `color` v = `red` ).
```

The test argument follows the same rules as in UI5: any binding expression is fine, and the string `"false"` is treated as boolean `false` (a UI5 convenience). For richer conditions use expression binding, e.g. `` `{= ${template>/MV_COUNT} > 0 }` ``.

`template:elseif` is also supported by UI5, and needs nothing extra here: it is the same `ele` call with a different name, `ns = template` and a `test` attribute, placed between the `then` and the `else`. That is the point of the builder — every templating instruction UI5 has is reachable the moment UI5 has it, without waiting for a method. The form in the next section picks one of four controls with it.

## Meta Model: a Form from a Field Catalog

The table above is driven by a flat list. A form usually needs more: fields in groups, and a control that depends on the type of the field. That is a meta model in the sense of UI5's OData meta model — a description of the entity — and in abap2UI5 it is a deep ABAP structure:

```abap
TYPES:
  BEGIN OF ty_s_field,
    name  TYPE string,
    label TYPE string,
    type  TYPE string,
  END OF ty_s_field,
  ty_t_field TYPE STANDARD TABLE OF ty_s_field WITH EMPTY KEY.

TYPES:
  BEGIN OF ty_s_group,
    title   TYPE string,
    t_field TYPE ty_t_field,
  END OF ty_s_group,
  ty_t_group TYPE STANDARD TABLE OF ty_s_group WITH EMPTY KEY.

TYPES:
  BEGIN OF ty_s_meta,
    entity  TYPE string,
    t_group TYPE ty_t_group,
  END OF ty_s_meta.

ms_meta = VALUE #(
  entity  = `Person`
  t_group = VALUE #(
    ( title   = `General`
      t_field = VALUE #( ( name = `NAME` label = `Name`       type = `STRING` )
                         ( name = `DATE` label = `Birth Date` type = `DATE` )
                         ( name = `CITY` label = `City`       type = `STRING` ) ) )
    ( title   = `Details`
      t_field = VALUE #( ( name = `AGE`    label = `Age`    type = `NUMBER` )
                         ( name = `ACTIVE` label = `Active` type = `BOOLEAN` ) ) ) ) ).
```

Four instructions turn it into a form:

- **`template:with`** gives the meta model a short name: `path = template>/MS_META`, `var = meta`, and everything inside reads `{meta>...}`. The `path` attribute is a plain path, without braces.
- **Nested `template:repeat`** — the outer loop runs over the groups and writes a `core:Title` per group, the inner one runs over `{group>T_FIELD}`, a path relative to the outer loop's variable, and writes a `Label` and a control per field.
- **`template:if` / `template:elseif` / `template:else`** picks that control from the field's type.
- **`startIndex` / `length`** in the binding info of a `list` cut the loop — the header shows the first two fields of the first group only.

The values the controls show are ordinary runtime bindings. The container carries an element binding to the record (`binding = {/MS_DETAIL}`), and each control's value is composed at templating time from the field name, the same expression-binding trick as the table cells above:

```abap
DATA(box) = view->ele( n = `with` ns = `template`
    )->a( n = `path` v = `template>/MS_META`
    )->a( n = `var`  v = `meta`
    )->ele( `VBox`
        )->a( n = `binding` v = `{/MS_DETAIL}` ).

box->ele( `ObjectHeader`
    )->a( n = `title` v = `{meta>ENTITY}`
    )->ele( `attributes`
        )->ele( n = `repeat` ns = `template`
            )->a( n = `list` v = `{path: 'meta>T_GROUP/0/T_FIELD', startIndex: 0, length: 2}`
            )->a( n = `var`  v = `head`
            )->tag( `ObjectAttribute`
                )->a( n = `title` v = `{head>LABEL}`
                )->a( n = `text`  v = `{= '{' + ${head>NAME} + '}' }` ).

DATA(field) = box->ele( n = `SimpleForm` ns = `form`
    )->a( n = `editable` b = abap_true
    )->a( n = `layout`   v = `ResponsiveGridLayout`
    )->ele( n = `content` ns = `form`
        )->ele( n = `repeat` ns = `template`
            )->a( n = `list` v = `{meta>T_GROUP}`
            )->a( n = `var`  v = `group`
            )->tag( n = `Title` ns = `core`
                )->a( n = `text` v = `{group>TITLE}`
            )->ele( n = `repeat` ns = `template`
                )->a( n = `list` v = `{group>T_FIELD}`
                )->a( n = `var`  v = `field` ).

field->tag( `Label`
    )->a( n = `text` v = `{field>LABEL}` ).

field->ele( n = `if` ns = `template`
    )->a( n = `test` v = `{= ${field>TYPE} === 'BOOLEAN' }`
    )->ele( n = `then` ns = `template`
        )->tag( `Switch`
            )->a( n = `state` v = `{= '{' + ${field>NAME} + '}' }`
    )->end(
    )->ele( n = `elseif` ns = `template`
        )->a( n = `test` v = `{= ${field>TYPE} === 'DATE' }`
        )->tag( `DatePicker`
            )->a( n = `value`       v = `{= '{' + ${field>NAME} + '}' }`
            )->a( n = `valueFormat` v = `yyyy-MM-dd`
    )->end(
    )->ele( n = `elseif` ns = `template`
        )->a( n = `test` v = `{= ${field>TYPE} === 'NUMBER' }`
        )->tag( `StepInput`
            )->a( n = `value` v = `{= '{' + ${field>NAME} + '}' }`
    )->end(
    )->ele( n = `else` ns = `template`
        )->tag( `Input`
            )->a( n = `value` v = `{= '{' + ${field>NAME} + '}' }` ).
```

Move a field to the other group, reorder it or change its type, and the form follows — no view code changes. In the sample the paths are composed from `client->_bind( val = ms_meta path = abap_true )` rather than written as literals; the literals above only keep the snippet short.

The full sample is `Z2UI5_CL_SMP_APP_173`.

::: warning Only the `template>` model
The frontend hands the preprocessor exactly one model: the view's JSON model, as `template`. A UI5 OData meta model — `{meta>/...}` against the `$metadata` of a service, `sap.ui.model.odata.AnnotationHelper` — is not available at templating time, not even with an OData default model from `switch_default_model_path`. Read the metadata in ABAP and bind the result, as above.
:::

## Re-rendering

Because templating runs once, the view must be rebuilt for changes in the templating model to take effect. There are two strategies:

**Rebuild the whole view** — simplest, works for most apps. After the user toggles a flag, render the view again:

```abap
CASE client->get( )-event.
  WHEN `CHANGE_FLAG`.
    view_display( ).   " builds and ships the view again
ENDCASE.
```

This is the pattern used in `Z2UI5_CL_SMP_APP_173`: the switch fires `CHANGE_FLAG`, `view_display` runs, the new value of `mv_flag` flows through `template:if`, and the icon swaps.

**Rebuild only the templated fragment** — keeps a stable shell on screen and replaces a nested piece. `Z2UI5_CL_SMP_APP_176` separates the main view from the templated table:

```abap
" Main view: built once, stays on screen
DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
    )->ele( n = `View` ns = `mvc`
        )->a( n = `xmlns`     v = `sap.m`
        )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

        )->ele( `Shell`
            )->ele( `Page`
                )->a( n = `title` v = `Main View`
                )->tag( `VBox`
                    )->a( n = `id` v = `box_nest` ).   " ...

client->view_display( view->stringify( ) ).

" Nested templated view: inserted into the main view by id.
" The template namespace has to be declared on the nested view's root.
DATA(nested) = z2ui5_cl_ui5_view_builder=>factory(
    )->ele( n = `View` ns = `mvc`
        )->a( n = `xmlns`          v = `sap.m`
        )->a( n = `xmlns:mvc`      v = `sap.ui.core.mvc`
        )->a( n = `xmlns:template` v = `http://schemas.sap.com/sapui5/extension/sap.ui.core.template/1`

        )->ele( `Shell`
            )->ele( `Page`
                )->a( n = `title` v = `Nested View`

                )->ele( `Table`
                    )->a( n = `items` v = client->_bind( mt_data )

                    )->ele( `columns`
                        )->ele( n = `repeat` ns = `template`
                            )->a( n = `list` v = `{template>/MT_LAYOUT}`
                            )->a( n = `var`  v = `L0`

                            )->ele( `Column` ).   " ...

client->nest_view_display( val            = nested->stringify( )
                           id             = `box_nest`
                           method_insert  = `addItem`
                           method_destroy = `removeAllItems` ).

```

`nest_view_display` targets a control in the existing view by id (`box_nest`) and appends/replaces the nested view there. Give the nested view a container of its own: `method_destroy` empties the whole aggregation, so a nested view inserted into the page's `content` would take everything else on the page with it on the next render. To refresh the templated piece on a data change, call `nest_view_display` again from the event handler — the main view is not sent and stays as it is:

```abap
ELSEIF client->check_on_event( `TOGGLE_AGE` ).
  ASSIGN mt_layout[ fname = `AGE` ] TO FIELD-SYMBOL(<age>).
  <age>-visible = COND #( WHEN <age>-visible = `true` THEN `false` ELSE `true` ).
  nest_view_display( ).
```

## Templating vs ABAP-side Composition

The two demo apps both build *dynamic* views, but with different mechanics. Pick based on what the dynamic part actually is:

| Situation                                                            | Approach                                                                             |
| -------------------------------------------------------------------- | ------------------------------------------------------------------------------------ |
| Whether a control exists at all, decided once per render             | ABAP `IF` around the builder call — simpler, no `template:` namespace involved       |
| Number of controls comes from an internal table, computed once       | ABAP `LOOP` over the table, calling the builder for each row                         |
| Reusable XML fragment that UI5 itself should expand against metadata | `template:repeat` / `template:if` — keeps the templating logic in the view layer     |
| Cells of a table where the column set itself is data-driven          | `template:repeat` — UI5 expands columns and cell bindings in one pass, as in app 173 |
| A form or table described by a meta model (groups, fields, types)    | `template:with` + nested `template:repeat` + `template:if`/`elseif`, as in app 173   |

Plain ABAP control flow covers most cases and is easier to debug. Reach for `template:` when you want the expansion to live in the view (closer to standard UI5 patterns) or when you are mapping a metadata-style structure onto controls.

## Tips

- Always bind the data that drives a template (`client->_bind`) **before** the builder call that references it. The binding registers the path that `{template>/...}` resolves against.
- Inside `template:repeat`, prefer `{var>FIELD}` over deeper paths — it keeps the body readable and lets you nest repeats with distinct `var` names (`L0`, `L1`, ...) without collisions.
- Use expression binding (`{= ... }`) when the value you need is a binding string itself. Templating-time expressions can read `${var>...}` and concatenate strings, which is how dynamic cell bindings are assembled.
- If a templated control does not update after a data change, you forgot to rebuild — call `view_display` or `nest_view_display` again.

See `Z2UI5_CL_SMP_APP_173` for `template:repeat`, `template:if`/`elseif`/`else`, `template:with` and a form generated from a meta model in a single view, and `Z2UI5_CL_SMP_APP_176` for the stable-shell / templated-nested-view pattern with a re-render on an event.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/playground/samples/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Metadata-Driven Table and Form | [`Z2UI5_CL_SMP_APP_173`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_173.clas.abap) |
| Dynamic Content in a Nested View | [`Z2UI5_CL_SMP_APP_176`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_176.clas.abap) |

<!-- samples:end -->
