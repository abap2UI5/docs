---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_493
  - z2ui5_cl_smp_app_050
  - z2ui5_cl_smp_app_255
---
# Definition

abap2UI5 uses [SAP UI5](https://sapui5.hana.ondemand.com) on the frontend without modification. Whatever your ABAP code sends to the browser is a **standard UI5 XML view** — the same XML you would write in any UI5 freestyle project.

The consequence: **everything in the UI5 SDK works in abap2UI5 1:1 when you write the XML directly**. Any control, any property, any namespace from the [UI5 Demo Kit](https://sapui5.hana.ondemand.com/sdk) is available. Copy the XML, paste it into your ABAP class, and it renders.

#### Sending a View

The simplest case: build an XML string and ship it to the client.

```abap
  METHOD z2ui5_if_app~main.

    client->view_display(
        |<mvc:View xmlns="sap.m" xmlns:core="sap.ui.core" xmlns:mvc="sap.ui.core.mvc" | &
        |          displayBlock="true" height="100%">| &
        |  <Shell>| &
        |     <Page title="My title">| &
        |         <Text text="My text"/>| &
        |     </Page>| &
        |  </Shell>| &
        |</mvc:View>| ).

ENDMETHOD.
```

Swap `<Text>` for any other control from the SDK; the framework doesn't care.

#### The View Builder

Writing raw XML by hand quickly turns cumbersome. abap2UI5 ships
`z2ui5_cl_ui5_view_builder`, which produces the same XML by method chaining —
one call per control, so the shape of the chain is the shape of the view.
`stringify( )` renders the tree into the XML string the framework sends to the
frontend:

```abap
  METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`      v = `sap.m`
            )->a( n = `xmlns:core` v = `sap.ui.core`
            )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`

            )->ele( `Shell`
                )->ele( `Page`
                    )->a( n = `title` v = `My title`

                    )->tag( `Text`
                        )->a( n = `text` v = `My text` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
```

Both snippets produce the exact same view. Use whichever you prefer — raw
strings are fine for a handful of lines, the builder scales better for real
apps.

Four verbs, and no catalogue of controls behind them:

| | |
| --- | --- |
| `ele( )` | add a control and **descend** into it — for a container |
| `tag( )` | add a control and **stay** — for a leaf |
| `a( )` | set **one** attribute on the control the chain is pointing at |
| `end( )` | ascend to the parent |

There is exactly one rule: `a( )` applies to the control the chain is
**pointing at** — the child just added by `ele( )` or `tag( )` — so an
attribute always follows its own control, and a control has to receive its
attributes *before* its first child. `ele( )` descends and is closed by an
`end( )`; `tag( )` stays, so a run of leaves needs none. The final `end( )`s
may be left out entirely: `stringify( )` always renders from the root, no
matter where the chain stopped.

The root `mvc:View` and its `xmlns` declarations are written by hand, exactly
as in a real UI5 view — the builder does not invent them for you.

Because the builder knows no control names, **it can express every control,
property and aggregation UI5 has**, including those released after this page
was written. Whatever name and namespace you pass is written into the XML
verbatim, in the SDK's own camelCase. An aggregation is an element like any
other and carries its parent's namespace — `<m:content>` under a Page is
``ele( n = `content` ns = `m` )``, a default-namespace `<columns>` inside an
`sap.ui.table.Table` is ``ele( `columns` )``.

For an ABAP boolean pass `b` instead of `v`; it renders `true`/`false`, so a
flag reaches the view without a conversion of its own:

```abap
    )->a( n = `editable` b = mv_edit_mode
    )->a( n = `visible`  b = xsdbool( lines( mt_item ) > 0 ) )
```

Tips for working with views:
- The [VS Code extension](https://github.com/abap2UI5/vscode-extension) gives
  the chain completion and hover for the whole UI5 API, and checks the view
  while you type.
- The [abap2UI5-linter](https://github.com/abap2UI5/linter) rebuilds the view
  from your chain and reports unknown controls, properties, enum values and
  `@since` violations — no SAP system involved.
- See the [samples repository](/get_started/next#sample-apps) for ready-made
  examples to copy and adapt.

::: warning Respect the UI5 Control Aggregation Rules
The builder is intentionally permissive — it lets you nest **any** control
inside **any** other control, because it never knew what either of them is.
UI5 itself is not permissive. Every UI5 control defines specific aggregations
(e.g. `sap.m.Page` has `content`, `headerContent`, `footer`) and each
aggregation accepts only certain child control types (often a particular
interface or base class).

Combining controls in a way that violates these rules can lead to broken
rendering, missing controls, layout glitches, runtime errors in the browser
console, or subtle bugs that only show up on certain devices or themes.

**Always check the [UI5 SDK](https://sapui5.hana.ondemand.com) for each
control** to confirm:
- which aggregations it exposes,
- which child types those aggregations accept, and
- which parent controls are valid for the control you want to use.

The ABAP compiler cannot catch these mistakes — they are pure UI5 concerns.
The [abap2UI5-linter](https://github.com/abap2UI5/linter) catches a large part
of them before you deploy, and the rest have to be verified against the SDK.
:::

#### Where to Look for Controls

Because UI5 XML is used 1:1, **the UI5 documentation is your reference** for anything visual:

- [UI5 Demo Kit](https://sapui5.hana.ondemand.com/sdk) — interactive samples for every control
- [UI5 Control API](https://sapui5.hana.ondemand.com/sdk/#/api) — properties, aggregations, events

Find a control you like in the UI5 docs, copy its XML, paste it into `view_display( )` — done. abap2UI5 has no separate control catalog to learn.

One thing the SDK will not warn you about while you copy: the control may be deprecated. Because the XML is passed through 1:1, a deprecated control renders exactly like any other — until UI5 removes it. See [Deprecated Controls](/cookbook/view/deprecated_controls) for the cases that come up most often.

#### Choosing a Control

The UI5 SDK is large. The table below covers the choices that come up in almost every abap2UI5 app — use it as a starting point before diving into the SDK.

| Need                              | Use                                                   | Notes                                                                              |
| --------------------------------- | ----------------------------------------------------- | ---------------------------------------------------------------------------------- |
| Tabular data, columns, sorting    | `sap.m.Table`                                         | Responsive, supports growing/p13n. Default choice for business data.               |
| Flat list with icons/avatars      | `sap.m.List` with `StandardListItem`                  | Lighter than `Table` when columns are not needed.                                  |
| Hierarchical data (parent/child)  | `sap.m.Tree` or `sap.ui.table.TreeTable`              | `Tree` is responsive; `TreeTable` shows fixed columns.                             |
| Form with labels + inputs         | `sap.ui.layout.form.SimpleForm`                       | Use this 90% of the time — auto-layouts labels and fields responsively.            |
| Form with custom grid layout      | `sap.ui.layout.form.Form`                             | When `SimpleForm` is not flexible enough.                                          |
| App page with title and content   | `sap.m.Page`                                          | The standard container. Wrap in `sap.m.Shell` for the SAP frame.                   |
| Page with collapsible header      | `sap.f.DynamicPage`                                   | For object pages and analytics screens.                                            |
| Page with action toolbar          | `sap.f.semantic.SemanticPage`                         | Adds semantic actions (edit, delete, share) in the footer.                         |
| Vertical / horizontal stack       | `sap.m.VBox` / `sap.m.HBox`                           | Quick layout without a form.                                                       |
| Tabs                              | `sap.m.IconTabBar`                                    | Use `IconTabFilter` for each tab.                                                  |
| Single-select dropdown            | `sap.m.Select` (≤ 20 items) / `sap.m.ComboBox`        | `ComboBox` allows typing and filtering.                                            |
| Multi-select dropdown             | `sap.m.MultiComboBox`                                 | Pills appear inside the field.                                                     |
| Date / time input                 | `sap.m.DatePicker` / `sap.m.TimePicker` / `sap.m.DateTimePicker` | Needs a formatter — see [Binding → Data-Type Mapping](/cookbook/model/binding#data-type-mapping). |
| Status indicator                  | `sap.m.ObjectStatus`                                  | Colored text + icon for state.                                                     |
| Modal dialog                      | `sap.m.Dialog` (inside a `core:FragmentDefinition`)           | See [Popup](/cookbook/popup_popover/popup).                                        |

When two controls fit, prefer the simpler one: `Table` over `TreeTable`, `SimpleForm` over `Form`, `Select` over `ComboBox`. Switch to the richer variant only when a concrete requirement justifies it.

#### Next Steps
This produces a static view. The next section walks through binding and sharing data between the view and the app logic.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Basics I — Hello World, the Smallest App | [`Z2UI5_CL_SMP_APP_493`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_493.clas.abap) |
| Ship Your Own CSS with the View | [`Z2UI5_CL_SMP_APP_050`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_050.clas.abap) |
| FlexBox Layouts with Custom Classes | [`Z2UI5_CL_SMP_APP_255`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_255.clas.abap) |

<!-- samples:end -->
