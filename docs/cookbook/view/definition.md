---
outline: [2, 4]
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

#### Helper Class

Writing raw XML by hand quickly turns cumbersome. abap2UI5 ships the helper class `z2ui5_cl_xml_view` with a fluent API that mirrors the XML structure and gives you code completion. The `stringify( )` method at the end converts the view tree into an XML string that the framework sends to the frontend:

```abap
  METHOD z2ui5_if_app~main.

    client->view_display(
        z2ui5_cl_xml_view=>factory(
            )->shell(
                )->page( `My title`
                    )->text( `My text`
        )->stringify( ) ).

  ENDMETHOD.
```

Both snippets produce the exact same view. Use whichever you prefer — raw strings are fine for a handful of lines, the fluent API scales better for real apps.

Tips for working with views:
- Use code completion on `Z2UI5_CL_XML_VIEW` to find controls and properties
- See the [samples repository](/get_started/next#sample-apps) for ready-made XML examples to copy and adapt

::: warning Respect the UI5 Control Aggregation Rules
`Z2UI5_CL_XML_VIEW` is intentionally permissive — its fluent API lets you nest **any** control inside **any** other control. UI5 itself is not. Every UI5 control defines specific aggregations (e.g. `sap.m.Page` has `content`, `headerContent`, `footer`) and each aggregation accepts only certain child control types (often a particular interface or base class).

Combining controls in a way that violates these rules can lead to broken rendering, missing controls, layout glitches, runtime errors in the browser console, or subtle bugs that only show up on certain devices or themes.

**Always check the [UI5 SDK](https://sapui5.hana.ondemand.com) for each control** to confirm:
- which aggregations it exposes,
- which child types those aggregations accept, and
- which parent controls are valid for the control you want to use.

The ABAP compiler cannot catch these mistakes — they are pure UI5 concerns and must be verified against the SDK documentation.
:::

#### Where to Look for Controls

Because UI5 XML is used 1:1, **the UI5 documentation is your reference** for anything visual:

- [UI5 Demo Kit](https://sapui5.hana.ondemand.com/sdk) — interactive samples for every control
- [UI5 Control API](https://sapui5.hana.ondemand.com/sdk/#/api) — properties, aggregations, events

Find a control you like in the UI5 docs, copy its XML, paste it into `view_display( )` — done. abap2UI5 has no separate control catalog to learn.

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
| Modal dialog                      | `sap.m.Dialog` (built with `factory_popup`)           | See [Popup](/cookbook/popup_popover/popup).                                        |

When two controls fit, prefer the simpler one: `Table` over `TreeTable`, `SimpleForm` over `Form`, `Select` over `ComboBox`. Switch to the richer variant only when a concrete requirement justifies it.

## Mapping UI5 XML ↔ ABAP Fluent API

`Z2UI5_CL_XML_VIEW` is a hand-curated wrapper around UI5 controls. It does not cover every UI5 control, and the naming is not always a strict 1:1 mapping of the UI5 SDK:

- Control names follow snake_case of the UI5 control class — `sap.m.MultiComboBox` becomes `->multi_combo_box( )`, `sap.m.Text` becomes `->text( )`.
- Properties are passed as named parameters, lowercased without underscores (`enabled`, `placeholder`, `selectedkey`, `growingthreshold`). Only the *method* names use snake_case (e.g. `multi_combo_box`).
- Aggregations are exposed as methods named after the aggregation itself (`->items( )`, `->content( )`, `->custom_data( )`) — not as `add_item( )` / `add_content( )`. Calling the aggregation method returns the parent builder so you can chain children inside it.
- Coverage is incomplete and occasionally inconsistent: some controls or properties are missing, some method signatures don't match the SDK exactly. When in doubt, check the source of `Z2UI5_CL_XML_VIEW` or fall back to the generic builder below.

### The Fully Generic Builder

Every fluent helper ultimately produces an XML element. The lowest-level method `_generic( name = ... ns = ... )` lets you emit **any** XML element with **any** attributes, regardless of whether `Z2UI5_CL_XML_VIEW` knows the control:

```abap
  DATA(view) = z2ui5_cl_xml_view=>factory( ).

  view->_generic( name = `Page` ns = `sap.m`
    )->_generic( name = `MultiComboBox` ns = `sap.m`
        t_prop = VALUE #(
          ( n = `selectedKeys` v = `{/keys}` )
          ( n = `placeholder`  v = `Pick one` )
        )
      )->_generic( name = `items` ns = `sap.m`
        )->_generic( name = `core:Item` ns = `sap.ui.core`
            t_prop = VALUE #(
              ( n = `key`  v = `{key}` )
              ( n = `text` v = `{text}` )
            ) ).

  client->view_display( view->stringify( ) ).
```

This maps **1:1** to the UI5 XML/SDK API — control names, property names, and aggregation names are written exactly as they appear in the [UI5 SDK](https://sapui5.hana.ondemand.com), in their original camelCase. There is no abstraction layer guessing what to call things.

The rest of this documentation uses the higher-level fluent API (`->page( )`, `->button( )`, `->multi_combo_box( )`) because it reads better in examples. Both styles can be mixed freely in the same view.

#### Next Steps
This produces a static view. The next section walks through binding and sharing data between the view and the app logic.
