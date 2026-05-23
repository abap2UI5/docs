---
outline: [2, 4]
---
# Overview

abap2UI5 uses [SAP UI5](https://sapui5.hana.ondemand.com) on the frontend without modification. Whatever your ABAP code sends to the browser is a **standard UI5 XML view** — the same XML you would write in any UI5 freestyle project.

The consequence: **everything in the UI5 SDK works in abap2UI5 1:1**. Any control, any property, any namespace from the [UI5 Demo Kit](https://sapui5.hana.ondemand.com/sdk) is available. Copy the XML, paste it into your ABAP class, and it renders.

#### Sending a View

The simplest case: build an XML string and ship it to the client.

```abap
METHOD z2ui5_if_app~main.

  client->view_display(
      `<mvc:View xmlns="sap.m" xmlns:mvc="sap.ui.core.mvc">` &&
      `  <Page title="Hello">`                              &&
      `    <Text text="World"/>`                            &&
      `  </Page>`                                           &&
      `</mvc:View>` ).

ENDMETHOD.
```

That's it — UI5 receives the XML and renders it. Swap `<Text>` for any other control from the SDK; the framework doesn't care.

#### Helper Class

Writing raw XML by hand quickly gets tedious. abap2UI5 ships the helper class `z2ui5_cl_xml_view` with a fluent API that mirrors the XML structure and gives you code completion:

```abap
METHOD z2ui5_if_app~main.

  client->view_display(
    z2ui5_cl_xml_view=>factory(
       )->page( `Hello`
           )->text( `World`
       )->stringify( ) ).

ENDMETHOD.
```

Both snippets produce the exact same view. Use whichever you prefer — raw strings are fine for a handful of lines, the fluent API scales better for real apps.

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
| Date / time input                 | `sap.m.DatePicker` / `sap.m.TimePicker` / `sap.m.DateTimePicker` | Needs a formatter — see [Binding → Data-Type Mapping](/cookbook/model/binding#known-limitations). |
| Status indicator                  | `sap.m.ObjectStatus`                                  | Colored text + icon for state.                                                     |
| Modal dialog                      | `sap.m.Dialog` (built with `factory_popup`)           | See [Popover](/cookbook/popup_popover/popover).                                    |

When two controls fit, prefer the simpler one: `Table` over `TreeTable`, `SimpleForm` over `Form`, `Select` over `ComboBox`. Switch to the richer variant only when a concrete requirement justifies it.
