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
