---
outline: [2, 4]
---
# Custom Controls

::: warning This page still shows the previous view builder
The examples below build views with `z2ui5_cl_xml_view`. That class is frozen:
it still runs, and your existing apps keep working — but it is no longer the
one to write new code against. The current builder is
`z2ui5_cl_ui5_view_builder`, and it has four verbs instead of a control per
method, which makes every UI5 control available rather than the curated set.

See [View → Definition](/cookbook/view/definition) for what the chain looks
like, and [Deprecations](/resources/deprecations) for the translation.
:::

You can build your own UI5 custom controls and use them in abap2UI5 apps.

First, set up your VS Code environment with the abap2UI5 frontend artifacts, following the [Frontend](/advanced/extensibility/frontend) page.

#### Frontend

Write the JavaScript for your new custom control. Each custom control lives in its own file under [app/webapp/cc/](https://github.com/abap2UI5/abap2UI5/tree/main/app/webapp/cc) — copy an existing one (e.g. `Timer.js`) and adapt it to your needs.

#### Backend
Extend the custom control view class by adding a method and defining the new control's properties:
[z2ui5_cl_xml_view_cc.clas.abap](https://github.com/abap2UI5/abap2UI5/blob/main/src/99/z2ui5_cl_xml_view_cc.clas.abap)
