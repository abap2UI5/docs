---
outline: [2, 4]
---
# Custom Controls

You can build your own UI5 custom controls and use them in abap2UI5 apps.

## Frontend

A custom control is a UI5 control like any other, served from a BSP of its own
next to the abap2UI5 frontend. The frontend reserves two resource roots for
exactly this, so a control needs no change to the framework: not to its
`index.html` and not to its `manifest.json`, both of which the next update
would overwrite.

| Root | BSP | For |
|---|---|---|
| `z2ui5_ccc` | `Z2UI5_CCC` | your own controls, icon fonts and CSS — start from the [custom-controls-customer](https://github.com/abap2UI5-addons/custom-controls-customer) template |
| `z2ui5_cci` | `Z2UI5_CCI` | the community controls of the [custom-controls](https://github.com/abap2UI5-addons/custom-controls) add-on, where a control that is useful to others belongs |

In the template, `app/webapp/cc/Example.js` defines the control
`z2ui5_ccc.cc.Example` — copy it and adapt it. `npm run app2bsp` turns
`app/webapp` into the BSP, and abapGit installs it. The browser loads the
control from `/sap/bc/ui5_ui5/sap/z2ui5_ccc/cc/Example.js` the first time a
view uses it. That is your own system, so the default Content-Security-Policy
already allows it.

A control that wraps a third-party library, such as a chart or a barcode
generator, follows the custom-controls add-on: the library is named once in
its `tools/libs.json` and loaded on first use. On the `main` branch it comes
from jsDelivr, which the policy has to allow (see
[Customizing the CSP](/configuration/security#customizing-the-csp)); the
`local` branch serves it from the BSP itself, for systems whose browsers have
no internet access.

## Backend

Nothing. The current view builder has no method per control, so a custom
control needs no backend counterpart — declare its namespace on the view and
write the element and its properties directly:

```abap
view->ele( n = `View` ns = `mvc`
    )->a( n = `xmlns`           v = `sap.m`
    )->a( n = `xmlns:mvc`       v = `sap.ui.core.mvc`
    )->a( n = `xmlns:z2ui5_ccc` v = `z2ui5_ccc.cc`

    )->ele( `Page`
        )->tag( n = `Example` ns = `z2ui5_ccc`
            )->a( n = `text`  v = client->_bind( mv_value )
            )->a( n = `press` v = client->_event( `MY_EVENT` ) ).
```

Nothing else is needed on the ABAP side. The builder writes whatever element
and namespace you pass, verbatim, so a custom control is just another tag —
there is no wrapper class to extend and no method to add before it can be
used.

The two roots differ in their last letter only. When a control does not
render, `sap.ui.require.toUrl("z2ui5_ccc/cc/Example.js")` in the browser
console has to return the BSP path — that separates a typo in the view's
`xmlns:` from a BSP that is not installed.

To change the abap2UI5 frontend itself, for a pull request to the framework,
see [Frontend](/advanced/extensibility/frontend).
