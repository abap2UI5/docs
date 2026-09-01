---
outline: [2, 4]
---
# Custom Controls

You can build your own UI5 custom controls and use them in abap2UI5 apps.

First, set up your VS Code environment with the abap2UI5 frontend artifacts, following the [Frontend](/advanced/extensibility/frontend) page.

## Frontend

Write the JavaScript for your new custom control. Each custom control lives in its own file under [app/webapp/cc/](https://github.com/abap2UI5/abap2UI5/tree/main/app/webapp/cc) — copy an existing one (e.g. `Timer.js`) and adapt it to your needs.

## Backend

Nothing. The current view builder has no method per control, so a custom
control needs no backend counterpart — write the element and its properties
directly, in the `z2ui5.cc` namespace the frontend registers:

```abap
view->ele( n = `View` ns = `mvc`
    )->a( n = `xmlns`       v = `sap.m`
    )->a( n = `xmlns:mvc`   v = `sap.ui.core.mvc`
    )->a( n = `xmlns:z2ui5` v = `z2ui5.cc`

    )->ele( `Page`
        )->tag( n = `MyControl` ns = `z2ui5`
            )->a( n = `value`   v = client->_bind( mv_value )
            )->a( n = `onEvent` v = client->_event( `MY_EVENT` ) ).
```

Nothing else is needed on the ABAP side. The builder writes whatever element
and namespace you pass, verbatim, so a custom control is just another tag —
there is no wrapper class to extend and no method to add before it can be
used.

