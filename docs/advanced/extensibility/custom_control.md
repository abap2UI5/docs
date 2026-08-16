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

The wrapper class the previous builder needed for this
([`z2ui5_cl_xml_view_cc`](https://github.com/abap2UI5/abap2UI5/blob/main/src/99/z2ui5_cl_xml_view_cc.clas.abap))
is frozen along with the builder itself, and adding a method to it is no longer
part of shipping a custom control.

