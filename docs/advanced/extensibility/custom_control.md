---
outline: [2, 4]
---
# Custom Controls

You can build your own UI5 custom controls and use them in abap2UI5 apps.

First, set up your VS Code environment with the abap2UI5 frontend artifacts, as described on the [Frontend](/advanced/extensibility/frontend) page.

#### Frontend

Create the JS code for your new custom control. Copy an existing one from [App.controller.js](https://github.com/abap2UI5/abap2UI5/blob/main/app/webapp/controller/App.controller.js) and adapt it.

#### Backend
Extend the custom control view class by adding a method and defining the new control's properties:
[z2ui5_cl_xml_view_cc.clas.abap](https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_cl_xml_view_cc.clas.abap)
