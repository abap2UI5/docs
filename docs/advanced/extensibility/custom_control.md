---
outline: [2, 4]
---
# Custom Controls

You can develop your own UI5 custom controls and use them in your abap2UI5 apps.

Set up your VS Code environment with the abap2UI5 frontend artifacts as described on the [Frontend](/advanced/extensibility/frontend) page.

#### Frontend

Create the JS code for your new custom control. Copy and paste an existing custom control from [App.controller.js](https://github.com/abap2UI5/abap2UI5/blob/main/app/webapp/controller/App.controller.js) and adjust it to your needs.

#### Backend
Extend the custom control view class by adding a method and defining the properties of your custom control:
[z2ui5_cl_xml_view_cc.clas.abap](https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_cl_xml_view_cc.clas.abap)
