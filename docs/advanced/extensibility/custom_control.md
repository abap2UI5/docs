# Custom Controls  

You can develop your own UI5 custom controls and use them in your abap2UI5 apps.

Set up your VS Code environment with the abap2UI5 frontend artifacts as described in the frontend chapter.  

#### Frontend

Create the JS code for your new custom control. You can copy and paste an existing custom control of this file and adjust it to your needs:  
https://github.com/abap2UI5/abap2UI5/blob/main/app/webapp/controller/App.controller.js

#### Backend  
Extend the custom control view class by adding an additional method and defining the properties of your custom control here:  
z2ui5_cl_xml_view_cc.clas.abap](https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_cl_xml_view_cc.clas.abap
