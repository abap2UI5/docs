# Integration to Fiori Element Apps

You can extend the object page of Fiori list report apps with an abap2UI5 app. A repository to showcase the use case can be found [here.](https://github.com/axelmohnen/ABAP2UI5_COMP_CONT)

<img width="747" height="387" alt="image" src="https://github.com/user-attachments/assets/c14d5732-3b8c-4fa5-83ab-6d188a4d87db" />


### Guide

Follow the following steps:

1. Register the FLP integration in the component.js:

```javascript
// Register ABAP2UI5 FLP integration for component container 
jQuery.sap.registerModulePath("z2ui5", "/sap/bc/ui5_ui5/sap/z2ui5");
```

2. Adapt the object page extension controller.js:

```javascript
sap.ui.core.Component.create({
				name: "z2ui5",
				settings: {
					componentData: {
						startupParameters: {
							app_start: ["ZCL_YOUR_2UI5_CLASS_APP"],
							key1: ["Param1"],
							key2: ["Param2"]
						}
					}
				}
			}).then(function (oComponent) {
				var oCompContainter = new sap.ui.core.ComponentContainer({
					component: oComponent,
					async: true
				});
				
				// Add component container to your VBox
				var oVBox = that.getView().byId("VBoxId")
				oVBox.destroyItems();
				oVBox.addItem(oCompContainter);
				
				//Overwrite default height of object page section
				var oSection = that.getView().byId("[This is the ID of your object page section::Section]");
				oSection.addStyleClass("customSectionHeight");
				
			});
```

3. Implement facet fragment:
```xml
<core:FragmentDefinition xmlns:commons="sap.ui.commons" xmlns:core="sap.ui.core" xmlns:form="sap.ui.layout.form" xmlns:l="sap.ui.layout"
	xmlns:sfi="sap.ui.comp.smartfield" xmlns:sfo="sap.ui.comp.smartform" xmlns:table="sap.ui.table"
	xmlns:template="http://schemas.sap.com/sapui5/extension/sap.ui.core.template/1" xmlns:uxap="sap.uxap" xmlns="sap.m">
		<VBox id="VBoxId" direction="Column" height="200px" ></VBox>
</core:FragmentDefinition>
```

4. Add CSS (style.css):
```json
.customSectionHeight {
    height: auto !important;
}
```

5. Create ABAP2U5 app class
```abap  
METHOD z2ui5_if_app~main.

    IF check_initialized = abap_false.
      check_initialized = abap_true.

* ---------- Get startup parameters ---------------------------------------------------------------
      DATA(lt_startup_params) = client->get( )-t_comp_params.

    ENDIF.

    DATA(lr_view) = z2ui5_cl_xml_view=>factory( ).
    DATA(lr_page) = lr_view->page( showheader = abap_false
                                   backgrounddesign = `List` )->content( ). "Backgrounddesign "List" sets a white background color

    lr_page->text( `TEXT` ).

    client->view_display( lr_view->stringify( ) ).

  ENDMETHOD.
```
