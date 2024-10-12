# UI5 Compatibility

## OpenUI5

## UI5

## OpenUI5 2.x
we have to take care of the deprecated APIs:
https://ui5.sap.com/#/topic/798dd9abcae24c8194922615191ab3f5
https://ui5.sap.com/#/topic/2acafbfcc2cb47e8aac6d2e32399af10
etc.

Checked abap2UI5 with the ui5-linter and made some fixes:
https://github.com/SAP/ui5-linter

@choper725 fyi:
changed the sdk check here:
abap2UI5/src/01/02/z2ui5_cl_core_http_get.clas.abap

Line 432 in a78952d

 `          var ui5_sdk = oCurrentVersionInfo.gav.includes('com.sap.ui5') ? true : false;` && |\n| && 
and changed the openById function:
abap2UI5/src/01/02/z2ui5_cl_core_http_get.clas.abap

Line 169 in a78952d

 `                  oControl = Element.getElementById(openById);` && |\n| && 
Maybe check on your end if it still works? I don't have a sample for this use case and can not test it by myself. Is there a chance for a sample or any infos of how i can reproduce this case?
