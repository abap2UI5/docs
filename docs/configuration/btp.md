---
outline: [2, 4]
---
# BTP Build Workzone

Integrate your abap2UI5 apps into BTP Services like Build Workzone. Find all information here:<br>
[**Installation & Configuration of BTP**](https://www.linkedin.com/pulse/abap2ui5-integration-sap-business-technology-platform-13-installation-lf1re/?trackingId=YQ0y%2Fq0y6Kw5PK8chNCbrw%3D%3D&lipi=urn%3Ali%3Apage%3Ad_flagship3_pulse_read%3BFGBWUSIZRzeRjUNKBFD0uA%3D%3D)<br>
[**Setup SAP Build Work Zone**](https://www.linkedin.com/pulse/abap2ui5-integration-sap-business-technology-platform-23-setup-ujdqe/?trackingId=vFwHvpI9oBk2igiz5P5CWA%3D%3D&lipi=urn%3Ali%3Apage%3Ad_flagship3_pulse_read%3BFGBWUSIZRzeRjUNKBFD0uA%3D%3D)<br>
[**Setup SAP Mobile Start**](https://www.linkedin.com/pulse/abap2ui5-integration-sap-business-technology-platform-33-setup-uzure/?trackingId=He2W8FnZZ5UxpbGKHOeLEg%3D%3D&lipi=urn%3Ali%3Apage%3Ad_flagship3_pulse_read%3BFGBWUSIZRzeRjUNKBFD0uA%3D%3D)<br>

### Configuration
<br>

##### Destination

* Name: BACKEND_ABAP2UI5 
* Type: HTTP 
* Description: abap2UI5 Destination 
* URL: the url above
* Proxy Type: Internet 
* Authentication: BasicAuthentication 
* User: (user)
* Password: (password)

##### Additional Properties

* HTML5.DynamicDestination true 
* product.name ABAP System 
* sap-client (client) 
* WebIDEEnabled true 
* WebIDEUsage odata_abap,dev_abap