---
outline: [2, 4]
---
# BTP Build Work Zone

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
* URL: the URL above
* Proxy Type: Internet
* Authentication: BasicAuthentication
* User: (user)
* Password: (password)

##### Additional Properties

These properties are required by SAP Build Work Zone to correctly route requests to your ABAP backend:

| Property | Value | Description |
|---|---|---|
| HTML5.DynamicDestination | `true` | Allows HTML5 apps to resolve this destination at runtime |
| product.name | `ABAP System` | Identifies the backend type for the Work Zone tile configuration |
| sap-client | *(your client number)* | The SAP system client to connect to (e.g. `001`) |
| WebIDEEnabled | `true` | Enables the destination for SAP Business Application Studio |
| WebIDEUsage | `odata_abap,dev_abap` | Declares supported protocols for development tools |