### Guideline - Destination Configuration

<img width="1175" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/fbdbadd0-b333-4216-b520-48dcc7250277"> <br>
###### **Note I:** Verify that the HTTP Service in the backend system is under the ICF Node sap/bc/z2ui5 and that the Destination is named "BACKEND_ABAP2UI5" for correct routing [(here)](https://github.com/abap2UI5/abap2UI5-btp_proxy_app/blob/e882d732eb509ce65f38e224112da8d8120b0f22/xs-app.json#L8)
###### **Note II:** Input the complete path of the HTTP endpoint here.


### Destination Configuration
Name                        BACKEND_ABAP2UI5 <br>
Type                        HTTP <br>
Description                 abap2UI5 Destination <br>
Proxy Type                  Internet <br>
Authentication              BasicAuthentication <br>
User                        User <br>
Password                    (password for User) <br>

### Additional Properties
HTML5.DynamicDestination    true <br>
product.name                ABAP System <br>
sap-client                  (client) <br>
WebIDEEnabled               true <br>
WebIDEUsage                 odata_abap,dev_abap <br>
