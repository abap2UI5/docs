### Guideline - Destination Configuration

<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-btp_proxy_app/assets/102328295/1a4880fd-aad7-4f40-9a45-9452205f9bff"> <br>
###### **Note I:** Verify that the HTTP Service in the backend system is under the ICF Node sap/... and that the Destination is named "BACKEND_ABAP2UI5" for correct routing [(here)](https://github.com/abap2UI5/abap2UI5-btp_proxy_app/blob/e882d732eb509ce65f38e224112da8d8120b0f22/xs-app.json#L8)
###### **Note II:** Input the complete path of the HTTP endpoint here. The frontend does not include any path information (e.g. https://<<backend_system>>/sap/bc/ZTEST)
