## Installlation

### 1/4 Install the project with abapGit
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/542604a9-a0ab-44bb-8b9e-de0ac7f1f4e4"><br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/65c504cb-b316-440d-a461-5398b00b1fcd"><br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/245c2905-6d21-419c-8384-6cb08a5d5a0f"><br>
If errors pop up, activate anyway:<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/ce33cdc1-57a8-4e64-af8d-d5cc8135c137"><br>
Sometimes there are small diffs with the ICF handler (this is no problem):<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/743f836e-7926-46ed-9b01-38c5efff5fbc"><br>
<br>
### 2/4 Check the OData Service
Go to transaction SICF:<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/62cd37d7-131c-4dcc-967f-6571a6dba5f7"><br>
Search for "2UI5" and check if there are three ICF Nodes and they are all activated:<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/f280fedc-47fe-4133-b826-e28ad97796b1"><br>
Next test the odata service (/sap/opu/odata/sap/z2ui5_odata_srv):<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/5546d3fc-886c-4f28-b814-20da96386bf6"><br>
<br>
### 3/4 Create a new System Alias
Sometimes this error occurs:<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/30ff5c7e-96da-4e42-9921-f599cdda90da"><br>
If this is the case we have to create a new system alias, go to transaction /iwfnd/maint_service and search for  z2ui5_odata_srv:<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/6b093f09-e733-4e4e-a00d-4f76cdb0ffb9"><br>
Press add system alias and create a new one:<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/68cec889-bb81-4d6d-9f06-83d66e465990"><br>
Now it should look similar to this:<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/30d5e299-2670-4c05-896c-df948919688c"><br>
Test the service again and now it looks like this:<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/4e55d569-c662-4514-b054-3ba9f4fade33"><br>
<br>
### 4/4 Check the UI5 App
Go again to transaction SICF and search for "2UI5":<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/f280fedc-47fe-4133-b826-e28ad97796b1"><br>
Now start the UI5 app (/sap/bc/ui5_ui5/sap/zabap2ui5/index.html):<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/d66ab600-13b3-40bf-8b85-83f72b02d3fd"><br>
And you can see the starting page:<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/d5789969-a1eb-4f41-b423-741375f9abbb"><br>
Use this app for the fiori launchpad maintenance.
