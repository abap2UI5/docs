# Configuration Launchpad On-Premise

The Launchpad tools have evolved over time. Some are deprecated and should not be used for maintenance anymore.
For more information, please visit: <br>
https://community.sap.com/t5/technology-blogs-by-sap/sap-fiori-for-sap-s-4hana-overview-of-tools-for-maintaining-custom/ba-p/13463870

Starting from S/4 1909 and above, you can utilize the following configuration guideline.

### Create a Semantic object
Transaction /UI2/SEMOBJ <br>
Create a semantic object for each abap class:
![image](https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/fe0a45a3-aa1d-4a18-8497-0a5b71bf6369)

### Create a Target Mapping
Transaction /UI2/FLPD_CUST <br>
Select the Catalog of your choice and create a new Target Mapping:
![image](https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/f3ea204e-101d-41e0-89ff-a783a3cc6108)<br><br>
And use the following parameters:
![image](https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/e8dd852a-6416-420c-b4dc-99a729bfb136)<br>
Semantic Object: Z2UI5_CL_DEMO_APP_127<br>
Action: display <br>
URL: /sap/bc/ui5_ui5/sap/zabap2ui5 <br>
ID: z2ui5 <br>
Parameters: app_start / Z2UI5_CL_DEMO_APP_127 <br>

### Create a Tile
Use the follwing parameters:
![image](https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/2d476659-6a81-4183-9cd1-f923e61f55db) <br>
Semantic Object: Z2UI5_CL_DEMO_APP_127<br>
Action: display<br>
<br>
___Repeat these first 3 steps for every app you want to add to the launchpad.___

### Edit the Launchpad Side
Transaction /UI2/FLP <br>
Go to your Launchpad and edit the current page:
![image](https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/30f7ebeb-80ef-4e6e-b8ae-7b0f7e5e0101)<br>
Press add a tile:
![image](https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/57711ded-4d7b-4c22-bef1-073195efe889)<br>
Find again your catalog and add the tiles by pressing +:
![image](https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/1d77dfd7-1e0e-41c1-a68a-24a32f6bd2bf)<br>
Next go back to your launchpad and you see the newly added tiles:
![image](https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/e7559cc5-d09f-4439-8793-d04438b5919a)<br>

___If you run into problems, don't miss to delete your browser cache and try again!___
