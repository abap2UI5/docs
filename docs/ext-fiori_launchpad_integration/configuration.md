# Configuration Launchpad On-Premise

The Launchpad tools have evolved over time. Some are deprecated and should not be used for maintenance anymore.
For more information, please visit:
https://community.sap.com/t5/technology-blogs-by-sap/sap-fiori-for-sap-s-4hana-overview-of-tools-for-maintaining-custom/ba-p/13463870

Starting from S/4 1909 and above, you can utilize the following configuration guideline.

### Create a Semantic object
Transaction /UI2/SEMOBJ <br>
<img width="800" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/731ce1bc-81d3-43b4-8bef-22e5542f63f9">

### Create a Catalog, Tile, Target Mapping & Group
Transaction /UI2/FLPD_CUST
#### Catalog
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/c23de836-ee9c-4883-9bd1-5a975d21e126">
<img width="300" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/95fd45bf-4ca6-4c71-80e7-b2430fe85d67">

#### Tile
Create a new static Tile: <br>
<img width="800" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/ac9978e5-01ff-425b-a093-ff8ef395da16">


#### Target Mapping
 <img width="800" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/12995750-bec2-4da5-b55d-c9c1ce1ca2a8">
 <img width="800" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/7f5aa5a5-9eaa-4651-87ee-8166ceae642e">

#### Group
<img width="800" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/66452679-eb2f-4e07-8236-6100b603db43">
<img width="800" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/a5519895-91e6-4dfc-926a-33c0d6fda1a2">

### Maintain Roles
Transaction PFCG <br>
<img width="763" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/60d6e3e0-e4dd-46fc-9551-d9915b9d4f86">
