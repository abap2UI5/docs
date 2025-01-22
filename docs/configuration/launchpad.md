---
outline: [2, 6]
---
# Fiori Launchpad

Integrate your abap2UI5 apps into SAP Fiori Launchpads. Find all information here: <br>
[**(1) Installation & Configuration**](https://www.linkedin.com/pulse/copy-abap2ui5-host-your-apps-sap-fiori-launchpad-abap2ui5-ocn2e/) <br>
[**(2) Features: Title, Parameters, Navigation**](https://www.linkedin.com/pulse/abap2ui5-host-your-apps-sap-fiori-launchpad-23-features-abap2ui5-upche/) <br>
[**(3) Integration of KPIs**](https://www.linkedin.com/pulse/abap2ui5-host-your-apps-sap-fiori-launchpad-33-kpis-abap2ui5-uuxxe/) <br>



### Target Mapping
Use the following parameters for target mapping in your Launchpad configuration:
* Semantic Object: Z2UI5_CL_MY_APP
* Action: display
* URL: /sap/bc/ui5_ui5/sap/z2ui5
* ID: z2ui5
* Parameter: app_start / Z2UI5_CL_MY_APP


### Troubleshooting
Sometimes, installation via abapGit can cause cache-related issues. Follow these steps to resolve them:

#### Cache Management

1. Recalculate app index of z2ui5 with report /UI5/APP_INDEX_CALCULATE
![389816897-f18e791e-1e07-4381-a8a8-deb5af3ec02c](https://github.com/user-attachments/assets/50c505ab-c58e-46a6-999e-67c4e4cdb929)
![389816886-093d087f-4d7d-48b3-b7c4-75c16046af5b](https://github.com/user-attachments/assets/81f8feae-fcfe-4175-aa91-28ce8d681539)

2. Recalculate index of distribution layer with report /UI5/APP_INDEX_CALCULATE (if tab isn't visible try switching to another tab, then it usually appears)
![389817086-2a480005-f9f9-46e8-a432-456494957665](https://github.com/user-attachments/assets/3fce0f2e-96f9-4487-9226-7940336582b1)
![389817130-389f2be1-d75b-4dbb-aa81-e5b5e4202440](https://github.com/user-attachments/assets/dc149874-6731-496d-90bf-79cb83d8c97d)

3. Invalidate http caches in transaction SMICM
![389817432-f6568b5e-0588-4a98-83cc-f1bd58e0dd64](https://github.com/user-attachments/assets/497b7677-8009-472e-9b50-34719105a12e)

4. Clear browser caches and hard reload

#### Manual Deployment
If cache clearing doesn’t resolve the issue, manually upload the frontend application:

1. Download the webapp folder of the project.

2. Use the SAP program `/UI5/UI5_REPOSITORY_LOAD` to upload the application to the server.
<img width="942" alt="image" src="https://github.com/user-attachments/assets/2eac29f4-596e-4bab-8a17-7a8f86630b95">


### Launchpad KPIs

Enhance your Fiori Launchpad with Key Performance Indicators (KPIs) using the abap2UI5 Launchpad KPI Add-On.

<i class="fa-brands fa-github"></i> [Repository](https://github.com/abap2UI5-addons/launchpad-kpi)

Find more information in the blog article on [LinkedIn.](https://www.linkedin.com/pulse/abap2ui5-host-your-apps-sap-fiori-launchpad-33-kpis-abap2ui5-uuxxe/)

#### Functionality
<img width="800" alt="image" src="https://github.com/abap2UI5/abap2UI5-connector_launchpad_kpi/assets/102328295/c7db9e46-6876-40d8-a632-be79e2fbcb91">
<br>

#### Approach
(1/3) Use a single Interface:
```abap
INTERFACE z2ui5_if_lp_kpi
  PUBLIC.

  METHODS count
    IMPORTING
      filter        TYPE string
    RETURNING
      VALUE(result) TYPE i.

ENDINTERFACE.
```
(2/3) Which can be used on app level to return KPIs:
```abap
CLASS z2ui5_cl_lp_kpi_hello_world DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES z2ui5_if_proxy_kpi.
    INTERFACES z2ui5_if_app.

ENDCLASS.

CLASS z2ui5_cl_proxy_kpi_hello_world IMPLEMENTATION.

  METHOD z2ui5_if_lp_kpi~count.
    "kpi calculation....
    result = 10.
  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    "abap2UI5 app logic here...
  ENDMETHOD.

ENDCLASS.
```
(3/3) A generic OData service takes care of everything else (which just returns n dummy entries). Just maintain the KPI at the Launchpad with the following endpoint:
```
.../sap/opu/odata/sap/Z2UI5_PROXY_KPI_SRV/ENTITYCollection/$count?$filter=CLASS eq 'z2ui5_cl_proxy_kpi_hello_world'
```
