---
outline: [2, 4]
---
# Cloud Readiness

_Ready for the Future — or Not?_

Whether it makes sense to build apps with abap2UI5 hinges on future-proofing. In the ABAP ecosystem, this means cloud readiness, closely tied to the ABAP Cloud language version. But what does that mean? And are abap2UI5 apps truly cloud-ready?

#### What is ABAP Cloud?

ABAP Cloud is a restricted ABAP language version that ensures cloud readiness and upgrade stability. Code that follows the ABAP Cloud rules can run on SAP BTP ABAP Environment and SAP S/4HANA Public Cloud ABAP Environment. SAP guarantees long-term compatibility with future platform updates — without code changes or further investment.

ABAP Cloud includes a simplified technology stack, syntax, and toolset:

- No classic UI technologies like Web Dynpro, ALV, GUI CFW
- Simplified syntax (e.g., no `WRITE` statements)
- Only released APIs allowed
- No direct access to SAP database tables — use the VDM instead
- Development with ADT or browser-based tools — no SE80

SAP recommends building all new applications in ABAP Cloud.

#### Is abap2UI5 Cloud Ready?

Yes — abap2UI5 is fully compatible with ABAP Cloud:

- Implemented entirely in the ABAP for Cloud language version
- Uses only released SAP APIs or its own internal classes
- Requires no changes to standard SAP code
- The frontend is a normal UI5 freestyle app

You can install abap2UI5 on SAP BTP ABAP Environment and SAP S/4HANA Public Cloud.

#### Are abap2UI5 Apps Cloud Ready?

Yes and no — it depends on how you build the app.

While the framework is cloud-ready, you must also build each app following cloud-ready principles.

1. Example: Display Sales Orders (Cloud-Ready):

```abap
CLASS z2ui5_cl_demo_app_003 DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mt_salesorder TYPE STANDARD TABLE OF I_SalesOrder WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_demo_app_003 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      SELECT FROM I_SalesOrder
       FIELDS salesorder, salesorganization
       INTO TABLE @mt_salesorder
       UP TO 10 ROWS.

      DATA(view) = z2ui5_cl_xml_view=>factory(
          )->list( client->_bind_edit( mt_salesorder )
            )->standard_list_item(
              title       = `{SALESORDER}`
              description = `{SALESORGANIZATION}` ).
      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```
This example uses only released APIs, such as the CDS View `I_SalesOrder`.

2. Example: Display Sales Orders (not Cloud-Ready):
```abap
CLASS z2ui5_cl_demo_app_003 DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mt_salesorder TYPE STANDARD TABLE OF vbak WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_demo_app_003 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      SELECT FROM vbak
       FIELDS vbeln, vkorg
       INTO TABLE @mt_salesorder
       UP TO 10 ROWS.

      DATA(view) = z2ui5_cl_xml_view=>factory(
        )->list( client->_bind_edit( mt_salesorder )
          )->standard_list_item(
              title       = `{VBELN}`
              description = `{VKORG}` ).
      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```
The app reads directly from the VBAK database table, which makes it cloud-incompatible.

Always follow cloud-ready development principles so your apps remain upgrade-stable and compatible with ABAP Cloud environments.

#### Do I Have to Use RAP to Be Cloud Ready?

No — RAP isn't mandatory.

RAP (RESTful Application Programming Model) is SAP's recommended approach for building cloud-native applications. It rests on:
- CDS Views for data modeling
- Behavior Definitions for logic and validations
- OData services for communication
- Fiori Elements for UI generation

abap2UI5 offers an alternative, especially for developers who want lightweight apps and don't need the full RAP stack. Both approaches are valid as long as you follow cloud development principles.

#### Conclusion

abap2UI5 is fully cloud-ready. It supports modern, backend-driven UI5 development while remaining upgrade-stable and compatible with SAP BTP ABAP Environment and S/4HANA Public Cloud. Each app can be built cloud-ready, making abap2UI5 a strong complement to existing RAP or UI5 freestyle apps.

Happy ABAPing!
