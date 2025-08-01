# Cloud Readiness

_Ready for the Future – or Not?_

Whether it makes sense to build apps with abap2UI5 depends heavily on future-proofing. In the ABAP ecosystem, this means cloud readiness, which is closely tied to using the ABAP Cloud language version. But what exactly does that mean? And are abap2UI5 apps truly cloud ready? Let’s take a closer look.

#### What is ABAP Cloud?

ABAP Cloud is a restricted ABAP language version that ensures cloud readiness and upgrade stability. Developments that follow the ABAP Cloud rules can run on SAP BTP ABAP Environment and SAP S/4HANA Public Cloud ABAP Environment. SAP guarantees long-term compatibility with future platform updates — without requiring code adjustments or any further investments.

ABAP Cloud contains of a simplified technology stack, syntax, and toolset:

- No classic UI technologies like Web Dynpro, ALV, GUI CFW
- Simplified syntax (e.g., no `WRITE` statements)
- Use of only released APIs
- No direct access to SAP database tables, use the VDM instead
- Development via ADT or browser-based tools, but no SE80

SAP recommends developing all new applications in ABAP Cloud.

#### Is abap2UI5 Cloud Ready?

Yes — abap2UI5 is fully compatible with ABAP Cloud:

- Implemented entirely using the ABAP for Cloud language version
- Uses only released SAP APIs or internally defined classes
- Requires no modifications to standard SAP code
- Frontend is a normal UI5 freestyle app

abap2UI5 can be installed on SAP BTP ABAP Environment and SAP S/4HANA Public Cloud.

#### Are abap2UI5 Apps Cloud Ready?

Yes and no — it depends on how you develop your app.

While the framework is cloud-ready, each individual app must also be developed following cloud-ready principles too.

1. Example: Display Sales Orders (Cloud-Ready):

```abap
CLASS z2ui5_cl_demo_app_003 DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mt_salesorder TYPE STANDARD TABLE OF I_SalesOrder WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_demo_app_003 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    SELECT FROM I_SalesOrder
     FIELDS salesorder, salesorganization
     INTO TABLE @mt_salesorder
     UP TO 10 ROWS.

    DATA(view) = z2ui5_cl_xml_view=>factory(
        )->list( client->_bind_edit( mt_salesorder )
          )->standard_list_item(
            title       = '{SALESORDER}'
            description = '{SALESORGANIZATION}' ).
    client->view_display( view ).

  ENDMETHOD.
ENDCLASS.
```
Only released APIs are used, e.g., the CDS View `I_SalesOrder`.

2. Example: Display Sales Orders (not Cloud-Ready):
```abap
CLASS z2ui5_cl_demo_app_003 DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mt_salesorder TYPE STANDARD TABLE OF vbak WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_demo_app_003 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    SELECT FROM vbak
     FIELDS vbeln, vkorg
     INTO TABLE @mt_salesorder
     UP TO 10 ROWS.

    DATA(view) = z2ui5_cl_xml_view=>factory(
      )->list( client->_bind_edit( mt_salesorder )
        )->standard_list_item(
            title       = '{VBELN}'
            description = '{VKORG}' ).
    client->view_display( view ).

  ENDMETHOD.
ENDCLASS.
```
A direct database table read is performed on VBAK, making this app not cloud-ready.

Always adhere to cloud-ready development principles to ensure that your apps remain upgrade-stable and compatible with ABAP Cloud environments.

#### Do I Have to Use RAP to Be Cloud Ready?

No - RAP is not mandantory.

RAP (RESTful Application Programming Model) is SAP’s recommended model for building cloud-native applications. It is based on:
- CDS Views for data modeling
- Behavior Definitions for logic and validations
- OData Services for communication
- Fiori Elements for UI generation

abap2UI5 provides an alternative, especially for developers who prefer lightweight apps and don't require the full RAP stack. As long as cloud development principles are followed, both approaches are valid.

#### Conclusion

abap2UI5 is fully cloud-ready. It enables modern, backend-driven UI5 development while remaining upgrade-stable and compatible with SAP BTP ABAP Environment and S/4HANA Public Cloud. Each app can be developed in a cloud-ready manner, making it a perfect addition to existing RAP or UI5 freestyle apps.

Happy ABAPing! ❤️🦖🦕🦣
