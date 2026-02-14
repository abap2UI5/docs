---
outline: [2, 4]
---
# View

In abap2UI5, the UI is rendered from a UI5 XML view that you build in your ABAP code. Here's a basic example using raw XML:

```abap
  METHOD z2ui5_if_app~main.

    client->view_display(
        |<mvc:View xmlns="sap.m" xmlns:core="sap.ui.core" xmlns:mvc="sap.ui.core.mvc" | &
        |          displayBlock="true" height="100%">| &
        |  <Shell>| &
        |     <Page title="My title">| &
        |         <Text text="My text"/>| &
        |     </Page>| &
        |  </Shell>| &
        |</mvc:View>| ).

ENDMETHOD.
```
You can use any UI5 control available in the [UI5 SDK](https://sapui5.hana.ondemand.com). However, writing raw XML quickly gets cumbersome. A more practical approach is to use the `Z2UI5_CL_XML_VIEW` helper class, which provides a fluent API for building views. The `stringify( )` method at the end serializes the view tree into an XML string that the framework sends to the frontend:

```abap
  METHOD z2ui5_if_app~main.

    client->view_display(
        z2ui5_cl_xml_view=>factory(
            )->shell(
                )->page( `My title`
                    )->text( `My text`
        )->stringify( ) ).

  ENDMETHOD.
```

Tips for working with views:
* Use code completion on `Z2UI5_CL_XML_VIEW` to explore available controls and properties
* Check the [sample repository](/get_started/samples) for ready-made XML examples you can copy and adjust

#### What's Next?
This setup produces a static view. In the next section, you'll learn how to bind and exchange data between the view and your application logic.
