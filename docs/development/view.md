---
outline: [2, 4]
---
# View

In abap2UI5, UI5 renders the UI from an XML view that you build in ABAP code. A basic example with raw XML:

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
You can use any UI5 control from the [UI5 SDK](https://sapui5.hana.ondemand.com). But writing raw XML quickly gets cumbersome. A more practical approach is the `Z2UI5_CL_XML_VIEW` helper class, which provides a fluent API for building views. The `stringify( )` method at the end serializes the view tree into an XML string that the framework sends to the frontend:

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
- Use code completion on `Z2UI5_CL_XML_VIEW` to discover controls and properties
- Browse the [samples repository](/get_started/samples) for ready-made XML examples to copy and adapt

#### What's Next?
This setup produces a static view. The next section covers how to bind and exchange data between the view and the application logic.
