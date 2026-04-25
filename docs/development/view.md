---
outline: [2, 4]
---
# View

In abap2UI5, UI5 renders the UI from an XML view that you build in ABAP. A simple example with raw XML:

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
You can use any UI5 control from the [UI5 SDK](https://sapui5.hana.ondemand.com). But writing raw XML quickly turns cumbersome. A handier approach is the `Z2UI5_CL_XML_VIEW` helper class, with a fluent API for building views. The `stringify( )` method at the end serializes the view tree into an XML string that the framework sends to the frontend:

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
- Use code completion on `Z2UI5_CL_XML_VIEW` to find controls and properties
- See the [samples repository](/get_started/samples) for ready-made XML examples to copy and adapt

#### What's Next?
This produces a static view. The next section shows how to bind and share data between the view and the app logic.
