---
outline: [2, 4]
samples:
  - z2ui5_cl_smps_app_315
  - z2ui5_cl_smps_app_314
description: Bind a view to an OData V2 service next to the ABAP model with the set_odata_model frontend action - a named model, several models, the annotation URI, and column labels read from the service metadata.
---
# OData

By default, you bind public attributes of your class to UI5 properties with `_bind`. For cases where you need access to large datasets, you can also use existing OData services. OData offers features like pagination and growing that improve performance with large amounts of data.

## Define Additional Model
As an example, we use the test OData service `/sap/opu/odata/DMO/UI_FLIGHT_R_V2/`, available on most ABAP systems. The browser fetches the service itself, so it has to be on the same origin as the abap2UI5 page and active in `SICF`, and the logged-in user needs the authorization for it — the ABAP side of the app never reads it. The action below creates the model and exposes it under the name `FLIGHT`:
```abap
client->follow_up_action(
    val   = z2ui5_if_client=>cs_event-set_odata_model
    t_arg = VALUE #(
        ( `/sap/opu/odata/DMO/UI_FLIGHT_R_V2/` )
        ( `FLIGHT` ) ) ).
```

`t_arg` is positional: the service URL, the model name (empty for the view's default model), and an optional third argument, the URI of an annotation file the model loads with its metadata — what smart controls read their `UI.LineItem` and value-help annotations from. The model belongs to the main view: it is created once the action runs and destroyed with the next `view_display( )`, so the action goes next to the display, in the navigated branch, not into every handler.
## Bind Data
Next, bind the OData model to your view definition. Since we use a non-default model, name the model explicitly for each binding:
```abap
DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
    )->ele( n = `View` ns = `mvc`
        )->a( n = `xmlns`     v = `sap.m`
        )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

        )->ele( `Page`
            )->ele( `Table`
                )->a( n = `items`   v = `{FLIGHT>/Airport}`
                )->a( n = `growing` b = abap_true

                )->ele( `columns`
                    )->ele( `Column`
                        )->tag( `Text`
                            )->a( n = `text` v = `AirportID`
                    )->end(
                    )->ele( `Column`
                        )->tag( `Text`
                            )->a( n = `text` v = `Name`
                    )->end(
                    )->ele( `Column`
                        )->tag( `Text`
                            )->a( n = `text` v = `City`
                    )->end(
                    )->ele( `Column`
                        )->tag( `Text`
                            )->a( n = `text` v = `CountryCode`
                    )->end(

                )->end(

                )->ele( `items`
                    )->ele( `ColumnListItem`
                        )->ele( `cells`
                            )->tag( `Text`
                                )->a( n = `text` v = `{FLIGHT>AirportID}`
                            )->tag( `Text`
                                )->a( n = `text` v = `{FLIGHT>Name}`
                            )->tag( `Text`
                                )->a( n = `text` v = `{FLIGHT>City}`
                            )->tag( `Text`
                                )->a( n = `text` v = `{FLIGHT>CountryCode}` ).
```
The `growing` property loads data in batches instead of all at once, boosting performance.

## Full Example
The full source code — the view and the model are set up once, in the navigated branch; an event handler of this app would leave both alone:
```abap
CLASS zcl_sample_odata DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_odata IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->ele( `Table`
                      )->a( n = `items`   v = `{FLIGHT>/Airport}`
                      )->a( n = `growing` b = abap_true

                      )->ele( `columns`
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `AirportID`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Name`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `City`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `CountryCode`
                          )->end(

                      )->end(

                      )->ele( `items`
                          )->ele( `ColumnListItem`
                              )->ele( `cells`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{FLIGHT>AirportID}`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{FLIGHT>Name}`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{FLIGHT>City}`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{FLIGHT>CountryCode}` ).

      client->view_display( view->stringify( ) ).

      client->follow_up_action(
          val   = z2ui5_if_client=>cs_event-set_odata_model
          t_arg = VALUE #(
              ( `/sap/opu/odata/DMO/UI_FLIGHT_R_V2/` )
              ( `FLIGHT` ) ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

## Multiple OData Models
You can also bind multiple OData models at once. For example, to bind an extra OData model under the name `TRAVEL`:
```abap
DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
    )->ele( n = `View` ns = `mvc`
        )->a( n = `xmlns`     v = `sap.m`
        )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

        )->ele( `Page`
            )->ele( `Table`
                )->a( n = `items`   v = `{TRAVEL>/BookingSupplement}`
                )->a( n = `growing` b = abap_true

                )->ele( `columns`
                    )->ele( `Column`
                        )->tag( `Text`
                            )->a( n = `text` v = `TravelID`
                    )->end(
                    )->ele( `Column`
                        )->tag( `Text`
                            )->a( n = `text` v = `BookingID`
                    )->end(
                    )->ele( `Column`
                        )->tag( `Text`
                            )->a( n = `text` v = `BookingSupplementID`
                    )->end(
                    )->ele( `Column`
                        )->tag( `Text`
                            )->a( n = `text` v = `SupplementID`
                    )->end(

                )->end(

                )->ele( `items`
                    )->ele( `ColumnListItem`
                        )->ele( `cells`
                            )->tag( `Text`
                                )->a( n = `text` v = `{TRAVEL>TravelID}`
                            )->tag( `Text`
                                )->a( n = `text` v = `{TRAVEL>BookingID}`
                            )->tag( `Text`
                                )->a( n = `text` v = `{TRAVEL>BookingSupplementID}`
                            )->tag( `Text`
                                )->a( n = `text` v = `{TRAVEL>SupplementID}` ).

client->view_display( view->stringify( ) ).

client->follow_up_action(
    val   = z2ui5_if_client=>cs_event-set_odata_model
    t_arg = VALUE #(
        ( `/sap/opu/odata/DMO/API_TRAVEL_U_V2/` )
        ( `TRAVEL` ) ) ).
```

## Metadata Binding
In SAP contexts, OData services often carry extra annotations. Check the metadata definition of the service `/sap/opu/odata/DMO/API_TRAVEL_U_V2/$metadata`. The definitions for the entity `Currency`:
```xml
<EntityType Name="Currency" sap:label="Währung" sap:content-version="1">
<Key>
<PropertyRef Name="Currency"/>
</Key>
<Property Name="Currency" Type="Edm.String" Nullable="false" MaxLength="5" sap:display-format="UpperCase" sap:text="Currency_Text" sap:label="Währung" sap:quickinfo="Währungsschlüssel" sap:semantics="currency-code"/>
<Property Name="Currency_Text" Type="Edm.String" MaxLength="40" sap:label="Beschreibung" sap:creatable="false" sap:updatable="false"/>
<Property Name="Decimals" Type="Edm.Byte" sap:label="Dezimalstellen" sap:quickinfo="Anzahl Dezimalstellen"/>
<Property Name="CurrencyISOCode" Type="Edm.String" MaxLength="3" sap:display-format="UpperCase" sap:label="ISO-Code" sap:quickinfo="ISO-Währungscode"/>
<Property Name="AlternativeCurrencyKey" Type="Edm.String" MaxLength="3" sap:display-format="UpperCase" sap:label="Alternativschlüssel" sap:quickinfo="Alternativer Schlüssel"/>
<Property Name="IsPrimaryCurrencyForISOCrcy" Type="Edm.Boolean" sap:display-format="UpperCase" sap:label="primär" sap:quickinfo="primärer SAP-Währungscode zum ISO-Code"/>
</EntityType>
```
Use these SAP annotations in the UI5 view to reuse backend translations via the `label` property. The metadata binding path follows this pattern:

```text
{MODEL>/#EntityType/PropertyName/@sap:annotation}
```

- **`TRAVEL>`** — the named OData model
- **`/#Currency`** — `#` switches to the metadata document, `Currency` is the entity type name (it must exactly match `<EntityType Name="Currency">`)
- **`/Currency`** — the property name within that entity type
- **`/@sap:label`** — the SAP annotation attribute (here: the translated label text)

So `{TRAVEL>/#Currency/Currency/@sap:label}` resolves to the value of `sap:label="Währung"` from the metadata — shown in the user's logon language.

```abap

DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
    )->ele( n = `View` ns = `mvc`
        )->a( n = `xmlns`     v = `sap.m`
        )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

        )->ele( `Page`
            )->ele( `Table`
                )->a( n = `items`   v = `{TRAVEL>/Currency}`
                )->a( n = `growing` b = abap_true

                )->ele( `columns`
                    )->ele( `Column`
                        )->tag( `Text`
                            )->a( n = `text` v = `{TRAVEL>/#Currency/Currency/@sap:label}`
                    )->end(
                    )->ele( `Column`
                        )->tag( `Text`
                            )->a( n = `text` v = `{TRAVEL>/#Currency/Currency_Text/@sap:label}`
                    )->end(
                    )->ele( `Column`
                        )->tag( `Text`
                            )->a( n = `text` v = `{TRAVEL>/#Currency/Decimals/@sap:label}`
                    )->end(
                    )->ele( `Column`
                        )->tag( `Text`
                            )->a( n = `text` v = `{TRAVEL>/#Currency/CurrencyISOCode/@sap:label}`
                    )->end(
                )->end(

                )->ele( `items`
                    )->ele( `ColumnListItem`
                        )->ele( `cells`
                            )->tag( `Text`
                                )->a( n = `text` v = `{TRAVEL>Currency}`
                            )->tag( `Text`
                                )->a( n = `text` v = `{TRAVEL>Currency_Text}`
                            )->tag( `Text`
                                )->a( n = `text` v = `{TRAVEL>Decimals}`
                            )->tag( `Text`
                                )->a( n = `text` v = `{TRAVEL>CurrencyISOCode}` ).

client->view_display( view->stringify( ) ).

client->follow_up_action(
    val   = z2ui5_if_client=>cs_event-set_odata_model
    t_arg = VALUE #(
        ( `/sap/opu/odata/DMO/API_TRAVEL_U_V2/` )
        ( `TRAVEL` ) ) ).
```
UI5 now picks each column title in the user's language automatically.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Two Models in One View — one table bound to each, column headers from the metadata | [`Z2UI5_CL_SMPS_APP_315`](https://github.com/abap2UI5/samples-stack/blob/main/src/01/z2ui5_cl_smps_app_315.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `01-odata` |
| Switch Default Model — device, HTTP and OData model side by side - GWSAMPLE_BASIC | [`Z2UI5_CL_SMPS_APP_314`](https://github.com/abap2UI5/samples-stack/blob/main/src/02/z2ui5_cl_smps_app_314.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `02-smart-controls` |

<!-- samples:end -->
