---
outline: [2, 4]
---
# OData

By default, you bind public attributes of your class to UI5 properties with `_bind`. For cases where you need access to large datasets, you can also use existing OData services. OData offers features like pagination and growing that improve performance with large amounts of data.

#### Define Additional Model
As an example, we use the test OData service `/sap/opu/odata/DMO/UI_FLIGHT_R_V2/`, available on most ABAP systems. Make sure the service is publicly reachable. The method below defines the model and exposes it under the name `FLIGHT`:
```abap
client->follow_up_action(
    val   = z2ui5_if_client=>cs_event-set_odata_model
    t_arg = VALUE #(
        ( `/sap/opu/odata/DMO/UI_FLIGHT_R_V2/` )
        ( `FLIGHT` ) ) ).
```
#### Bind Data
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

#### Full Example
The full source code:
```abap
  METHOD z2ui5_if_app~main.

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

ENDMETHOD.
```

#### Multiple OData Models
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
For a complete code snippet, see the sample `Z2UI5_CL_SMPS_APP_315` in the [samples-stack repository](https://github.com/abap2UI5/samples-stack).

#### Metadata Binding
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
