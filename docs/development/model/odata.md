---
outline: [2, 4]
---
# OData

By default, you can bind all public attributes of your implementation class to UI5 properties, enabling the display of internal tables and, with bind_edit, even updating data. Additionally, in scenarios where direct access to database tables is required, using pre-defined OData services can be beneficial. Leveraging OData protocols provides features such as pagination and growing, which enhance performance when handling large datasets.

#### Define Additional Model
As an example, we will use the test OData service `/sap/opu/odata/DMO/UI_FLIGHT_R_V2/`, which is available in most ABAP systems. Ensure the service is publicly accessible. Use the following method to define the model and make it available under the name `FLIGHT`:
```abap
client->follow_up_action( client->_event_client(
    val = z2ui5_if_client=>cs_event-set_odata_model
    t_arg = VALUE #(
        ( `/sap/opu/odata/DMO/UI_FLIGHT_R_V2/` )
        ( `FLIGHT` ) ) ) ).
```
#### Bind Data
Next, bind this OData model to your view definition. Since we’re using a non-default model, we must explicitly specify the model name for each binding. Here's an example:
```abap
DATA(tab) = z2ui5_cl_xml_view=>factory( )->page( )->table(
    items = `{FLIGHT>/Airport}`
    growing = abap_true ).

tab->columns(
    )->column( )->text( `AirportID` )->get_parent(
    )->column( )->text( `Name` )->get_parent(
    )->column( )->text( `City` )->get_parent(
    )->column( )->text( `CountryCode` ).
 
tab->items( )->column_list_item( )->cells(
    )->text( `{FLIGHT>AirportID}`
    )->text( `{FLIGHT>Name}`
    )->text( `{FLIGHT>City}`
    )->text( `{FLIGHT>CountryCode}` ).
```
By using the growing property we can make use of the feature that not all data is loaded at once, leveraging performance.

#### Full Example
Here's the complete source code:
```abap
  METHOD z2ui5_if_app~main.

    DATA(tab) = z2ui5_cl_xml_view=>factory( )->page( )->table(
        items   = `{FLIGHT>/Airport}`
        growing = abap_true ).

    tab->columns(
        )->column( )->text( `AirportID` )->get_parent(
        )->column( )->text( `Name` )->get_parent(
        )->column( )->text( `City` )->get_parent(
        )->column( )->text( `CountryCode` ).
 
    tab->items( )->column_list_item( )->cells(
        )->text( `{FLIGHT>AirportID}`
        )->text( `{FLIGHT>Name}`
        )->text( `{FLIGHT>City}`
        )->text( `{FLIGHT>CountryCode}` ).

    client->view_display( tab->stringify( ) ).

    client->follow_up_action( client->_event_client(
        val   = z2ui5_if_client=>cs_event-set_odata_model
        t_arg = VALUE #(
            ( `/sap/opu/odata/DMO/UI_FLIGHT_R_V2/` )
            ( `FLIGHT` ) ) ) ).
 
ENDMETHOD.
```

#### Multiple OData Models
You can also bind multiple OData models simultaneously. For example, here’s how to bind an additional OData model under the name `TRAVEL`:
```abap
DATA(tab) = z2ui5_cl_xml_view=>factory( )->page( )->table(
    items   = `{TRAVEL>/BookingSupplement}`
    growing = abap_true ).

tab->columns(
    )->column( )->text( `TravelID` )->get_parent(
    )->column( )->text( `BookingID` )->get_parent(
    )->column( )->text( `BookingSupplementID` )->get_parent(
    )->column( )->text( `SupplementID` )->get_parent( ).

tab->items( )->column_list_item( )->cells(
    )->text( `{TRAVEL>TravelID}`
    )->text( `{TRAVEL>BookingID}`
    )->text( `{TRAVEL>BookingSupplementID}`
    )->text( `{TRAVEL>SupplementID}` ).

client->view_display( tab->stringify( ) ).

client->follow_up_action( client->_event_client(
    val   = z2ui5_if_client=>cs_event-set_odata_model
    t_arg = VALUE #(
        ( `/sap/opu/odata/DMO/API_TRAVEL_U_V2/` )
        ( `TRAVEL` ) ) ) ).  
```
For a fully functional code snippet, check out the sample `Z2UI5_CL_DEMO_APP_315`.

#### Metadata Binding
In SAP contexts, OData services are often enriched with additional annotations. Check the metadata definition of the service `/sap/opu/odata/DMO/API_TRAVEL_U_V2/$metadata`. You find the definitions for the entity `Currency`:
```xml
<EntityType Name="CurrencyType" sap:label="Währung" sap:content-version="1">
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
We can use these SAP annotations in our UI5 view to utilize backend translations via the property `label`. Here’s an example:
```abap

DATA(tab) = page->table(
    items   = `{TRAVEL>/Currency}`
    growing = abap_true ).

tab->columns(
    )->column( )->text( `{TRAVEL>/#Currency/Currency/@sap:label}` )->get_parent(
    )->column( )->text( `{TRAVEL>/#Currency/Currency_Text/@sap:label}` )->get_parent(
    )->column( )->text( `{TRAVEL>/#Currency/Decimals/@sap:label}` )->get_parent(
    )->column( )->text( `{TRAVEL>/#Currency/CurrencyISOCode/@sap:label}` ).

tab->items( )->column_list_item( )->cells(
    )->text( `{TRAVEL>Currency}`
    )->text( `{TRAVEL>Currency_Text}`
    )->text( `{TRAVEL>Decimals}`
    )->text( `{TRAVEL>CurrencyISOCode}` ).

client->view_display( tab->stringify( ) ).

client->follow_up_action( client->_event_client(
    val   = z2ui5_if_client=>cs_event-set_odata_model
    t_arg = VALUE #(
        ( `/sap/opu/odata/DMO/API_TRAVEL_U_V2/` )
        ( `TRAVEL` ) ) ) ).  
```
The column titles are now automatically set with the correct title in the user’s language.
