---
outline: [2, 4]
---
# OData

By default, you can bind all public attributes of your implementation class to UI5 properties, enabling the display of internal tables and, with bind_edit, even updating data. Additionally, in scenarios where direct access to database tables is required, using pre-defined OData services can be beneficial. Leveraging OData protocols provides features such as pagination and growing, which enhance performance when handling large datasets.

#### Define Additional Model
As an example, we will use the test OData service `/sap/opu/odata/DMO/API_TRAVEL_U_V2/`, which is available in most ABAP systems. Ensure the service is publicly accessible. Use the following method to define the model and make it available under the name `FLIGHT`:
```abap
client->follow_up_action( client->_event_client(
    val = z2ui5_if_client=>cs_event-set_odata_model
    t_arg = value #(
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
    )->column(  )->text( 'AirportID' )->get_parent(
    )->column( )->text( 'Name' )->get_parent(
    )->column( )->text( 'City' )->get_parent(
    )->column( )->text( 'CountryCode' ).
 
tab->items( )->column_list_item( )->cells(
    )->text( '{FLIGHT>AirportID}'
    )->text( '{FLIGHT>Name}'
    )->text( '{FLIGHT>City}'
    )->text( '{FLIGHT>CountryCode}' ).
```
By using the growing property we can make use of the feature that not all data is loaded at once, leveraging performance.

#### Full Example
Here’s the complete source code:
```abap
METHOD z2ui5_if_app~main.
  
    DATA(tab) = z2ui5_cl_xml_view=>factory( )->page( )->table(
        items = `{FLIGHT>/Airport}`
        growing = abap_true ).

    tab->columns(
        )->column(  )->text( 'AirportID' )->get_parent(
        )->column( )->text( 'Name' )->get_parent(
        )->column( )->text( 'City' )->get_parent(
        )->column( )->text( 'CountryCode' ).
 
    tab->items( )->column_list_item( )->cells(
        )->text( '{FLIGHT>AirportID}'
        )->text( '{FLIGHT>Name}'
        )->text( '{FLIGHT>City}'
        )->text( '{FLIGHT>CountryCode}' ).
 
    client->follow_up_action( client->_event_client(
        val = z2ui5_if_client=>cs_event-set_odata_model
        t_arg = value #(
            ( `/sap/opu/odata/DMO/UI_FLIGHT_R_V2/` )
            ( `FLIGHT` ) ) ) ).
 
ENDMETHOD.
```

#### Multiple OData Models
You can also bind multiple OData models simultaneously. Here’s an example:
```abap
DATA(tab) = z2ui5_cl_xml_view=>factory( )->page( )->table(
    items = `{TRAVEL>/BookingSupplement}`
    growing = abap_true ).

tab->columns(
    )->column(  )->text( 'TravelID' )->get_parent(
    )->column( )->text( 'BookingID' )->get_parent(
    )->column( )->text( 'BookingSupplementID' )->get_parent(
    )->column( )->text( 'SupplementID' )->get_parent( ).

tab->items( )->column_list_item( )->cells(
    )->text( '{TRAVEL>TravelID}'
    )->text( '{TRAVEL>BookingID}'
    )->text( '{TRAVEL>BookingSupplementID}'
    )->text( '{TRAVEL>SupplementID}' ).

client->follow_up_action( client->_event_client(
    val = z2ui5_if_client=>cs_event-set_odata_model
    t_arg = value #(
        ( `/sap/opu/odata/DMO/API_TRAVEL_U_V2/` )
        ( `TRAVEL` ) ) ) ).  
```
For a fully functional code snippet, check out the sample `Z2UI5_CL_DEMO_APP_315`.
