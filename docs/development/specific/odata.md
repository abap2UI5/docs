# OData

By default, you can bind all public attributes of your implementation class to UI5 properties. This allows you to display data, and, with bind_edit, even update data. In some scenarios, using pre-defined OData services can be particularly helpful, especially for displaying data in tables. Leveraging OData protocols enables features like pagination and growing, which improve performance when handling large datasets.

### Define Second Model
As an example, we will use the test OData service `/sap/opu/odata/DMO/API_TRAVEL_U_V2/`, which is available in most ABAP systems. Ensure the service is publicly accessible before use. Use the following method to define the model and make it available under the name `TRAVEL`:
```abap
    client->follow_up_action( client->_event_client(
        val = z2ui5_if_client=>cs_event-set_odata_model
        t_arg = value #(
            ( `/sap/opu/odata/DMO/API_TRAVEL_U_V2/` )
            ( `TRAVEL` ) ) ) ).
```
### Bind Data
Next, bind this OData model to your view definition. Since we’re using a non-default model, we must explicitly specify the model name for each binding. Here's an example:
```abap
    DATA(view) = z2ui5_cl_xml_view=>factory( )->page( ).
    DATA(tab) = view->table(
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
By using the growing property we can make use of the feautre that not all data is loaded at once, leveraging performance when accessing large tables.

### Multiple OData Models
You can bind multiple OData models simultaneously by defining additional OData models. Here’s an example:
```abap
    tab = page->table(
        items = `{FLIGHT>/Airport}`
        growing = abap_true ).
 
    tab->columns(
        )->column(  )->text( 'AirportID' )->get_parent(
        )->column( )->text( 'Name' )->get_parent(
        )->column( )->text( 'City' )->get_parent(
        )->column( )->text( 'CountryCode' )->get_parent( ).
 
    tab->items( )->column_list_item( )->cells(
        )->text( '{FLIGHT>AirportID}'
        )->text( '{FLIGHT>Name}'
        )->text( '{FLIGHT>City}'
        )->text( '{FLIGHT>CountryCode}' ).

    client->follow_up_action( client->_event_client(
        val = z2ui5_if_client=>cs_event-set_odata_model
        t_arg = value #(
            ( `/sap/opu/odata/DMO/ui_flight_r_v2/` )
            ( `FLIGHT` )  ) ) ).      
```

### Full Example
Here’s the complete source code for one OData model:
```abap
METHOD z2ui5_if_app~main.
  
    DATA(view) = z2ui5_cl_xml_view=>factory( )->page( ).
    DATA(tab) = view->table(
        items = `{TRAVEL>/BookingSupplement}`
        growing = abap_true ).
 
    tab->columns(
          )->column(  )->text( 'TravelID' )->get_parent(
          )->column( )->text( 'BookingID' )->get_parent(
          )->column( )->text( 'BookingSupplementID' )->get_parent(
          )->column( )->text( 'SupplementID' )->get_parent(
          )->column( )->text( 'SupplementText' )->get_parent(
          )->column( )->text( 'Price' )->get_parent(
          )->column( )->text( 'CurrencyCode' )->get_parent( ).
 
    tab->items( )->column_list_item( )->cells(
         )->text( '{TRAVEL>TravelID}'
         )->text( '{TRAVEL>BookingID}'
         )->text( '{TRAVEL>BookingSupplementID}'
         )->text( '{TRAVEL>SupplementID}'
         )->text( '{TRAVEL>SupplementText}'
         )->text( '{TRAVEL>Price}'
         )->text( '{TRAVEL>CurrencyCode}' ).
 
    client->follow_up_action( client->_event_client(
        val = z2ui5_if_client=>cs_event-set_odata_model
        t_arg = value #(
            ( `/sap/opu/odata/DMO/API_TRAVEL_U_V2/` )
            ( `TRAVEL` ) ) ) ).
 
ENDMETHOD.
```
Check out sample `Z2UI5_CL_DEMO_APP_315` for a running code snippet.


