---
outline: [2, 4]
---
# CDS, EML

For compatibility reasons, all examples and snippets are provided without CDS and EML calls. However, if you're using the latest ABAP releases, you can leverage these modern features in your abap2UI5 applications.

### ABAP CDS
ABAP CDS provides a powerful way to define views and consume data from the database. The example below demonstrates how to use the `I_SalesOrder` view from the Virtual Data Model to fetch data and display it in a UI5 table control:
```abap
CLASS z2ui5_cl_sample_cds DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mt_salesorder TYPE STANDARD TABLE OF I_SalesOrder WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_sample_cds IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    SELECT FROM I_SalesOrder
     FIELDS salesorder, salesordertype, salesorganization
     INTO TABLE @DATA(mt_salesorder)
     UP TO 10 ROWS.

    DATA(view) = z2ui5_cl_xml_view=>factory( )->page( ).
    DATA(table) = view->table( client->_bind( mt_salesorder ) ).
    table->columns(
         )->column( )->text( 'SalesOrder' )->get_parent(
         )->column( )->text( 'SalesOrderType' )->get_parent(
         )->column( )->text( 'SalesOrganization' ).

    table->items( )->column_list_item( )->cells(
       )->text( '{SALESORDER}'
       )->text( '{SALESORDERTYPE}'
       )->text( '{SALESORGANIZATION}' ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

### EML
The Entity Manipulation Language simplifies the creation, update, and deletion of RAP business objects. The example below demonstrates how to create a sales order using EML in an abap2UI5 application:
```abap
METHOD z2ui5_if_app~main.

    MODIFY ENTITIES OF i_salesordertp
           ENTITY salesorder
           CREATE
           FIELDS ( salesordertype
                    salesorganization
                    distributionchannel
                    organizationdivision
                    soldtoparty )
           WITH VALUE #( ( %cid  = '0001'
                           %data = VALUE #(
                SalesOrderType       = 'TA'
                SalesOrganization    = '1010'
                DistributionChannel  = '10'
                OrganizationDivision = '00'
                SoldToParty          = '0033500056' ) ) )
           MAPPED   DATA(ls_mapped)
           FAILED   DATA(ls_failed)
           REPORTED DATA(ls_reported_modify).

    COMMIT ENTITIES BEGIN
           RESPONSE OF i_salesordertp
           FAILED   DATA(ls_save_failed)
           REPORTED DATA(ls_save_reported).
    COMMIT ENTITIES END.

ENDMETHOD.
```

Key Considerations:
* Transaction Management: EML calls in abap2UI5 apps are executed outside the RAP framework. Therefore, don't forget to explicitly commit transactions using `COMMIT ENTITIES`
* Commit Limitations: RAP enforces strict limitations, such as disallowing direct calls to posting function modules or explicit commits within its framework. These restrictions do not apply when using EML in abap2UI5 apps, allowing greater flexibility in commit management

You can also do an `READ ENTITY`:
```abap
CLASS z2ui5_cl_sample_eml_read DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mt_salesorder TYPE TABLE FOR READ RESULT i_salesordertp\\salesorder.

ENDCLASS.

CLASS z2ui5_cl_sample_eml_read IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    READ ENTITIES OF I_SalesOrderTP
      ENTITY SalesOrder
      ALL FIELDS WITH
      VALUE #( ( SalesOrder = `0000000001` ) )
      RESULT mt_salesorder.

    DATA(view) = z2ui5_cl_xml_view=>factory( )->page( ).
    DATA(table) = view->table( client->_bind( mt_salesorder ) ).
    table->columns(
         )->column( )->text( 'SalesOrder' )->get_parent(
         )->column( )->text( 'SalesOrderType' )->get_parent(
         )->column( )->text( 'SalesOrganization' ).

    table->items( )->column_list_item( )->cells(
       )->text( '{SALESORDER}'
       )->text( '{SALESORDERTYPE}'
       )->text( '{SALESORGANIZATION}' ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```
