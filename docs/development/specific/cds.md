---
outline: [2, 4]
---
# CDS, EML

All examples in this documentation work without CDS or EML. But on a recent ABAP release, you can use these features in abap2UI5 apps too.

### ABAP CDS
ABAP Core Data Services (CDS) let you define structured views and read data directly from the database. The example below fetches sales orders from the `I_SalesOrder` view of the Virtual Data Model (VDM) and shows them in a UI5 table:
```abap
CLASS z2ui5_cl_sample_cds DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mt_salesorder TYPE STANDARD TABLE OF I_SalesOrder WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_sample_cds IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      SELECT FROM I_SalesOrder
       FIELDS salesorder, salesordertype, salesorganization
       INTO TABLE @mt_salesorder
       UP TO 10 ROWS.

      DATA(view) = z2ui5_cl_xml_view=>factory( )->page( ).
      DATA(table) = view->table( client->_bind( mt_salesorder ) ).
      table->columns(
           )->column( )->text( `SalesOrder` )->get_parent(
           )->column( )->text( `SalesOrderType` )->get_parent(
           )->column( )->text( `SalesOrganization` ).

      table->items( )->column_list_item( )->cells(
         )->text( `{SALESORDER}`
         )->text( `{SALESORDERTYPE}`
         )->text( `{SALESORGANIZATION}` ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

### EML
The Entity Manipulation Language simplifies work with RAP business objects by offering a consistent way to read, create, update, and delete entities.

#### Read
Use `READ ENTITIES` to fetch sales orders and display them in a UI5 table:
```abap
CLASS z2ui5_cl_sample_eml_read DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mt_salesorder TYPE TABLE FOR READ RESULT i_salesordertp\\salesorder.

ENDCLASS.

CLASS z2ui5_cl_sample_eml_read IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      READ ENTITIES OF I_SalesOrderTP
        ENTITY SalesOrder
        ALL FIELDS WITH
        VALUE #( ( SalesOrder = `0000000001` ) )
        RESULT mt_salesorder.

      DATA(view) = z2ui5_cl_xml_view=>factory( )->page( ).
      DATA(table) = view->table( client->_bind( mt_salesorder ) ).
      table->columns(
           )->column( )->text( `SalesOrder` )->get_parent(
           )->column( )->text( `SalesOrderType` )->get_parent(
           )->column( )->text( `SalesOrganization` ).

      table->items( )->column_list_item( )->cells(
         )->text( `{SALESORDER}`
         )->text( `{SALESORDERTYPE}`
         )->text( `{SALESORGANIZATION}` ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

#### Modify
The example below creates a sales order through `MODIFY` inside an abap2UI5 application:

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
           WITH VALUE #( ( %cid  = `0001`
                           %data = VALUE #(
                SalesOrderType       = `TA`
                SalesOrganization    = `1010`
                DistributionChannel  = `10`
                OrganizationDivision = `00`
                SoldToParty          = `0033500056` ) ) )
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
- EML calls in abap2UI5 apps run outside the RAP framework, so explicit transaction commits (COMMIT ENTITIES) are needed.
- Restrictions inside the RAP framework, like disallowing direct calls to posting function modules or explicit commits, don't apply to abap2UI5 EML operations. You get more flexibility when managing commits and other actions.
