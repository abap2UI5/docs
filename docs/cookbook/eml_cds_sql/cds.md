---
outline: [2, 4]
---
# CDS

All examples in these docs work without CDS. On a recent ABAP release, you can also read data through CDS views in your abap2UI5 apps.

### ABAP CDS
ABAP Core Data Services (CDS) let you define structured views and read data straight from the database. The example below fetches sales orders from the `I_SalesOrder` view of the Virtual Data Model (VDM) and shows them in a UI5 table:
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

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->ele( `Table`
                      )->a( n = `items` v = client->_bind( mt_salesorder )

                      )->ele( `columns`
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `SalesOrder`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `SalesOrderType`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `SalesOrganization`
                          )->end(

                      )->end(

                      )->ele( `items`
                          )->ele( `ColumnListItem`
                              )->ele( `cells`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{SALESORDER}`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{SALESORDERTYPE}`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{SALESORGANIZATION}` ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```
