---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_011
  - z2ui5_cl_smp_app_019
  - z2ui5_cl_smp_app_006
  - z2ui5_cl_smp_app_045
  - z2ui5_cl_smp_app_053
  - z2ui5_cl_smp_app_059
  - z2ui5_cl_smp_app_070
  - z2ui5_cl_smp_app_160
  - z2ui5_cl_smp_app_143
  - z2ui5_cl_smp_app_459
---
# Tables

This section walks through rendering tabular and nested data in views.

### Basic Table
The example below binds a simple table to a UI5 control:
```abap
CLASS z2ui5_cl_sample_tab DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    TYPES:
      BEGIN OF ty_row,
        count      TYPE i,
        value      TYPE string,
        descr      TYPE string,
      END OF ty_row.
    DATA mt_itab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_sample_tab IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      DO 100 TIMES.
        INSERT VALUE #(
          count = sy-index
          value = `red`
          descr = `this is a description` ) INTO TABLE mt_itab.
      ENDDO.

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->ele( `Table`
                      )->a( n = `items` v = client->_bind( mt_itab )

                      )->ele( `columns`
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Count`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Value`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Description`
                          )->end(

                      )->end(

                      )->ele( `items`
                          )->ele( `ColumnListItem`
                              )->ele( `cells`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{COUNT}`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{VALUE}`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{DESCR}` ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

### Editable
To make a table editable, use editable cell controls (e.g. `input`) — the binding is the same `_bind`:
```abap
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      DO 100 TIMES.
        INSERT VALUE #(
          count = sy-index
          value = `red`
          descr = `this is a description` ) INTO TABLE mt_itab.
      ENDDO.

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->ele( `Table`
                      )->a( n = `items` v = client->_bind( mt_itab )

                      )->ele( `columns`
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Count`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Value`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Description`
                          )->end(

                      )->end(

                      )->ele( `items`
                          )->ele( `ColumnListItem`
                              )->ele( `cells`
                                  )->tag( `Input`
                                      )->a( n = `value` v = `{COUNT}`
                                  )->tag( `Input`
                                      )->a( n = `value` v = `{VALUE}`
                                  )->tag( `Input`
                                      )->a( n = `value` v = `{DESCR}` ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
```

### Nested Structures
You can also bind nested structures — use `structure/component` as the binding path:
```abap
CLASS z2ui5_cl_sample_nested_structures DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_tab,
        product TYPE string,
        BEGIN OF s_details,
          create_date TYPE string,
          create_by   TYPE string,
        END OF s_details,
      END OF ty_s_tab.
    DATA mt_itab TYPE STANDARD TABLE OF ty_s_tab WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_sample_nested_structures IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    mt_itab = VALUE #(
        ( product = `table` s_details = VALUE #( create_date = `01.01.2023` create_by = `Peter`  ) )
        ( product = `chair` s_details = VALUE #( create_date = `25.10.2022` create_by = `Frank`  ) )
        ( product = `sofa`  s_details = VALUE #( create_date = `12.03.2024` create_by = `George` ) ) ).

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Table`
                )->a( n = `items` v = client->_bind( mt_itab )

                )->ele( `columns`
                    )->ele( `Column`
                        )->tag( `Text`
                            )->a( n = `text` v = `Product`
                    )->end(
                    )->ele( `Column`
                        )->tag( `Text`
                            )->a( n = `text` v = `Created at`
                    )->end(
                    )->ele( `Column`
                        )->tag( `Text`
                            )->a( n = `text` v = `By`
                    )->end(

                )->end(

                )->ele( `items`
                    )->ele( `ColumnListItem`
                        )->ele( `cells`
                            )->tag( `Text`
                                )->a( n = `text` v = `{PRODUCT}`
                            )->tag( `Text`
                                " abap2ui5lint-disable-next-line unknown-binding-path -- linter defect, fixed in @abap2ui5/linter 0.2.0: a nested BEGIN OF inside a row type was dropped. Delete this line with the pin bump; the path is correct
                                )->a( n = `text` v = `{S_DETAILS/CREATE_DATE}`
                            )->tag( `Text`
                                " abap2ui5lint-disable-next-line unknown-binding-path -- same, and it goes with the same pin bump
                                )->a( n = `text` v = `{S_DETAILS/CREATE_BY}` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Editable Cells, Add and Delete Rows | [`Z2UI5_CL_SMP_APP_011`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_011.clas.abap) |
| Selection Modes: Single and Multi Select | [`Z2UI5_CL_SMP_APP_019`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_019.clas.abap) |
| Large Table with Growing and ScrollContainer | [`Z2UI5_CL_SMP_APP_006`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_006.clas.abap) |
| Filter Rows in the Backend | [`Z2UI5_CL_SMP_APP_045`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_045.clas.abap) |
| Search in the Backend (SearchField) | [`Z2UI5_CL_SMP_APP_053`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_053.clas.abap) |
| Live Search with Parallel Requests | [`Z2UI5_CL_SMP_APP_059`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_059.clas.abap) |
| Full Example with sap.ui.table | [`Z2UI5_CL_SMP_APP_070`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_070.clas.abap) |
| Events on Cell Level | [`Z2UI5_CL_SMP_APP_160`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_160.clas.abap) |
| Keep Column Filters on Refresh (C) | [`Z2UI5_CL_SMP_APP_143`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_143.clas.abap) |
| Drag and Drop Rows (A) | [`Z2UI5_CL_SMP_APP_459`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_459.clas.abap) |

<!-- samples:end -->
