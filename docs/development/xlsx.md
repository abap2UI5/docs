---
outline: [2, 6]
---
# XLSX

The abap2UI5 framework allows you to leverage the existing XLSX features of your ABAP system. You can implement file uploads or downloads, converting the contents of XLSX files into internal ABAP tables and vice versa.

#### Upload

Transform uploaded xlsx files into an internal table:

::: code-group

```abap
CLASS z2ui5_cl_sample_upload DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES Z2UI5_if_app.
    DATA mv_path  TYPE string.
    DATA mv_value TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_upload IMPLEMENTATION.
    METHOD z2ui5_if_app~main.

        client->view_display( z2ui5_cl_xml_view=>factory(
            )->page(
                )->_z2ui5( )->file_uploader(
                    value       = client->_bind_edit( mv_value )
                    path        = client->_bind_edit( mv_path )
                    placeholder = 'filepath here...'
                    upload      = client->_event( 'UPLOAD' )
            )->stringify( ) ).

        IF client->get( )-event = 'UPLOAD'.

            data(lr_itab) = lcl_help=>itab_get_by_xlsx( mv_value ).
            "further process with itab...
            client->message_box_display( `xlsx uploaded` ).
        ENDIF.

    ENDMETHOD.
ENDCLASS.
```

```abap [lcl_help]
CLASS lcl_help DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS itab_get_by_xlsx
      IMPORTING
        VALUE(val)    TYPE string
      RETURNING
        VALUE(result) TYPE REF TO data.

ENDCLASS.

CLASS lcl_help IMPLEMENTATION.

  METHOD itab_get_by_xlsx.

    SPLIT val AT `;` INTO DATA(lv_dummy) DATA(lv_data).
    SPLIT lv_data AT `,` INTO lv_dummy lv_data.

    DATA(lv_xdata) = z2ui5_cl_util=>conv_decode_x_base64( lv_data ).
    DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet(
        document_name = `test`
        xdocument     = lv_xdata ) .

    lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
      IMPORTING worksheet_names = DATA(lt_worksheets) ).

    result = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_worksheets[ 1 ] ).

  ENDMETHOD.

ENDCLASS.
```
:::

#### Download

Convert an internal table to an XLSX file and download it to the frontend:

::: code-group

```abap
  METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
        )->page(
            )->button(
                text = 'Open Download Popup'
                press = client->_event( 'DOWNLOAD' )
        )->stringify( ) ).

    IF client->get( )-event = `DOWNLOAD`.

        TYPES:
          BEGIN OF ty_row,
            count TYPE i,
            value TYPE string,
            descr TYPE string,
          END OF ty_row.
        TYPES ty_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

        DATA(lt_tab) = VALUE ty_tab(
        ( count = '1' value = `red` descr = `this is a description` )
        ( count = '2' value = `red` descr = `this is a description` )
        ( count = '3' value = `red` descr = `this is a description` ) ).

        DATA(lv_file) = lcl_help=>xlsx_get_by_itab( lt_tab ).
        client->follow_up_action( val = client->_event_client(
            val = client->cs_event-download_b64_file
            t_arg = VALUE #( ( lv_file ) ( `test.xlsx` ) ) ) ).
    ENDIF.

ENDMETHOD.
```

```abap [lcl_help]
class lcl_help DEFINITION.

PUBLIC SECTION.
    CLASS-METHODS xlsx_get_by_itab
      IMPORTING
        VALUE(val)    TYPE STANDARD TABLE
      RETURNING
        VALUE(result) TYPE string.

endclass.

CLASS lcl_help IMPLEMENTATION.

  METHOD xlsx_get_by_itab.

    DATA(lt_data) = REF #( val ).

    FIELD-SYMBOLS: <tab> TYPE STANDARD TABLE.
    ASSIGN lt_data->* TO <tab>.
    TRY.
        cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = DATA(lo_salv)
        CHANGING
          t_table      = <tab> ).

        DATA(lt_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                 r_columns      = lo_salv->get_columns( )
                                 r_aggregations = lo_salv->get_aggregations( ) ).
      CATCH cx_salv_msg.
        RETURN.
    ENDTRY.

    cl_salv_bs_lex=>export_from_result_data_table(
      EXPORTING
        is_format            = if_salv_bs_lex_format=>mc_format_xlsx
        ir_result_data_table =  cl_salv_ex_util=>factory_result_data_table(
            r_data           = lt_data
            t_fieldcatalog   = lt_fcat )
      IMPORTING
        er_result_file       = DATA(lv_xstring) ).

    result = z2ui5_cl_util=>conv_encode_x_base64( lv_xstring ).
    result = `data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,` && result.

  ENDMETHOD.

ENDCLASS.
```
:::

::: tip **ABAP Cloud**
The snippets provided above are not compatible with ABAP Cloud. To make them compatible, replace the code in the lcl_help class with functions from the new XCO_CP_XLSX APIs.
:::

#### abap2xlsx
Instead of using the code in `lcl_help`, consider taking the easy route and leveraging the wonderful open-source project [abap2xlsx](https://github.com/abap2xlsx/abap2xlsx), which offers reusable APIs for all common XLSX operations. It works entirely within the ABAP stack and, therefore, seamlessly with abap2UI5.

#### UI5 Control
If you want to export the data directly at the frontend, SAP offers the sap.ui.export.Spreadsheet control to export table content. With some additional logic, this control is also usable with abap2UI5. Check out the UI-Extension add-on for a running sample [here.](/addons/popup) However, the programming effort might be higher compared to the file-based approach shown above.
