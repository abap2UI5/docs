---
outline: [2, 4]
---
# Translation, i18n

In UI5 apps, translations are typically managed through i18n files, with translation content stored in frontend artifacts. In abap2UI5, since all apps reside on the ABAP backend, we can leverage ABAP's built-in translation mechanisms, such as text elements or message classes.

### Text Element
Messages can be translated using the ABAP text elements, making them available in different languages without changing the code:
```abap
METHOD z2ui5_if_app~main.

  data(lv_msg_translated) = 'this is a translatable message in english'(001).
  client->message_box_display( lv_msg_translated ).

ENDMETHOD.
```

### Messages
Messages are translated using message classes, ensuring that translations are managed centrally and can be maintained easily in different languages:
```abap
METHOD z2ui5_if_app~main.

  MESSAGE ID 'NET' TYPE 'I' NUMBER '001' into data(lv_msg_translated).
  client->message_box_display( lv_msg_translated ). 

ENDMETHOD.
```

### Data Element
You can also retrieve and display the short, medium, or long descriptions of data elements (DD04T). Here's how to access these text types programmatically:

::: code-group

```abap
METHOD z2ui5_if_app~main.

  DATA(ls_product_label) = lcl_help=>get_data_element_text( 'PRODUCT' ).
  client->message_box_display( |{ ls_product_label-short }: 100 | ).

ENDMETHOD.
```

```abap [LCL_HELP]
CLASS lcl_help DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_data_element_text,
        header TYPE string,
        short  TYPE string,
        medium TYPE string,
        long   TYPE string,
      END OF ty_s_data_element_text.

    CLASS-METHODS get_data_element_text
      IMPORTING
        i_data_element_name TYPE string
      RETURNING
        VALUE(result)       TYPE ty_s_data_element_text.

ENDCLASS.

CLASS lcl_help IMPLEMENTATION.

  METHOD get_data_element_text.

    DATA ddic_ref     TYPE REF TO data.
    DATA data_element TYPE REF TO object.
    DATA content      TYPE REF TO object.
    DATA: BEGIN OF ddic,
            reptext   TYPE string,
            scrtext_s TYPE string,
            scrtext_m TYPE string,
            scrtext_l TYPE string,
          END OF ddic.
    DATA exists TYPE abap_bool.

    DATA(data_element_name) = i_data_element_name.

    TRY.
        cl_abap_typedescr=>describe_by_name( 'T100' ).

        DATA(struct_desrc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( 'DFIES' ) ).

        CREATE DATA ddic_ref TYPE HANDLE struct_desrc.
        ASSIGN ddic_ref->* TO FIELD-SYMBOL(<ddic>).
        ASSERT sy-subrc = 0.

        cl_abap_elemdescr=>describe_by_name( EXPORTING  p_name      = data_element_name
                                             RECEIVING  p_descr_ref = DATA(lo_typedescr)
                                             EXCEPTIONS OTHERS      = 1 ).
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        DATA(data_descr) = CAST cl_abap_datadescr( lo_typedescr ).

        CALL METHOD data_descr->('GET_DDIC_FIELD')
          RECEIVING
            p_flddescr   = <ddic>
          EXCEPTIONS
            not_found    = 1
            no_ddic_type = 2
            OTHERS       = 3.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        ddic = CORRESPONDING #( <ddic> ).
        result-header = ddic-reptext.
        result-short  = ddic-scrtext_s.
        result-medium = ddic-scrtext_m.
        result-long   = ddic-scrtext_l.

      CATCH cx_root.
        TRY.
            DATA(xco_cp_abap_dictionary) = 'XCO_CP_ABAP_DICTIONARY'.
            CALL METHOD (xco_cp_abap_dictionary)=>('DATA_ELEMENT')
              EXPORTING
                iv_name         = data_element_name
              RECEIVING
                ro_data_element = data_element.

            CALL METHOD data_element->('IF_XCO_AD_DATA_ELEMENT~EXISTS')
              RECEIVING
                rv_exists = exists.

            IF exists = abap_false.
              RETURN.
            ENDIF.

            CALL METHOD data_element->('IF_XCO_AD_DATA_ELEMENT~CONTENT')
              RECEIVING
                ro_content = content.

            CALL METHOD content->('IF_XCO_DTEL_CONTENT~GET_HEADING_FIELD_LABEL')
              RECEIVING
                rs_heading_field_label = result-header.

            CALL METHOD content->('IF_XCO_DTEL_CONTENT~GET_SHORT_FIELD_LABEL')
              RECEIVING
                rs_short_field_label = result-short.

            CALL METHOD content->('IF_XCO_DTEL_CONTENT~GET_MEDIUM_FIELD_LABEL')
              RECEIVING
                rs_medium_field_label = result-medium.

            CALL METHOD content->('IF_XCO_DTEL_CONTENT~GET_LONG_FIELD_LABEL')
              RECEIVING
                rs_long_field_label = result-long.

          CATCH cx_root.
        ENDTRY.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
```

:::
