---
outline: [2, 4]
description: Send a mail from an abap2UI5 event handler with the classic BCS API - cl_bcs, cl_document_bcs and cl_cam_address_bcs - with an attachment from an uploaded file, and what ABAP Cloud offers instead.
---
# E-Mail

abap2UI5 has no e-mail control of its own — sending mail is plain ABAP via the Business Communication Services (BCS). The UI part is a normal event handler that gathers the form fields and calls the BCS API.

## Plain Text Mail

The classic BCS API is `cl_bcs` for the send request, `cl_document_bcs` for the document and `cl_cam_address_bcs` for the addresses. It is Standard ABAP: the three classes are not released for ABAP Cloud, see [Compatibility](#compatibility) below.

<!-- playground: no Run button — cl_bcs is an on-premise class the browser runtime does not implement -->
```abap
CLASS zcl_sample_email DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_to      TYPE string.
    DATA mv_subject TYPE string.
    DATA mv_body    TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_email IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_navigated( ).
        DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
            )->ele( n = `View` ns = `mvc`
                )->a( n = `xmlns`      v = `sap.m`
                )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
                )->a( n = `xmlns:form` v = `sap.ui.layout.form`

                )->ele( `Page`
                    )->a( n = `title` v = `Send E-Mail`

                    )->ele( n = `SimpleForm` ns = `form`
                        )->a( n = `editable` b = abap_true

                        )->ele( n = `content` ns = `form`
                            )->tag( `Label`
                                )->a( n = `text` v = `To`
                            )->tag( `Input`
                                )->a( n = `value` v = client->_bind( mv_to )
                            )->tag( `Label`
                                )->a( n = `text` v = `Subject`
                            )->tag( `Input`
                                )->a( n = `value` v = client->_bind( mv_subject )
                            )->tag( `Label`
                                )->a( n = `text` v = `Body`
                            )->tag( `TextArea`
                                )->a( n = `value` v = client->_bind( mv_body )
                            )->tag( `Button`
                                )->a( n = `text`  v = `send`
                                )->a( n = `press` v = client->_event( `SEND` ) ).

        client->view_display( view->stringify( ) ).

      WHEN client->check_on_event( `SEND` ).
        TRY.
            DATA(lo_request)  = cl_bcs=>create_persistent( ).
            DATA(lo_document) = cl_document_bcs=>create_document(
                i_type    = `RAW`
                i_text    = cl_bcs_convert=>string_to_soli( mv_body )
                i_subject = CONV so_obj_des( mv_subject ) ).
            lo_request->set_document( lo_document ).
            lo_request->add_recipient(
                i_recipient = cl_cam_address_bcs=>create_internet_address( CONV ad_smtpadr( mv_to ) ) ).
            lo_request->send( ).
            COMMIT WORK.
            client->message_toast_display( `mail queued` ).
          CATCH cx_bcs INTO DATA(lx).
            client->message_box_display( lx ).
        ENDTRY.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

`send( )` hands the request to SAPconnect; the `COMMIT WORK` is what actually queues it, so a handler that forgets it sends nothing and reports no error. `message_box_display( )` takes the exception object as it is and shows its text.

## Attachment

Reuse the [file upload](/cookbook/device_capabilities/upload_download) flow to capture an attachment as a base64 data URL, decode its payload, and add it to the document before `set_document( )`:

```abap
SPLIT mv_attachment_base64 AT `,` INTO DATA(lv_header) DATA(lv_payload).
DATA(lv_xstring) = cl_web_http_utility=>decode_x_base64( lv_payload ).

lo_document->add_attachment(
    i_attachment_type    = `PDF`
    i_attachment_subject = CONV so_obj_des( mv_attachment_name )
    i_att_content_hex    = cl_bcs_convert=>xstring_to_solix( lv_xstring ) ).
```

`i_attachment_type` is the three-letter document class of the SAP office functions — `PDF`, `XLS`, `BIN` — not the MIME type of the data URL.

::: tip
SAPconnect (`SCOT`) must be configured for the mail to actually leave the system. If you do not see anything in `SOST`, that is where to look first.
:::

## Compatibility

The example needs Standard ABAP: `cl_bcs`, `cl_document_bcs` and `cl_cam_address_bcs` are not released for ABAP Cloud, and neither is `cl_web_http_utility` on every release — older systems have `cl_http_utility=>if_http_utility~encode_x_base64( )` instead. In ABAP Cloud (BTP ABAP Environment, S/4 Public Cloud) the released mail API is `cl_bcs_mail_message`; the structure of the handler stays the same, only the API class changes.
