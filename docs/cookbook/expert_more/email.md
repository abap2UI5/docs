---
outline: [2, 4]
---
# E-Mail

abap2UI5 has no e-mail control of its own — sending mail is plain ABAP via `cl_bcs_message` (or `cl_bcs` on older releases). The UI part is a normal event handler that gathers the form fields and calls the BCS API.

#### Plain Text Mail

```abap
CLASS z2ui5_cl_sample_email DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_to      TYPE string.
    DATA mv_subject TYPE string.
    DATA mv_body    TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_email IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).
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
            DATA(lo_mail) = cl_bcs_message=>create_persistent( ).
            lo_mail->set_subject( CONV #( mv_subject ) ).
            lo_mail->set_main( cl_bcs_convert=>string_to_soli( mv_body ) ).
            lo_mail->add_recipient( i_address = CONV #( mv_to ) i_address_type = `INT` ).
            lo_mail->send( i_with_error_screen = abap_false ).
            COMMIT WORK.
            client->message_toast_display( `mail queued` ).
          CATCH cx_root INTO DATA(lx).
            client->message_box_display( lx->get_text( ) ).
        ENDTRY.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

#### Attachment

Reuse the [file upload](../device_capabilities/upload_download.md) flow to capture an attachment as base64, then hand it to `cl_bcs_message`:

```abap
DATA(lv_xstring) = cl_web_http_utility=>decode_x_base64( mv_attachment_base64 ).
lo_mail->add_attachment(
    iv_attachment_type    = `PDF`
    iv_attachment_subject = mv_attachment_name
    iv_attachment_size    = CONV #( xstrlen( lv_xstring ) )
    i_attachment_content_hex = cl_bcs_convert=>xstring_to_solix( lv_xstring ) ).
```

::: tip
SAPconnect (`SCOT`) must be configured for the mail to actually leave the system. If you do not see anything in `SOST`, that is where to look first.
:::

#### Compatibility

`cl_bcs_message` is the modern API. On older releases or in ABAP Cloud check what is released for your platform — the structure of the example stays the same, only the API class changes.
