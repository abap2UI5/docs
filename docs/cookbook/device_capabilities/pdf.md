---
outline: [2, 4]
---
# PDF

Render a PDF directly in your app — for printouts from Adobe Forms, SmartForms, archived documents from the Content Server, or anything else that produces an `xstring`.

#### Built-In Popup

The simplest path is the built-in popup `Z2UI5_CL_POP_PDF`. It expects the PDF as a `data:application/pdf;base64,...` URI and embeds it in an iframe:

```abap
METHOD z2ui5_if_app~main.

  CASE abap_true.

    WHEN client->check_on_init( ).
      client->view_display( z2ui5_cl_xml_view=>factory(
          )->page(
              )->button(
                  text  = `show PDF`
                  press = client->_event( `SHOW_PDF` )
          )->stringify( ) ).

    WHEN client->check_on_event( `SHOW_PDF` ).
      "lv_xstring contains the binary PDF — e.g. from cl_fp_function_module=>get_pdf, SmartForm OTF
      "conversion, or SELECT FROM the SOFFCONT1 archive
      DATA(lv_base64) = cl_web_http_utility=>encode_x_base64( lv_xstring ).
      DATA(lo_popup)  = z2ui5_cl_pop_pdf=>factory(
          i_title = `Invoice 4711`
          i_pdf   = `data:application/pdf;base64,` && lv_base64 ).
      client->nav_app_call( lo_popup ).

  ENDCASE.

ENDMETHOD.
```

#### Download Instead of Display

To let the user save the PDF rather than view it inline, use the [file download](./upload_download.md) pattern:

```abap
client->follow_up_action(
    val   = client->cs_event-download_b64_file
    t_arg = VALUE #( ( `data:application/pdf;base64,` && lv_base64 )
                     ( `invoice_4711.pdf` ) ) ).
```

::: tip
On older ABAP releases without `cl_web_http_utility`, use `cl_http_utility=>if_http_utility~encode_x_base64( lv_xstring )` instead.
:::
