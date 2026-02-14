# CL_DEMO_OUTPUT

If you're familiar with `CL_DEMO_OUTPUT` from classic ABAP, you can display its HTML output inside an abap2UI5 app as well. This is handy for quick data visualization or when migrating existing demo code.

The approach: generate the HTML with `cl_demo_output=>get( )`, inject CSS styles via `_cc_plain_xml` (which inserts raw XML/HTML into the view), and render the result with the UI5 `html` control. The CSS block is needed because `CL_DEMO_OUTPUT` produces HTML with specific class names (e.g. `heading1`, `header`, `body`) that require matching style definitions to display correctly.

::: tip
This technique is best suited for prototyping or migrating existing demo programs. For production apps, consider building proper UI5 views instead.
:::

```abap
METHOD z2ui5_if_app~main.

    DATA(lv_style) = `<html:style type="text/css">body {` && |\n|  &&
                                      `     font-family: Arial;` && |\n|  &&
                                      `     font-size: 90%;` && |\n|  &&
                                      `}` && |\n|  &&
                                      `table {` && |\n|  &&
                                      `     font-family: Arial;` && |\n|  &&
                                      `     font-size: 90%;` && |\n|  &&
                                      `}` && |\n|  &&
                                      `caption {` && |\n|  &&
                                      `     font-family: Arial;` && |\n|  &&
                                      `     font-size: 90%;` && |\n|  &&
                                      `     font-weight:bold;` && |\n|  &&
                                      `     text-align:left;` && |\n|  &&
                                      `}` && |\n|  &&
                                      `span.heading1 {` && |\n|  &&
                                      `    font-size: 150%;` && |\n|  &&
                                      `     color:#000080;` && |\n|  &&
                                      `     font-weight:bold;` && |\n|  &&
                                      `}` && |\n|  &&
                                      `span.heading2 {` && |\n|  &&
                                      `    font-size: 135%;` && |\n|  &&
                                      `     color:#000080;` && |\n|  &&
                                      `     font-weight:bold;` && |\n|  &&
                                      `}` && |\n|  &&
                                      `span.heading3 {` && |\n|  &&
                                      `    font-size: 120%;` && |\n|  &&
                                      `     color:#000080;` && |\n|  &&
                                      `     font-weight:bold;` && |\n|  &&
                                      `}` && |\n|  &&
                                      `span.heading4 {` && |\n|  &&
                                      `    font-size: 105%;` && |\n|  &&
                                      `     color:#000080;` && |\n|  &&
                                      `     font-weight:bold;` && |\n|  &&
                                      `}` && |\n|  &&
                                      `span.normal {` && |\n|  &&
                                      `    font-size: 100%;` && |\n|  &&
                                      `     color:#000000;` && |\n|  &&
                                      `     font-weight:normal;` && |\n|  &&
                                      `}` && |\n|  &&
                                      `span.nonprop {` && |\n|  &&
                                      `    font-family: Courier New;` && |\n|  &&
                                      `     font-size: 100%;` && |\n|  &&
                                      `     color:#000000;` && |\n|  &&
                                      `     font-weight:400;` && |\n|  &&
                                      `}` && |\n|  &&
                                      `span.nowrap {` && |\n|  &&
                                      `    white-space:nowrap;` && |\n|  &&
                                      `}` && |\n|  &&
                                      `span.nprpnwrp {` && |\n|  &&
                                      `    font-family: Courier New;` && |\n|  &&
                                      `     font-size: 100%;` && |\n|  &&
                                      `     color:#000000;` && |\n|  &&
                                      `     font-weight:400;` && |\n|  &&
                                      `     white-space:nowrap;` && |\n|  &&
                                      `}` && |\n|  &&
                                      `tr.header {` && |\n|  &&
                                      `    background-color:#D3D3D3;` && |\n|  &&
                                      `}` && |\n|  &&
                                      `tr.body {` && |\n|  &&
                                      `    background-color:#EFEFEF;` && |\n|  &&
                                      `}` && |\n|  &&
                                      `</html:style>`.

    "generate HTML output, e.g. from a database query:
    "SELECT * FROM scarr INTO TABLE @DATA(carriers).
    "DATA(lv_html) = cl_demo_output=>get( carriers ).

    DATA(lv_html) = `<h2 title="I'm a header">The title Attribute</h2>` && |\n|  &&
                    `<p title="I'm a tooltip">Mouse over this paragraph, to display the title attribute as a tooltip.</p>`.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->shell(
           )->page(
           )->button( text = `test`
          )->_cc_plain_xml( lv_style
          )->html( lv_html ).

    client->view_display( view->stringify( ) ).

ENDMETHOD.
```
