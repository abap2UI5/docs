---
outline: [2, 4]
description: Show the HTML that cl_demo_output=>get( ) produces inside an abap2UI5 view with a core:HTML control and the stylesheet its class names need.
---
# Demo Output

Familiar with `CL_DEMO_OUTPUT` from classic ABAP? You can show its HTML output inside an abap2UI5 app too. This is handy for quick data visualization or when porting existing demos.

The approach: build the HTML with `cl_demo_output=>get( )` and render it, together with a stylesheet, as the content of one `core:HTML` control. The CSS block is needed because `CL_DEMO_OUTPUT` produces HTML with specific class names (e.g., `heading1`, `header`, `body`) that need matching style definitions to render correctly.

::: tip
This technique fits prototyping or porting existing demo programs. For production apps, build proper UI5 views instead.
:::

<!-- playground: no Run button — cl_demo_output is an on-premise class the browser runtime does not implement -->
```abap
CLASS zcl_sample_demo_output DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_demo_output IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

    DATA(lv_style) = `<style type="text/css">body \{` && |\n|  &&
                                      `     font-family: Arial;` && |\n|  &&
                                      `     font-size: 90%;` && |\n|  &&
                                      `\}` && |\n|  &&
                                      `table \{` && |\n|  &&
                                      `     font-family: Arial;` && |\n|  &&
                                      `     font-size: 90%;` && |\n|  &&
                                      `\}` && |\n|  &&
                                      `caption \{` && |\n|  &&
                                      `     font-family: Arial;` && |\n|  &&
                                      `     font-size: 90%;` && |\n|  &&
                                      `     font-weight:bold;` && |\n|  &&
                                      `     text-align:left;` && |\n|  &&
                                      `\}` && |\n|  &&
                                      `span.heading1 \{` && |\n|  &&
                                      `    font-size: 150%;` && |\n|  &&
                                      `     color:#000080;` && |\n|  &&
                                      `     font-weight:bold;` && |\n|  &&
                                      `\}` && |\n|  &&
                                      `span.heading2 \{` && |\n|  &&
                                      `    font-size: 135%;` && |\n|  &&
                                      `     color:#000080;` && |\n|  &&
                                      `     font-weight:bold;` && |\n|  &&
                                      `\}` && |\n|  &&
                                      `span.heading3 \{` && |\n|  &&
                                      `    font-size: 120%;` && |\n|  &&
                                      `     color:#000080;` && |\n|  &&
                                      `     font-weight:bold;` && |\n|  &&
                                      `\}` && |\n|  &&
                                      `span.heading4 \{` && |\n|  &&
                                      `    font-size: 105%;` && |\n|  &&
                                      `     color:#000080;` && |\n|  &&
                                      `     font-weight:bold;` && |\n|  &&
                                      `\}` && |\n|  &&
                                      `span.normal \{` && |\n|  &&
                                      `    font-size: 100%;` && |\n|  &&
                                      `     color:#000000;` && |\n|  &&
                                      `     font-weight:normal;` && |\n|  &&
                                      `\}` && |\n|  &&
                                      `span.nonprop \{` && |\n|  &&
                                      `    font-family: Courier New;` && |\n|  &&
                                      `     font-size: 100%;` && |\n|  &&
                                      `     color:#000000;` && |\n|  &&
                                      `     font-weight:400;` && |\n|  &&
                                      `\}` && |\n|  &&
                                      `span.nowrap \{` && |\n|  &&
                                      `    white-space:nowrap;` && |\n|  &&
                                      `\}` && |\n|  &&
                                      `span.nprpnwrp \{` && |\n|  &&
                                      `    font-family: Courier New;` && |\n|  &&
                                      `     font-size: 100%;` && |\n|  &&
                                      `     color:#000000;` && |\n|  &&
                                      `     font-weight:400;` && |\n|  &&
                                      `     white-space:nowrap;` && |\n|  &&
                                      `\}` && |\n|  &&
                                      `tr.header \{` && |\n|  &&
                                      `    background-color:#D3D3D3;` && |\n|  &&
                                      `\}` && |\n|  &&
                                      `tr.body \{` && |\n|  &&
                                      `    background-color:#EFEFEF;` && |\n|  &&
                                      `\}` && |\n|  &&
                                      `</style>`.

    " any data cl_demo_output takes - here the rows of a database table
    SELECT * FROM scarr INTO TABLE @DATA(carriers).
    DATA(lv_html) = cl_demo_output=>get( carriers ).

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`      v = `sap.m`
            )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
            )->a( n = `xmlns:core` v = `sap.ui.core`

            )->ele( `Shell`
                )->ele( `Page`
                    )->tag( n = `HTML` ns = `core`
                        )->a( n = `content` v = lv_style && lv_html ).

    client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```
