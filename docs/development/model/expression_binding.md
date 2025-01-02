# Expression Binding

Expression Binding in UI5 allows you to dynamically compute values directly in XML views using JavaScript-like expressions. It simplifies coding, especially in the ABAP2UI5 context, by enabling calculations, logical conditions, and string operations directly at the frontend—eliminating the need for additional server roundtrips.

#### Calculate the Maximum Value at the Frontend

```abap
CLASS z2ui5_cl_demo_app_max_val DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA input31 TYPE i.
    DATA input32 TYPE i.

ENDCLASS.

CLASS z2ui5_cl_demo_app_max_val IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->shell( )->page(
      )->label( 'max value of the first two inputs'
                )->input( '{ type : "sap.ui.model.type.Integer",' &&
                          '  path:"' && client->_bind( val  = input31
                                                       path = abap_true ) && '" }'
                )->input( '{ type : "sap.ui.model.type.Integer",' && |\n| &&
                          '  path:"' && client->_bind( val  = input32
                                                       path = abap_true ) && '" }'
                )->input(
                    value   = '{= Math.max($' && client->_bind( input31 ) &&', $' && client->_bind( input32 ) && ') }'
                    enabled = abap_false ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

#### Conditionally Set Input Field Editability
```abap
CLASS z2ui5_cl_demo_editable DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA quantity TYPE i.
    DATA product TYPE string.

ENDCLASS.

CLASS z2ui5_cl_demo_editable IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->shell( )->page(
      )->label( 'only enabled when the quantity equals 500'
                )->input( '{ type : "sap.ui.model.type.Integer",' &&
                          '  path:"' && client->_bind( val  = quantity
                                                       path = abap_true ) && `"  }`
                )->input(
                    value   = product
                    enabled = '{= 500===$' && client->_bind( quantity ) && ' }' ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

Explore all the possibilities in the sample class `Z2UI5_CL_DEMO_APP_027` or refer to the UI5 documentation [here.](https://sapui5.hana.ondemand.com/sdk/#/topic/daf6852a04b44d118963968a1239d2c0)