---
outline: [2, 4]
---
# Model

there are three different ways to exchange data with the frontend.

#### One Way Binding

If you just need to display data to the frontend, and now changes must be made, you just use one way binding with the method `client->_bind` and the framework sends the data to the frontend and connects it with the view:
```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA name TYPE string.

ENDCLASS.

CLASS z2ui5_cl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
      )->page( 'abap2UI5 - Hello World'
          )->text( text = `My Text`
          )->text( client->_bind( name )
      )->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```
Same works for tables, trees are other deep data structures. Check out the samples repository and take a look to the table or tree samples.

#### Two Way Binding
If the user should be able top change the data, you habe to make sure that the data will also be updated in the ABAP backend. Therefor use two way binding and use the mehtod `client->_bind_edit`. After an event the framework will update the value in you abap class.

```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA name TYPE string.

ENDCLASS.

CLASS z2ui5_cl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE client->get( )-event.
      WHEN 'POST'.
        client->message_box_display( |Your name is { name }.| ).
        RETURN.
    ENDCASE.

    client->view_display( z2ui5_cl_xml_view=>factory(
      )->page( 'abap2UI5 - Hello World'
          )->text( text = `Enter your name`
          )->input( client->_bind_edit( name )
          )->button( text = 'post' press = client->_event( 'POST' )
      )->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

::: tip **Data in Public Attributes**
Be aware that in both cases the binded data is public attribute in your class, because the framework needs to access it from outside. A bit like in the old pbo/pai screen days where the needed t be saved in global attributes too.
:::

#### Local Binding
If you only have access to the data locally but still want to bind it, you have the chance to use bind local. This is especcially helpful for value helps when no changes at the frontend are made and dont need to chenge the data in the app logic. the framework will imeediatley copy the data in the bind method and then sned it to the frontend:

```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    
ENDCLASS.

CLASS z2ui5_cl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    DATA(lv_name) TYPE string.
    
    client->view_display( z2ui5_cl_xml_view=>factory(
      )->page( 'abap2UI5 - Hello World'
          )->text( text = `My Text`
          )->input( client->_bind_local( lv_name )
      )->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

Check out the `Z2UI5_CL_DEMO_APP_002` to see the value help use case for local binding in action.