---
outline: [2, 4]
---
# Model

In abap2UI5, there are two ways to exchange data with the frontend.

#### One-Way Binding
Use one-way binding when you need to display data on the frontend without allowing changes. The `client->_bind` method sends data to the frontend and connects it to the view:

```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA name TYPE string.

ENDCLASS.

CLASS z2ui5_cl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
      )->page( `abap2UI5 - Hello World`
          )->text( `My Text`
          )->text( client->_bind( name )
      )->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```
This method works with tables, trees, and other complex data structures. For more details, refer to the samples repository and explore the table or tree samples.

#### Two-Way Binding
If the user should be able to modify data, enable two-way binding to update the data in the ABAP backend. Use the `client->_bind_edit` method so that after an event, the framework will synchronize the data with your ABAP class:

```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA name TYPE string.

ENDCLASS.

CLASS z2ui5_cl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
      )->page( `abap2UI5 - Hello World`
          )->text( `Enter your name`
          )->input( client->_bind_edit( name )
          )->button( text = `post` press = client->_event( `POST` )
      )->stringify( ) ).

    CASE client->get( )-event.
      WHEN `POST`.
        client->message_box_display( |Your name is { name }.| ).
        RETURN.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

::: tip **Data in Public Attributes**
When using One-Way or Two-Way binding, ensure your data is stored in the public attributes of your class. This allows the framework to access it from outside. This is similar to the PBO/PAI logic, where data had to be stored in global variables.
:::
