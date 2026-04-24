---
outline: [2, 4]
---
# Model

In abap2UI5, there are two ways to exchange data between your ABAP code and the UI5 frontend.

#### One-Way Binding
Use one-way binding to display data on the frontend without allowing edits. The `client->_bind` method sends data to the frontend and binds it to the view:

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
This method works with tables, trees, and other complex data structures. For details, browse the table and tree samples in the samples repository.

#### Two-Way Binding
When users need to edit data, use two-way binding to update it in the ABAP backend. Call the `client->_bind_edit` method — after an event, the framework syncs the data with your ABAP class:

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
When using one-way or two-way binding, store your data in the public attributes of your class so the framework can access it externally. This is similar to the PAI/PBO logic, where data had to live in global variables.
:::
