---
outline: [2, 4]
---
# Form

An input mask with labels, fields and a save button — the bread-and-butter screen of every business app. UI5 builds it with `sap.ui.layout.form.SimpleForm`: a responsive grid that arranges label/field pairs and wraps automatically on small screens.

The pattern is always the same — a `simple_form` with `editable = abap_true`, then `content( 'form' )` for the form body, and finally a sequence of `label( … )` + an input control. What follows builds that up step by step.

#### Step 1 — Label and input

The minimum: one label, one input, bound to a string field. Set `editable` so the user can type into it:

```abap
client->view_display( z2ui5_cl_xml_view=>factory(
    )->page( `Customer`
        )->simple_form( editable = abap_true
            )->content( `form`
                )->label( `Name` )->input( client->_bind_edit( ms_customer-name )
    )->stringify( ) ).
```

#### Step 2 — More field types

Real forms use more than text inputs. Mix in `select` for dropdowns, `check_box` for booleans, `date_picker` for dates and `text_area` for longer text. Bind each with `_bind_edit` against the matching ABAP field:

```abap
DATA(form) = z2ui5_cl_xml_view=>factory(
    )->page( `Customer`
        )->simple_form( editable = abap_true
            )->content( `form` ).

form->label( `Name`     )->input( client->_bind_edit( ms_customer-name ) ).
form->label( `Country`  )->select( selectedkey = client->_bind_edit( ms_customer-country )
    )->items( )->core_item( key = `DE` text = `Germany`
                  )->core_item( key = `FR` text = `France`
                  )->core_item( key = `IT` text = `Italy` ).
form->label( `Active`   )->check_box( selected = client->_bind_edit( ms_customer-active ) ).
form->label( `Birthday` )->date_picker( value = client->_bind_edit( ms_customer-birthday ) ).
form->label( `Notes`    )->text_area(   value = client->_bind_edit( ms_customer-notes ) rows = `4` ).
```

Storing the result as `DATA(form)` and adding rows in separate statements is easier to read than one long chain — pick whichever style fits your code.

#### Step 3 — Required and validation

Mark a field required by setting `required = abap_true` on the `label`. UI5 then renders the red asterisk. The actual validation still runs in ABAP — check the values on save and report problems via the message API:

```abap
form->label( `Name` required = abap_true )->input( client->_bind_edit( ms_customer-name ) ).

" ...later in the save handler:
WHEN client->check_on_event( `SAVE` ).
  IF ms_customer-name IS INITIAL.
    client->message_box_display( `Name is required.` ).
    RETURN.
  ENDIF.
  " ...persist ms_customer here...
  client->message_toast_display( `saved` ).
```

For richer feedback — error states on individual inputs, multi-message popups — see [Messages, Errors](../messages/messages.md).

#### Full Example

A complete editable customer form with a save handler and basic validation:

```abap
CLASS z2ui5_cl_sample_form DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    TYPES:
      BEGIN OF ty_customer,
        name     TYPE string,
        country  TYPE string,
        active   TYPE abap_bool,
        birthday TYPE d,
        notes    TYPE string,
      END OF ty_customer.
    DATA ms_customer TYPE ty_customer.

ENDCLASS.

CLASS z2ui5_cl_sample_form IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).
        ms_customer = VALUE #( country = `DE` active = abap_true ).

        DATA(form) = z2ui5_cl_xml_view=>factory(
            )->page( `Customer`
                )->simple_form( editable = abap_true
                    )->content( `form` ).

        form->title( `General` ).
        form->label( `Name`     required = abap_true )->input( client->_bind_edit( ms_customer-name ) ).
        form->label( `Country`  )->select( selectedkey = client->_bind_edit( ms_customer-country )
            )->items( )->core_item( key = `DE` text = `Germany`
                          )->core_item( key = `FR` text = `France`
                          )->core_item( key = `IT` text = `Italy` ).
        form->label( `Active`   )->check_box( selected = client->_bind_edit( ms_customer-active ) ).
        form->label( `Birthday` )->date_picker( value = client->_bind_edit( ms_customer-birthday ) ).

        form->title( `Notes` ).
        form->label( `Comment` )->text_area( value = client->_bind_edit( ms_customer-notes ) rows = `4` ).

        form->toolbar( )->toolbar_spacer(
            )->button( text = `Save` type = `Emphasized` press = client->_event( `SAVE` )
            )->button( text = `Cancel` press = client->_event( `CANCEL` ) ).

        client->view_display( form->stringify( ) ).

      WHEN client->check_on_event( `SAVE` ).
        IF ms_customer-name IS INITIAL.
          client->message_box_display( `Name is required.` ).
          RETURN.
        ENDIF.
        " ...persist ms_customer here...
        client->message_toast_display( `saved` ).

      WHEN client->check_on_event( `CANCEL` ).
        CLEAR ms_customer.
        client->view_model_update( ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

::: tip
For a responsive layout that places labels next to fields on desktop and stacks them on mobile, pass `layout = 'ResponsiveGridLayout'` to `simple_form` and tune `labelspanxl`, `columnsxl` etc. — see the `Z2UI5_CL_APP_STARTUP` source for a fine-tuned reference.
:::
