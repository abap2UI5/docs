---
outline: [2, 4]
---
# Focus

abap2UI5 offers the custom control `_z2ui5( )->focus( )` to set the input focus from the backend. Pass a bound variable holding the target control's `id` — the framework moves the cursor to that field after the next `view_model_update( )`.

This is useful for guided data entry, barcode scanning, or any flow where the next field to focus depends on backend logic.

#### Basic Usage

Bind a `focus_id` variable to the focus control. Update its value in any event handler and call `view_model_update( )` — the browser focuses the matching input automatically:

```abap
CLASS z2ui5_cl_sample_focus DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA one      TYPE string.
    DATA two      TYPE string.
    DATA focus_id TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_focus IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
      focus_id = `id1`.

      DATA(page) = z2ui5_cl_xml_view=>factory( )->page( ).
      page->simple_form(
         )->content( ns = `form`
         )->label( `One`
         )->input(
              id     = `id1`
              value  = client->_bind_edit( one )
              submit = client->_event( |one_enter| )
         )->label( `Two`
         )->input(
              id     = `id2`
              value  = client->_bind_edit( two )
              submit = client->_event( |two_enter| ) ).

      page->_z2ui5( )->focus( client->_bind( focus_id ) ).
      client->view_display( page->stringify( ) ).

    ENDIF.

    CASE client->get( )-event.
      WHEN `one_enter`.
        focus_id = `id2`.
        client->view_model_update( ).
      WHEN `two_enter`.
        focus_id = `id1`.
        client->view_model_update( ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

After the user presses Enter in `id1`, the backend sets `focus_id = id2` and the frontend moves the cursor to the second input. The same pattern works for any chain of fields.

#### Barcode Scanning

Most barcode scanner devices emulate a keyboard. Combine the focus control with input fields to capture scans into the right field automatically — see [Barcode Scanning](../device_capabilities/barcode_scanning.md) for a full walkthrough.
