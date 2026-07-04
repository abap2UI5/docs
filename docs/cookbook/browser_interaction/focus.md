---
outline: [2, 4]
---
# Focus

Read which control currently holds the focus from the backend, or move the focus from the backend to a specific field — both work without any custom control.

#### Read the Current Focus

`client->get( )-s_focus` tells you which control currently holds the focus and where the caret sits inside it. Useful when an action depends on the field the user was just editing.

```abap
DATA(focus) = client->get( )-s_focus.

DATA(id)              = focus-id.                " id of the focused control
DATA(selection_start) = focus-selection_start.   " caret start, in chars
DATA(selection_end)   = focus-selection_end.     " caret end, in chars
```

#### Set the Focus

Set the input focus from the backend with the `set_focus` frontend event. Pass the target control's `id` as the first argument — the framework moves the cursor to that field after the next roundtrip.

This is useful for guided data entry, barcode scanning, or any flow where the next field to focus depends on backend logic.

#### Basic Usage

After processing an event, call `client->action->gen( )` with `cs_event-set_focus` and the id of the input to focus next. (The value bindings on the inputs are omitted here to keep the focus logic clear — see [Barcode Scanning](/cookbook/device_capabilities/barcode_scanning) for the same form with bound inputs.)

```abap
METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
      DATA(page) = z2ui5_cl_xml_view=>factory( )->page( ).
      page->simple_form(
         )->content( ns = `form`
         )->label( `One`
         )->input(
              id     = `id1`
              submit = client->_event( `ONE_ENTER` )
         )->label( `Two`
         )->input(
              id     = `id2`
              submit = client->_event( `TWO_ENTER` ) ).
      client->view_display( page->stringify( ) ).

    ELSEIF client->check_on_event( `ONE_ENTER` ).
        client->action->gen( val   = client->cs_event-set_focus
                        t_arg = VALUE #( ( `id2` ) ) ).

    ELSEIF client->check_on_event( `TWO_ENTER` ).
        client->action->gen( val   = client->cs_event-set_focus
                        t_arg = VALUE #( ( `id1` ) ) ).

    ENDIF.

ENDMETHOD.
```

After the user presses Enter in `id1`, the backend fires `set_focus` for `id2` and the cursor moves to the second input. The same pattern works for any chain of fields.

#### Selection Range

To position the caret inside the field or pre-select a range, pass the start and end offsets as additional arguments:

```abap
client->action->gen( val   = client->cs_event-set_focus
                t_arg = VALUE #( ( `id1` ) ( `0` ) ( `5` ) ) ).
```

#### Barcode Scanning

Most barcode scanner devices emulate a keyboard. Combine `set_focus` with input fields to capture scans into the right field automatically — see [Barcode Scanning](../device_capabilities/barcode_scanning.md) for a full walkthrough.
