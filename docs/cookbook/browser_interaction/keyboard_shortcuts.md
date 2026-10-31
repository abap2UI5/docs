---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_471
description: Bind a key combination to a named backend event with the keyboard_shortcut frontend action - spelling, aliases, scoped registrations for a popup, and how to remove one.
---
# Keyboard Shortcuts

The `keyboard_shortcut` frontend event binds a key combination to a **named backend event** — the declarative equivalent of a `sap.ui.core.CommandExecution` shortcut, which needs a controller method and therefore has no place in a controller-less abap2UI5 app. The binding is pure data: the key combination and the name of the backend event it fires.

## Registering a Shortcut

`t_arg` is positional — the key combination and the backend event name:

```abap
CLASS zcl_sample_shortcut DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_shortcut IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      client->follow_up_action( val   = client->cs_event-keyboard_shortcut
                                t_arg = VALUE #( ( `Ctrl+S` )
                                                 ( `SAVE` ) ) ).

      client->follow_up_action( val   = client->cs_event-keyboard_shortcut
                                t_arg = VALUE #( ( `Ctrl+D` )
                                                 ( `DELETE` ) ) ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->a( n = `title` v = `Keyboard Shortcuts`

                  )->tag( `Text`
                      )->a( n = `text` v = `Press Ctrl+S or Ctrl+D` ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `SAVE` ).
      client->message_toast_display( `Saved via Ctrl+S` ).

    ELSEIF client->check_on_event( `DELETE` ).
      client->message_toast_display( `Deleted via Ctrl+D` ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

Pressing the combination fires the backend event exactly like a button press would — and the browser default for the combination is suppressed.

## Behavior

- **Spelling** follows the UI5 convention: `Ctrl+S`, `Ctrl+Shift+D`, `F2`. Common aliases are normalized (`Cmd`/`Command` → `Meta`, `Control` → `Ctrl`, `Esc` → `Escape`, `Return` → `Enter`, …), so registration and keypress match for any spelling. The `+` key itself is spelled with the separator as the key: `Ctrl++`.
- **A character the layout only produces with Shift** — `+` and `?` on a US keyboard, the digits on AZERTY — arrives with Shift held, so a registration without `Shift` matches it as well; `Ctrl+Shift+S` and `Ctrl+S` stay apart, because a letter's case is the Shift.
- **The focused field is committed first.** A shortcut pressed while typing in an input comes before the field's `change`, so the frontend commits the field the way leaving it would, and the backend sees the typed value.
- **One registration per combination and scope.** Registering the same combination again rebinds it — no need to unregister first.
- **Unregister** a combination by sending it with an empty event name:

```abap
client->follow_up_action( val   = client->cs_event-keyboard_shortcut
                          t_arg = VALUE #( ( `Ctrl+S` )
                                           ( `` ) ) ).
```

- **Lifetime:** the registry lives in the frontend and survives every roundtrip until the app is left. Navigating to another app starts from an empty set.

The same registration also works without a backend roundtrip: write the `follow_up_action( )` call where its result is consumed, as a view attribute, with the identical `t_arg` — see [the two positions](/cookbook/event_navigation/frontend#the-two-positions).

## Scoped Shortcuts

An optional third argument scopes the registration, the way a `sap.ui.core.CommandExecution` in a popover's `dependents` shadows the page-level one for the same command while the popover is open. The scope is either a view slot — `POPUP`, `POPOVER`, `NEST`, `NEST2` or `MAIN`, the values of `cs_view` — or the id of a control declared in the view that can be open or closed, such as a `Dialog` in `dependents` opened with `control_by_id`. A control scope wins over a slot scope, the innermost open slot wins over an outer one, and the registration without a scope applies when no scope is open:

```abap
" page level: Ctrl+S saves the record
client->follow_up_action( val   = client->cs_event-keyboard_shortcut
                          t_arg = VALUE #( ( `Ctrl+S` ) ( `SAVE` ) ) ).

" while the popup is open: the same key saves the popup instead
client->follow_up_action( val   = client->cs_event-keyboard_shortcut
                          t_arg = VALUE #( ( `Ctrl+S` ) ( `POPUP_SAVE` ) ( client->cs_view-popup ) ) ).
```

A slot-scoped shortcut is dispatched through that slot's own controller, so what the user typed into the dialog travels with the event, as the dialog's own Save button would send it. An empty event name removes the registration of that scope only — `( `Ctrl+S` ) ( `` ) ( client->cs_view-popup )` leaves the page-level `SAVE` in place.

For controlling the *soft keyboard* on mobile devices, see [Soft Keyboard](/cookbook/browser_interaction/soft_keyboard).

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Keyboard Shortcuts, Ctrl+S (A) | [`Z2UI5_CL_SMP_APP_471`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_471.clas.abap) |

<!-- samples:end -->
