---
outline: [2, 4]
---

# Messages

#### Message Toast

Messages only displayed for a short period of time eg. sucesses can be displayed via the client method:

```abap
CLASS zcl_my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    client->message_toast_display( `this is my message` ).

  ENDMETHOD.
ENDCLASS.
```

#### Message Box

Messages wihch need to be closed manually can be displayed via the message box:

```abap
CLASS zcl_my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    client->message_box_display( `this is my message` ).

  ENDMETHOD.
ENDCLASS.
```

Or use an error

```abap
CLASS zcl_my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
     client->message_box_display( text = 'Select a team in the "Development" area.' && cl_abap_char_utilities=>cr_lf &&
                                            '"Marketing" isn’t assigned to this area.' type = 'error' ).

  ENDMETHOD.
ENDCLASS.
```

#### Popup T100/BAPIRET

```abap
CLASS z2ui5_cl_demo_app_154 IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

     DATA(lo_app) = z2ui5_cl_pop_messages=>factory( VALUE #(
            ( message = 'An empty Report field causes an empty XML Message to be sent' type = 'E' id = 'MSG1' number = '001' )
            ( message = 'Check was executed for wrong Scenario' type = 'E' id = 'MSG1' number = '002' )
            ( message = 'Request was handled without errors' type = 'S' id = 'MSG1' number = '003' )
            ( message = 'product activated' type = 'S' id = 'MSG4' number = '375' )
            ( message = 'check the input values' type = 'W' id = 'MSG2' number = '375' )
            ( message = 'product already in use' type = 'I' id = 'MSG2' number = '375' )
       ) ).

  client->nav_app_call( lo_app ).

  ENDMETHOD.
ENDCLASS.
```

if you need interaction, you can use built-in popups. The commen messages in format T100 or bairet, you can use the following popup to display these messages:

PRs welcome

#### Popup To Inform

Popup

#### Popup To Decide
