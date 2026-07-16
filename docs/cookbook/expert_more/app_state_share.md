---
outline: [2, 4]
---
# App State, Share

abap2UI5 saves the current app state so you can return to it later — like how standard UI5 apps manage state. This opens up several useful options, like sharing and bookmarking the current state of your app.

### Usage
Each state persists as a draft with a unique ID. Calling `client->set_app_state_active` appends this ID as a hash fragment to the URL. The resulting URL is shareable — copy it and open it in another browser window (or send it to a colleague) to restore the same app state. The hash value (`z2ui5-xapp-state=...`) is a server-side key that points to the saved draft. Drafts expire after a configurable time (default: 4 hours, adjustable via [User Exits](/advanced/extensibility/user_exits)).

An example URL: <br>
`.../sap/bc/z2ui5?sap-client=001&app_start=z2ui5_cl_demo_app_000#/z2ui5-xapp-state=024251849E5A1EDFB1DAE2C97C8CE8C2`

### Sample Code
An implementation of the app state feature:
```abap
CLASS z2ui5_cl_sample_app_state DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_quantity TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_app_state IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).
      DATA(view) = z2ui5_cl_xml_view=>factory( ).
      client->view_display(
        view->label( `quantity`
            )->input( client->_bind_edit( mv_quantity )
            )->button(
                text  = `post with state`
                press = client->_event( `BUTTON_POST` )
              )->stringify( ) ).
    ENDIF.

    CASE client->get( )-event.
      WHEN `BUTTON_POST`.
        client->message_toast_display( `data updated and url adjusted` ).
        client->set_app_state_active( ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

For a complete implementation, see sample `Z2UI5_CL_DEMO_APP_321`.

### Share
Add a share button that copies the current state to the clipboard to share with colleagues:
```abap
CLASS z2ui5_cl_sample_share DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_quantity TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_share IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_navigated( ).

        DATA(view) = z2ui5_cl_xml_view=>factory( )->shell( )->page(
          )->label( `quantity`
          )->input( client->_bind_edit( mv_quantity )
          )->button( text  = `share` press = client->_event( `BUTTON_POST` ) ).
        client->view_display( view->stringify( ) ).

      WHEN client->check_on_event( `BUTTON_POST` ).

        client->follow_up_action( z2ui5_if_client=>cs_event-clipboard_app_state ).
        client->message_toast_display( `clipboard copied` ).

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
```

### Bookmark
You can also use these URLs for bookmarking, but note that the server keeps the app state only for a limited time. The default is 4 hours. See the [draft service](https://github.com/abap2UI5/abap2UI5/blob/main/src/01/01/z2ui5_cl_core_srv_draft.clas.abap#L46) source.
