# Share, Bookmark

The app state feature leads to various additional use cases.

### Share
You can easily integrate a share button, which copies the actiual state into the clpiboard for sharing with your collegues. Check out the followig snippet:
 ```abap
CLASS z2ui5_cl_sample_share DEFINITION PUBLIC FINAL CREATE PUBLIC.
 
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
        client->follow_up_action( client->_event_client( z2ui5_if_client=>cs_event-CLIPBOARD_APP_STATE ) ).
        client->message_toast_display( `clipboard copied` ).

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
 ```

 ### Bookmark
 You can also use these URLs for bookmarking, but keep in mind that the app state only saved for a certain amount of time on the server. The default configuration is 4 hours. See [here.](https://github.com/abap2UI5/abap2UI5/blob/main/src/01/01/z2ui5_cl_core_srv_draft.clas.abap#L46)
