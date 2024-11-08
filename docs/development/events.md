---
outline: [2, 4]
---
# Events

### Frontend

#### Basic

If an UI5 property is an event, just import the result of the method `client->_event` to trigger events in the backend. Check out the event with the value `client->get( )-event`:

```abap
METHOD z2ui5_if_app~main.
 
    client->view_display( z2ui5_cl_xml_view=>factory(
        )->button(
            text  = 'post'
            press = client->_event( 'BUTTON_POST' )
        )->stringify( ) ).

    CASE client->get( )-event.
        WHEN 'BUTTON_POST'.
            client->message_box_display( |Your name is { name }| ).
     ENDCASE.
 
ENDMETHOD.
```

Sometimes the backend needs more information about the süpecific event, you can use the parameter `$event`, `$source` and `$params` to send further information into the backend. Therefore use the parameter `t_arg`. Check the documentation [here.](https://openui5.hana.ondemand.com/#/topic/b0fb4de7364f4bcbb053a99aa645affe). Check out sampple `Z2UI5_CL_DEMO_APP_167`:

#### Source Properties
Send properties of the event source control:
```abap
METHOD z2ui5_if_app~main.
 
    client->view_display( z2ui5_cl_xml_view=>factory(
        )->button( text = `post` press = client->_event( val = `BUTTON_POST` 
            t_arg = VALUE #( ( `${$source>/text}` ) ) ) 
        )->stringify( ) ).
 
    CASE client->get( )-event.
      WHEN 'BUTTON_POST'.
          client->message_box_display( |The button text is { client->get_event_arg( ) }| ).
    ENDCASE.
 
ENDMETHOD.
```

#### Event Parameters
Read Event Parameters:
```abap
METHOD z2ui5_if_app~main.
 
    client->view_display( z2ui5_cl_xml_view=>factory(
        )->button( text = `post` press = client->_event( val = `BUTTON_POST` 
            t_arg = VALUE #( ( `${$source>/text}` ) ) ) 
        )->stringify( ) ).
 
    CASE client->get( )-event.
        WHEN 'BUTTON_POST'.
            client->message_box_display( |The button text is { client->get_event_arg( ) }| ).
        WHEN OTHERS.
    ENDCASE.
 
ENDMETHOD.
```

#### Event Properties
Read Event Parameters:
```abap
METHOD z2ui5_if_app~main.
 
    client->view_display( z2ui5_cl_xml_view=>factory(
        )->button( text = `post` press = client->_event( val = `BUTTON_POST` 
            t_arg = VALUE #( ( `${$source>/text}` ) ) ) 
        )->stringify( ) ).
 
    CASE client->get( )-event.
        WHEN 'BUTTON_POST'.
            client->message_box_display( |The button text is { client->get_event_arg( ) }| ).
      WHEN OTHERS.
    ENDCASE.
 
ENDMETHOD.
```

#### Model Properties
Read Event Parameters:
```abap
METHOD z2ui5_if_app~main.
 
    client->view_display( z2ui5_cl_xml_view=>factory(
        )->button( text = `post` press = client->_event( 
            val   = `BUTTON_POST` 
            t_arg = VALUE #( ( `${$source>/text}` ) ) ) 
        )->stringify( ) ).
 
    CASE client->get( )-event.
        WHEN 'BUTTON_POST'.
            client->message_box_display( |The button text is { client->get_event_arg( ) }| ).
    ENDCASE.
 
ENDMETHOD.
```

### Frontend

#### Event Frontend

The following frontend events re availibkle:
```abap
  CONSTANTS:
    BEGIN OF cs_event,
      popup_close               TYPE string VALUE `POPUP_CLOSE`,
      open_new_tab              TYPE string VALUE `OPEN_NEW_TAB`,
      popover_close             TYPE string VALUE `POPOVER_CLOSE`,
      location_reload           TYPE string VALUE `LOCATION_RELOAD`,
      nav_container_to          TYPE string VALUE `NAV_CONTAINER_TO`,
      nest_nav_container_to     TYPE string VALUE `NEST_NAV_CONTAINER_TO`,
      nest2_nav_container_to    TYPE string VALUE `NEST2_NAV_CONTAINER_TO`,
      cross_app_nav_to_ext      TYPE string VALUE `CROSS_APP_NAV_TO_EXT`,
      cross_app_nav_to_prev_app TYPE string VALUE `CROSS_APP_NAV_TO_PREV_APP`,
      popup_nav_container_to    TYPE string VALUE `POPUP_NAV_CONTAINER_TO`,
      download_b64_file         TYPE string VALUE `DOWNLOAD_B64_FILE`,
      set_size_limit            TYPE string VALUE `SET_SIZE_LIMIT`,
    END OF cs_event.
```

```abap
METHOD z2ui5_if_app~main.
 
    client->view_display( z2ui5_cl_xml_view=>factory(
        )->button(
            text = `post`
            press = client->_event( client->_event_client( 
                 val   = client->cs_event-open_new_tab 
                 t_arg = VALUE #( ( `https://github.com/abap2UI5` ) ) ) ) 
        )->stringify( ) ).
 
ENDMETHOD.
```

#### Follow Up Action
```abap
METHOD z2ui5_if_app~main.

    client->follow_up_action( client->_event_client( 
        val   = client->cs_event-open_new_tab 
        t_arg = VALUE #( ( `https://github.com/abap2UI5` ) ) ) ).

ENDMETHOD.
```
Check out sample `Z2UI5_CL_DEMO_APP_180` for a working snippet.
