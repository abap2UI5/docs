# General

### Basic Structure


```abap
CLASS z2ui5_cl_app DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

ENDCLASS.

CLASS z2ui5_cl_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    TRY.

        "first app start,
        IF client->check_on_init( ).
          "init values here..
          RETURN.
        ENDIF.


        "callback after previous app.
        IF client->check_on_navigated( ).
          DATA(lo_app_prev) = client->get_app_prev( ).
          "read attributes of previous app here...
          RETURN.
        ENDIF.


        "handle events..
        CASE client->get( )-event.
          WHEN 'OK'.
            data(lt_arg) = client->get_event_arg( ).
            "...
          WHEN 'CANCEL'.
            "...
        ENDCASE.


        "error handling here..
      CATCH cx_root INTO DATA(lx).
        client->message_box_display( lx ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
````

###