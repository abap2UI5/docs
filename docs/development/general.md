# General

abap2UI5 offers great flexibility in app development. Most sample applications follow a process similar to the sequence outlined below. You can use it as a starting point, but feel free to create your own sequence or build a wrapper on top of abap2UI5 for more customized behavior:

```abap
CLASS z2ui5_cl_app DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

ENDCLASS.

CLASS z2ui5_cl_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    TRY.

        "first app start
        IF client->check_on_init( ).
          "init values
          "display view
          RETURN.
        ENDIF.

        "callback after navigation
        IF client->check_on_navigated( ).
          DATA(lo_app_prev) = client->get_app_prev( ).
          "read attributes of previous app
          "do something
          RETURN.
        ENDIF.

        "handle events after frontend
        CASE client->get( )-event.
          WHEN |OK|.
            DATA(lt_arg) = client->get_event_arg( ).
            "event handling
          WHEN |CANCEL|.
            "...
        ENDCASE.

        "error handling
      CATCH cx_root INTO DATA(lx).
        client->message_box_display( lx ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
````
Refer to the specific sections of this development guide for more details on the implementation process.
