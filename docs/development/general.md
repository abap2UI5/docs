# Controller

abap2UI5 offers great flexibility in app development. Most sample applications follow a process similar to the sequence outlined below. You can use it as a starting point, but feel free to create your own sequence or build a wrapper on top of abap2UI5 for more customized behavior:

```abap
CLASS z2ui5_cl_app DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

ENDCLASS.

CLASS z2ui5_cl_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    CASE abap_true.

      "first start, init your app here...
      WHEN client->check_on_init( ).
        "...

      "after coming back from another app
      WHEN client->check_on_navigated( ).
        "read the previous app
        DATA(lo_app_prev) = client->get_app_prev( ).
        "...

      "after user event
      WHEN client->check_on_event( ).
        "read event information
        DATA(lt_arg) = client->get_event_arg( ).
        "...

        "handle event
        CASE abap_true.
          WHEN client->check_on_event( `OK` ).
          "...
          WHEN client->check_on_event( `CANCEL` ).
          "...
        ENDCASE.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
```
Refer to the specific sections of this development guide for more details on the implementation process.
