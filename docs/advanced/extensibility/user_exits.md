# User Exits

abap2UI5 contains predefined user exits which can be used to modify the standard behavior. The user exits are exposed by the interface [`Z2UI5_IF_EXIT`](https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_if_exit.intf.abap). To use them in your system you have to create a new class which implements the interface and its methods. They're called dynamically by abap2UI5 class [`Z2UI5_CL_EXIT`](https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_cl_exit.clas.abap). You should **not** include your class into abap2UI5 packages but in any other custom package.

The following example changes the title, theme and the time drafts are saved in the backend:

```abap
CLASS zcl_a2ui5_user_exit DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES  z2ui5_if_exit.

ENDCLASS.

CLASS zcl_a2ui5_user_exit IMPLEMENTATION.

  METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-title = `my title`.
    cs_config-theme = `sap_belize`.

  ENDMETHOD.

  METHOD z2ui5_if_exit~set_config_http_post.

    cs_config-draft_exp_time_in_hours = 8.

  ENDMETHOD.

ENDCLASS.
```
