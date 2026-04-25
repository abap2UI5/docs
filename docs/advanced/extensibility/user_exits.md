---
outline: [2, 4]
---
# User Exits

abap2UI5 offers predefined user exits for adjusting the standard behavior. The interface [`Z2UI5_IF_EXIT`](https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_if_exit.intf.abap) exposes the user exits. To use them on your system, create a class that implements the interface and its methods. The abap2UI5 class [`Z2UI5_CL_EXIT`](https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_cl_exit.clas.abap) calls them dynamically. Put your class in a custom package — **not** in the abap2UI5 packages.

The interface exposes two exit methods:
- **`set_config_http_get`** — called on the initial HTTP GET request (page load). Use it to customize frontend settings like the page title, UI5 theme, or UI5 version.
- **`set_config_http_post`** — called on every later HTTP POST request (each roundtrip). Use it to set backend behavior like the draft expiration time.

Both methods receive a `cs_config` changing parameter whose fields you can set as needed. The example below changes the title, the theme, and how long the backend keeps drafts:

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
