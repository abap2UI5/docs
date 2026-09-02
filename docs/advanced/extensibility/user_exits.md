---
outline: [2, 4]
---
# User Exits

abap2UI5 offers predefined user exits for tweaking the standard behavior. The interface [`Z2UI5_IF_UI5_EXIT`](https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_if_ui5_exit.intf.abap) exposes the user exits. To use them on your system, build a class that implements the interface and its methods. The abap2UI5 class [`Z2UI5_CL_UI5_USER_EXIT`](https://github.com/abap2UI5/abap2UI5/blob/main/src/01/04/z2ui5_cl_ui5_user_exit.clas.abap) calls them dynamically. Put your class in a custom package — **not** in the abap2UI5 packages.

The interface exposes two exit methods:
- **`set_config_http_get`** — called on the initial HTTP GET request (page load). Use it to set frontend properties like the UI5 theme, the UI5 version, or the inline CSS.
- **`set_config_http_post`** — called on every later HTTP POST request (each roundtrip). Use it to set backend behavior like the draft expiration time.

Both methods take a `cs_config` changing parameter whose fields you can set as needed. The example below changes the theme and how long the backend keeps drafts:

```abap
CLASS zcl_a2ui5_user_exit DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES  z2ui5_if_ui5_exit.

ENDCLASS.

CLASS zcl_a2ui5_user_exit IMPLEMENTATION.

  METHOD z2ui5_if_ui5_exit~set_config_http_get.

    cs_config-theme = `sap_belize`.

  ENDMETHOD.

  METHOD z2ui5_if_ui5_exit~set_config_http_post.

    cs_config-draft_exp_time_in_hours = 8.

    " CSRF protection is on by default; disable it only if your endpoint
    " must accept cross-origin POSTs (see the Security page)
    " cs_config-check_csrf_active = abap_false.

    " An unhandled exception is answered with a 500 whose body carries the
    " whole exception chain - class names, source positions, the system
    " context. Priceless while developing, more than a production system
    " should hand a browser; this replaces it with a bare
    " `Internal Server Error` (see the Exception page)
    " cs_config-check_hide_error_details = abap_true.

  ENDMETHOD.

ENDCLASS.
```


