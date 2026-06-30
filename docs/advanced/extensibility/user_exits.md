---
outline: [2, 4]
---
# User Exits

abap2UI5 offers predefined user exits for tweaking the standard behavior. The interface [`Z2UI5_IF_EXIT`](https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_if_exit.intf.abap) exposes the user exits. To use them on your system, build a class that implements the interface and its methods. The abap2UI5 class [`Z2UI5_CL_EXIT`](https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_cl_exit.clas.abap) calls them dynamically. Put your class in a custom package — **not** in the abap2UI5 packages.

The interface exposes two exit methods:
- **`set_config_http_get`** — called on the initial HTTP GET request (page load). Use it to set frontend properties like the page title, UI5 theme, or UI5 version.
- **`set_config_http_post`** — called on every later HTTP POST request (each roundtrip). Use it to set backend behavior like the draft expiration time.

Both methods take a `cs_config` changing parameter whose fields you can set as needed. The example below changes the title, the theme, and how long the backend keeps drafts:

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

## Draft Persistence Exit

Between roundtrips abap2UI5 stores the session state ("draft") in the database
table `Z2UI5_T_01`. By default the draft service
([`Z2UI5_CL_CORE_SRV_DRAFT`](https://github.com/abap2UI5/abap2UI5/blob/main/src/01/01/z2ui5_cl_core_srv_draft.clas.abap))
writes the draft with `MODIFY` followed by `COMMIT WORK AND WAIT`. That commit
runs on the primary database connection, so it also commits whatever the
surrounding application has changed in the current LUW.

The interface [`Z2UI5_IF_PERSIST`](https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_if_persist.intf.abap)
lets you replace the complete draft persistence with your own implementation —
for example to persist the draft on a **secondary database connection** so that
the framework no longer issues a `COMMIT WORK` on your application's LUW.

Just like `Z2UI5_IF_EXIT`, you only have to **create a class that implements the
interface** and put it in a custom package. abap2UI5 discovers it at runtime via
RTTI and routes every draft operation through it. As long as no implementing
class exists, the default database access is used unchanged.

The interface mirrors the draft service operations:

| Method | Purpose |
|---|---|
| `create` | Persist a draft (called on every roundtrip) |
| `read_draft` | Read a full draft incl. the serialized app state |
| `read_info` | Read only the navigation/chaining info of a draft |
| `check_exists` | Check whether a draft id exists |
| `count_entries` | Count stored drafts |
| `cleanup` | Delete expired drafts |

A minimal example that keeps using `Z2UI5_T_01` but writes on a secondary
connection (so the draft commit no longer affects the application's LUW):

```abap
CLASS zcl_a2ui5_persist DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_persist.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_con TYPE dbcon-con_name VALUE 'A2UI5'. "secondary db connection

ENDCLASS.

CLASS zcl_a2ui5_persist IMPLEMENTATION.

  METHOD z2ui5_if_persist~create.

    DATA ls_db TYPE z2ui5_if_persist~ty_s_db.
    ls_db-id                = draft-id.
    ls_db-id_prev           = draft-id_prev.
    ls_db-id_prev_app       = draft-id_prev_app.
    ls_db-id_prev_app_stack = draft-id_prev_app_stack.
    ls_db-timestampl        = z2ui5_cl_util=>time_get_timestampl( ).
    ls_db-data              = model_xml.

    MODIFY z2ui5_t_01 CONNECTION (c_con) FROM @ls_db.
    COMMIT CONNECTION a2ui5.

  ENDMETHOD.

  METHOD z2ui5_if_persist~read_draft.

    SELECT SINGLE * FROM z2ui5_t_01 CONNECTION (c_con)
      WHERE id = @id
      INTO @result.

  ENDMETHOD.

  METHOD z2ui5_if_persist~read_info.

    SELECT SINGLE id, id_prev, id_prev_app, id_prev_app_stack
      FROM z2ui5_t_01 CONNECTION (c_con)
      WHERE id = @id
      INTO CORRESPONDING FIELDS OF @result.

  ENDMETHOD.

  METHOD z2ui5_if_persist~check_exists.

    SELECT SINGLE id FROM z2ui5_t_01 CONNECTION (c_con)
      WHERE id = @id
      INTO @DATA(lv_id).
    result = xsdbool( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD z2ui5_if_persist~count_entries.

    SELECT COUNT( * ) FROM z2ui5_t_01 CONNECTION (c_con) INTO @result.

  ENDMETHOD.

  METHOD z2ui5_if_persist~cleanup.

    " delete expired drafts on the secondary connection
    DELETE FROM z2ui5_t_01 CONNECTION (c_con) WHERE timestampl < @lv_limit.
    COMMIT CONNECTION a2ui5.

  ENDMETHOD.

ENDCLASS.
```

::: warning ABAP Cloud
Secondary database connections via the classic `CONNECTION` addition are **not
released for ABAP Cloud**. There the exit can still be used to redirect drafts
to a different store, but not with a classic secondary connection.
:::
