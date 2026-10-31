---
outline: [2, 4]
description: S-RTTI is how abap2UI5 serializes the data references of an app instance into the draft - every TYPE REF TO data attribute travels with its runtime type - as a mirrored copy under z2ui5_cl_srt_* that app code does not call.
---
# S-RTTI

Between two roundtrips abap2UI5 serializes the whole app instance into a draft row and reads it back for the next `main( )` (see [Life Cycle](/cookbook/event_navigation/life_cycle#check-on-event-fires-once-per-roundtrip)). The standard transformation that does it, `CALL TRANSFORMATION id`, writes an attribute by its declared type — and a data reference has none that would help: `TYPE REF TO data` says nothing about the table or structure behind it, and a type created at runtime with `cl_abap_tabledescr=>create( )` exists nowhere a transformation could look it up.

The framework ships [S-RTTI](https://github.com/sandraros/S-RTTI) for exactly that gap: it wraps the RTTS type description of a data object in a serializable object, so type and data can be written as one and the data object rebuilt from both.

## What the Serializer Does

Before the draft is written, `z2ui5_cl_ui5_srv_model` walks the attributes of the app instance (`main_attri_db_save_srtti`). Every attribute whose type kind is a data reference is treated the same way, whether it points at a DDIC type or at one created at runtime:

1. The target the reference points at is described once.
2. The table, structure or elementary value behind it is serialized through S-RTTI — type description and data as one pair — into the attribute's row of the framework's attribute table.
3. The reference itself is cleared on the live instance, so the standard transformation that follows sees an initial reference and writes nothing for it.
4. On the next roundtrip the draft is read, every S-RTTI pair is parsed back, a data object of the described type is created and the attribute is pointed at it again (`main_attri_db_load`). The instance that goes on rendering in the same roundtrip gets its original references back untouched (`main_attri_reattach`).

So a `TYPE REF TO data` attribute survives a roundtrip like any typed one, and `_bind( )` on the table behind it works as on a declared one:

<!-- playground: no Run button — describe_by_name is a DDIC lookup, and there is no DDIC in the browser -->
```abap
CLASS zcl_sample_srtti DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mr_tab TYPE REF TO data.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.
    METHODS view_display.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_srtti IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    me->client = client.

    CASE abap_true.

      WHEN client->check_on_init( ).
        " a table type that exists only at runtime: the line type is read
        " from the DDIC by name, the table type built from it
        DATA(lo_struct) = cl_abap_structdescr=>describe_by_name( `USR01` ).
        DATA(lo_table)  = cl_abap_tabledescr=>create(
                              p_line_type  = CAST #( lo_struct )
                              p_table_kind = cl_abap_tabledescr=>tablekind_std
                              p_unique     = abap_false ).
        CREATE DATA mr_tab TYPE HANDLE lo_table.

        FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
        ASSIGN mr_tab->* TO <tab>.
        SELECT * FROM usr01 INTO TABLE @<tab> UP TO 10 ROWS.

        view_display( ).

      WHEN client->check_on_navigated( ).
        view_display( ).

    ENDCASE.

  ENDMETHOD.

  METHOD view_display.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN mr_tab->* TO <tab>.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->a( n = `title` v = `Users`

                )->ele( `List`
                    )->a( n = `items` v = client->_bind( <tab> )

                    )->ele( `items`
                        )->tag( `StandardListItem`
                            )->a( n = `title` v = `{BNAME}` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

The table type is rebuilt from the draft on every roundtrip; the app never names it again after the init branch.

## Limits

- **Object references are not serialized.** The walk skips every attribute of type kind `oref` — a `TYPE REF TO` a class or an interface — and the draft carries no objects. Keep a model instance or a helper object out of the attributes, or recreate it in `main( )`.
- **A type S-RTTI does not know raises `z2ui5_cx_srt`.** Elementary types, structures, tables, references, classes and interfaces are described; any other type kind — one a future ABAP release adds — ends the save with that exception.
- **A dictionary type is resolved by name on the way back.** S-RTTI stores the absolute name of a dictionary type and describes it again on load, so a draft written before a change to the DDIC object reads back against the changed definition — or not at all.
- **A reference to a reference** (`MR_REF->*` is itself `TYPE REF TO data`) is reached through its parent; a longer chain is not followed.

An attribute that is declared with a type — a DDIC table type, a `TYPES` of the class — never goes through S-RTTI at all; only the attributes whose type kind is a data reference do.

## Integration

S-RTTI plugs directly into the `z2ui5` namespace and installs with abap2UI5 automatically. The classes carry the `z2ui5_cl_srt_*` prefix (for example `z2ui5_cl_srt_datadescr`) and sit in `src/00/02`: a **mirrored copy** for the framework's own serializer, outside the released API, like the [ajson](/technical/tools/ajson) copy next to it. Do not call them from app code — the abap2UI5 linter reports such a call as `non-released-api`; an app that needs S-RTTI itself installs its own copy of the project.

## Updates

Every S-RTTI update and bug fix flows into abap2UI5 automatically via GitHub Actions and the [mirror-srtti](https://github.com/abap2UI5/mirror-srtti) repository, so you always run the latest version.
