# abap2UI5 Know-How #2 — Whatever Happened to RTTI?

> **Status:** draft · **Target:** LinkedIn native post + code carousel
> **Frame:** runtime typing is a capability we lost, not a better contract.
> Do not argue against OData — argue for the category of tooling it never fit.

## Hook (the ~200 characters visible before "see more")

> Before Fiori, ABAP developers built screens for tables whose structure they
> did not know at compile time. `cl_salv_table=>factory( )` took any internal
> table and drew it.
>
> Somewhere on the way to UI5, we lost that.

## Body

The standard path to a UI5 application goes through a typed OData service. A
CDS view, an entity type, a metadata document. The shape of the model is
decided at design time and baked into the contract. That is a feature — it is
what makes a service consumable, cacheable, documentable and stable.

It is also a constraint, and ABAP has a long tradition of not needing it:

```abap
" ABAP, any release, since forever
cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                        CHANGING  t_table      = <lt_any> ).
lo_alv->display( ).
```

Two statements. Any internal table. No type known when the code was written.
RTTI reads the structure at runtime, the field catalog follows from it, DDIC
labels come along for free. Generic report tools, table maintenance, data
browsers, migration cockpits — a whole category of ABAP tooling was built
exactly this way.

Try the same through OData and the friction shows up immediately: the entity
type has to exist first.

### abap2UI5 binds at runtime, not at design time

An abap2UI5 view is a string the app builds, and the model is bound from ABAP
data — including data whose type only exists at runtime. So the SALV pattern
comes back:

```abap
METHOD display_any_table.

  FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.
  ASSIGN mr_tab->* TO <lt_tab>.

  " RTTI: what are the columns of this table?
  DATA(lo_tab)  = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( <lt_tab> ) ).
  DATA(lo_line) = CAST cl_abap_structdescr( lo_tab->get_table_line_type( ) ).
  DATA(lt_comp) = lo_line->get_components( ).

  DATA(view) = z2ui5_cl_ui5_view_builder=>factory( ).

  DATA(table) = view->ele( n = `View` ns = `mvc`
                    )->a( n = `xmlns`     v = `sap.m`
                    )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

                    )->ele( `Page`
                        )->a( n = `title` v = `Generic Table`

                        )->ele( `Table`
                            )->a( n = `items` v = client->_bind( <lt_tab> ) ).

  " one column per component - discovered, not declared
  DATA(columns) = table->ele( `columns` ).
  LOOP AT lt_comp INTO DATA(ls_comp).
    columns->ele( `Column`
              )->ele( `header`
                  )->tag( `Text`
                      )->a( n = `text` v = ls_comp-name ).
  ENDLOOP.

  " one cell per component, bound by field name
  DATA(cells) = table->ele( `items`
                    )->ele( `ColumnListItem`
                        )->ele( `cells` ).
  LOOP AT lt_comp INTO ls_comp.
    cells->tag( `Text`
            )->a( n = `text` v = |\{{ ls_comp-name }\}| ).
  ENDLOOP.

  client->view_display( view->stringify( ) ).

ENDMETHOD.
```

No entity type. No CDS view. No service binding. The columns are whatever the
table happens to have when the method runs.

And you can go further than field names. RTTI reaches into the DDIC, so the
same loop can pull the real data element label instead of the technical name:

```abap
IF ls_comp-type IS BOUND AND ls_comp-type->is_ddic_type( ) = abap_true.
  DATA(lv_type)  = z2ui5_cl_util=>rtti_get_ddic_type_name( ls_comp-type ).
  DATA(lv_label) = z2ui5_cl_util=>rtti_get_data_element_text_l( lv_type ).
ENDIF.
```

Which is, of course, exactly what SALV always did.

### This is not hypothetical — it ships

The framework itself uses the pattern. `z2ui5_cl_pop_table` is a generic table
popup built on precisely this code, and calling it is a one-liner:

```abap
client->nav_app_call( z2ui5_cl_pop_table=>factory( i_tab = lt_any ) ).
```

Any table, any structure, a UI5 dialog with DDIC labels. No design-time
contract anywhere.

### Where the line actually is

I am not arguing that runtime typing beats a typed service. It does not — for a
published API consumed by systems you do not control, the metadata contract
*is* the value, and OData with RAP is the right tool for it.

The argument is narrower: a lot of ABAP work was never that. It was generic
tooling over structures known only at runtime, and it was excellent at it. That
capability never left the language — RTTI is still there, dynamic `SELECT` is
still there. What disappeared was a UI technology willing to talk to it.

*abap2UI5 gives runtime-typed ABAP a UI5 face again. Everything you already run
keeps running.*

---

**Closing question:** Where do you still use RTTI-driven tooling today — and
what does its UI look like?

**Series line:** `abap2UI5 Know-How — #1 Not a Programming Model · #2 RTTI · #3 The Roundtrip (next)`

**Hashtags:** `#ABAP #SAP #UI5 #RTTI #Fiori #OpenSource`

**First comment:** link to https://github.com/abap2UI5/abap2UI5

## Carousel outline (6 pages)

1. **Whatever happened to RTTI?** — before Fiori, we drew screens for tables we
   had never seen
2. **Two statements, any table** — the `cl_salv_table=>factory( )` snippet
3. **The OData path** — the entity type has to exist first. That is a feature,
   and it is also a constraint
4. **Discovered, not declared** — the RTTI loop building columns and cells
5. **It ships** — `z2ui5_cl_pop_table=>factory( i_tab = lt_any )`, one line, any
   structure, DDIC labels
6. **Where the line is** — typed service for a published API; runtime typing
   for generic tooling. Both, not either

## Split option

If this compresses badly on LinkedIn, it is two posts:

- **2a — Whatever happened to RTTI?** the history and the SALV comparison,
  ending on the question rather than the answer
- **2b — A UI5 table for a structure you have never seen** the RTTI loop and
  `z2ui5_cl_pop_table`
