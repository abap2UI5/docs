# Somewhere on the Way to UI5, We Lost RTTI

*abap2UI5 Know-How #1 — scheduled for Tuesday, 25 August 2026*

![The dinosaur at a classic ALV grid, the sheep and the sloth at the same table
rendered in UI5, with cl_abap_structdescr->get_components( ) bridging the
two](assets/01-rtti-header.png)

Before Fiori, ABAP developers routinely built screens for tables whose
structure was unknown when the code was written:

```abap
cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                        CHANGING  t_table      = <lt_any> ).
lo_alv->display( ).
```

Two statements, any internal table, no type known at the time of writing. RTTI
read the structure at runtime, the field catalog followed from it, DDIC labels
came along for free. Data browsers, table maintenance, migration cockpits,
generic reports — a whole category of tooling was built that way, and the
generality was the requirement, not a trick.

## Why those tools are still ALV grids

The standard path to a UI5 application goes through a typed OData service. The
shape of the model is decided when the code is written and baked into the
contract — which is exactly what makes a service dependable for clients nobody
controls.

It is also a precondition: the entity type has to exist before anything else
can happen. That is fine when the structure is known, and a dead end when not
knowing it is the entire point of the tool.

So the generic tools stayed where they were. Not because anyone decided ALV was
the right long-term answer, but because the road to anything newer began with a
question they could not answer.

## Binding at runtime

An abap2UI5 view is a string the application builds, and the model is bound
from ABAP data — including data whose type only exists at runtime:

```abap
" what are the columns of this table?
DATA(lo_tab)  = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( <lt_tab> ) ).
DATA(lo_line) = CAST cl_abap_structdescr( lo_tab->get_table_line_type( ) ).
DATA(lt_comp) = lo_line->get_components( ).

DATA(table) = z2ui5_cl_ui5_view_builder=>factory(
                  )->ele( n = `View` ns = `mvc`
                      )->a( n = `xmlns`     v = `sap.m`
                      )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
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
```

No entity type, no CDS view, no service binding. The columns are whatever the
table happens to have when the method runs, and the binding paths are the
component names RTTI just handed back. One step further, `ls_comp-type` answers
whether a component is a DDIC type, which is where the real field labels come
from — exactly as SALV always did it.

The framework ships this. `z2ui5_cl_pop_table` is a generic table popup built
on the same code, and calling it is one line:

```abap
client->nav_app_call( z2ui5_cl_pop_table=>factory( i_tab = lt_any ) ).
```

## What it costs

A generic table has no contract, and that is a real price. Nothing external can
depend on it, nothing announces that the underlying structure changed, and a
locally defined structure gets technical names instead of labels.

So this is not an upgrade over a typed service — the two answer different
questions. One asks what a foreign system can depend on for the next five
years. The other asks what an internal tool should show a user right now, given
a structure it was handed a millisecond ago.

## What actually changed

None of the runtime machinery ever went away. `cl_abap_structdescr` is still
there, still released in ABAP Cloud. Only the screen in front of it
disappeared, and it disappeared by accident — a side effect of the path to a
UI5 application running through a typed service.

abap2UI5 gives runtime-typed ABAP a UI5 face again. Not by adding a generic
framework on top, but by binding ABAP data directly, so data typed at runtime
is not a special case. It is just data.

If a tool in your system has been an ALV grid since 2009 because giving it
anything else was never realistic, that is the specific thing that changed.

---

## LinkedIn teaser post

> Before Fiori, screens were routinely built for tables nobody had seen.
> `cl_salv_table=>factory( )` took any internal table and drew it — RTTI
> answered what the columns were, the DDIC supplied the labels, and a whole
> category of ABAP tooling was built that way.
>
> Then the path to a screen started going through a typed service, and the
> entity type had to exist first. So those tools stayed ALV grids — not because
> anyone decided that was right, but because the road to anything newer began
> with a question they could not answer.
>
> A new article on building UI5 views from RTTI at runtime, and what the
> approach costs: [link]
>
> Where do you still use RTTI-driven tooling today?
>
> #ABAP #SAP #UI5
