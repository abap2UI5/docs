# Somewhere on the Way to UI5, We Lost RTTI

*abap2UI5 Know-How #1 — scheduled for Tuesday, 25 August 2026*

Before Fiori, ABAP developers routinely built screens for tables whose
structure was unknown when the code was written. It was ordinary work.
`cl_salv_table=>factory( )` took any internal table and drew it, RTTI answered
what the columns were, and the DDIC supplied the labels.

A whole category of tooling was built that way: data browsers, table
maintenance, migration cockpits, generic reports, job monitors — anything whose
job was to be useful for a structure it had never seen. The generality was not
a clever trick, it was the requirement. A tool that only works for the
structures somebody anticipated is not a tool, it is an application.

That capability never left the language. What quietly disappeared was a way to
put a modern user interface on it.

## The design-time contract

The standard path to a UI5 application goes through a typed OData service: a
CDS view, an entity type, a metadata document. The shape of the model is
decided when the code is written and baked into the contract.

That is a feature, and an important one. The metadata is what makes a service
consumable by clients nobody controls — cacheable, documentable, versionable,
stable. For a published business service, the contract is most of the value.

It is also a precondition, and ABAP has a long tradition of not needing one:

```abap
" ABAP, any release, since forever
cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                        CHANGING  t_table      = <lt_any> ).
lo_alv->display( ).
```

Two statements. Any internal table. No type known at the time of writing. RTTI
reads the structure at runtime, the field catalog follows from it, DDIC labels
come along for free.

The same tool expressed as a typed service hits a wall in the first minute: the
entity type has to exist before anything else can happen. Which is exactly
right when the structure is known, and a dead end when not knowing it is the
entire point of the tool.

So the generic tools stayed where they were. Not because anyone decided ALV was
the right long-term answer for them, but because the road to anything newer
started with a question they could not answer.

## Binding at runtime

An abap2UI5 view is a string the application builds, and the model is bound
from ABAP data — including data whose type only exists at runtime. Which means
the old pattern is available again:

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

No entity type, no CDS view, no service binding. The columns are whatever the
table happens to have when the method runs, and the UI5 binding paths are the
component names RTTI just handed back.

The DDIC is one step further down the same road. The component carries its type
description, so the loop can ask whether it is a dictionary type and pull the
real field label instead of the technical name:

```abap
IF ls_comp-type IS BOUND AND ls_comp-type->is_ddic_type( ) = abap_true.
  DATA(lv_type)  = z2ui5_cl_util=>rtti_get_ddic_type_name( ls_comp-type ).
  DATA(lv_label) = z2ui5_cl_util=>rtti_get_data_element_text_l( lv_type ).
ENDIF.
```

Which is, of course, exactly what SALV always did.

## It is not hypothetical

The framework uses the pattern itself. `z2ui5_cl_pop_table` is a generic table
popup built on precisely this code — RTTI over an arbitrary internal table,
DDIC labels where they exist, technical names where they do not. Calling it is
one line:

```abap
client->nav_app_call( z2ui5_cl_pop_table=>factory( i_tab = lt_any ) ).
```

Any table, any structure, a UI5 dialog. No design-time contract anywhere in
sight.

## What runtime typing costs

A generic table has no contract, and that is a real price rather than a
technicality. Nothing external can rely on it. Nothing announces that the
underlying structure changed — the screen simply grows a column, or loses one,
and no consumer was warned because there was no consumer to warn. Field labels
are only as good as the DDIC underneath them, and a locally defined structure
gets technical names.

So runtime typing is not an upgrade over a typed service, and reaching for it
to publish an API would be a mistake. The two answer different questions. One
asks what a foreign system can depend on for the next five years. The other
asks what this internal tool should show a user right now, given a structure it
was handed a millisecond ago.

Most of the tooling described at the top of this article has always been the
second question. ABAP has been unusually good at answering it for two decades:
first-class runtime type information, dynamic SQL alongside it, and
`cl_abap_structdescr` still released in ABAP Cloud.

## What actually changed

None of the runtime machinery went away. Only the screen in front of it did —
and it went away by accident, as a side effect of the path to a UI5 application
running through a typed service.

abap2UI5 gives runtime-typed ABAP a UI5 face again. Not by adding a generic
framework on top, but by binding ABAP data directly instead of through a
generated service, which means data whose type is decided at runtime is not a
special case. It is just data.

If a tool in your system has been an ALV grid since 2009 because giving it
anything else was never realistic, that is the specific thing that changed.

---

## LinkedIn teaser post

> Before Fiori, screens were routinely built for tables nobody had seen.
> `cl_salv_table=>factory( )` took any internal table and drew it — RTTI
> answered what the columns were, the DDIC supplied the labels, and a whole
> category of ABAP tooling was built that way: data browsers, table
> maintenance, migration cockpits.
>
> Then the path to a screen started going through a typed service, and the
> entity type had to exist first. Exactly right for something a foreign system
> depends on; a dead end when not knowing the structure is the entire point of
> the tool. So those tools stayed ALV grids — not because anyone decided that
> was right, but because the road to anything newer began with a question they
> could not answer.
>
> A new article on building UI5 views from RTTI at runtime, what the framework
> already ships for it, and what the approach genuinely costs: [link]
>
> Where do you still use RTTI-driven tooling today?
>
> #ABAP #SAP #UI5
