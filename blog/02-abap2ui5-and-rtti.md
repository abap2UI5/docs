# Whatever Happened to RTTI?

*abap2UI5 Know-How #2*

Before Fiori, ABAP developers routinely built screens for tables whose
structure they did not know when they wrote the code. It was ordinary work.
`cl_salv_table=>factory( )` took any internal table and drew it, RTTI answered
what the columns were, and the DDIC supplied the labels. A whole category of
tooling — data browsers, table maintenance, migration cockpits, generic
reports — was built that way.

Somewhere on the way to UI5, that stopped being possible, and I do not think we
ever really discussed it.

## The design-time contract

The standard path to a UI5 application goes through a typed OData service. A
CDS view, an entity type, a metadata document. The shape of the model is
decided when you write the code and baked into the contract.

That is a feature, and an important one. The metadata is what makes a service
consumable by clients you do not control, cacheable, documentable, versionable
and stable. For a published business service, the contract *is* the product.

But it is also a constraint, and ABAP has a long tradition of not needing it:

```abap
" ABAP, any release, since forever
cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                        CHANGING  t_table      = <lt_any> ).
lo_alv->display( ).
```

Two statements. Any internal table. No type known at the time of writing. RTTI
reads the structure at runtime, the field catalog follows from it, DDIC labels
come along for free.

Try the same through OData and the friction shows up in the first minute: the
entity type has to exist before anything else can happen. Which is fine when
you know what it is, and a dead end when the whole point of the tool is that
you do not.

## Binding at runtime

An abap2UI5 view is a string the application builds, and the model is bound
from ABAP data — including data whose type only exists at runtime. Which means
the old pattern comes back:

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

The DDIC is one step further along the same road. Since the component carries
its type description, the loop can ask whether it is a dictionary type and pull
the real field label instead of the technical name:

```abap
IF ls_comp-type IS BOUND AND ls_comp-type->is_ddic_type( ) = abap_true.
  DATA(lv_type)  = z2ui5_cl_util=>rtti_get_ddic_type_name( ls_comp-type ).
  DATA(lv_label) = z2ui5_cl_util=>rtti_get_data_element_text_l( lv_type ).
ENDIF.
```

Which is, of course, exactly what SALV always did.

## It is not hypothetical

The framework uses this pattern itself. `z2ui5_cl_pop_table` is a generic table
popup built on precisely this code — RTTI over an arbitrary internal table,
DDIC labels where they exist, technical names where they do not. Calling it is
one line:

```abap
client->nav_app_call( z2ui5_cl_pop_table=>factory( i_tab = lt_any ) ).
```

Any table, any structure, a UI5 dialog. No design-time contract anywhere in
sight.

## Where the line actually is

I want to be careful here, because there is an argument nearby that I am not
making. Runtime typing does not beat a typed service. For an API consumed by
systems you do not control, the metadata contract is the whole value, and OData
with RAP is the right tool for it. A generic table has no contract, and that is
a real cost: nothing external can rely on it, and nothing tells you when the
underlying structure changes.

The narrower point is that a great deal of ABAP work was never that kind of
application. It was internal tooling over structures known only at runtime, and
ABAP was unusually good at it — the language has had first-class runtime type
information for two decades, and dynamic SQL alongside it. None of that went
away. `cl_abap_structdescr` is still there, and it is released in ABAP Cloud.

What went away was a UI technology willing to talk to it. That is the gap
abap2UI5 happens to fill, more or less as a side effect of binding ABAP data
directly rather than through a generated service.

If you have a tool that has been an ALV grid since 2009 because there was never
a sensible way to give it a modern UI, this is the specific thing that changed.

---

## LinkedIn teaser post

> Before Fiori, we built screens for tables we had never seen.
> `cl_salv_table=>factory( )` took any internal table and drew it — RTTI
> answered what the columns were, the DDIC supplied the labels, and an entire
> category of ABAP tooling was built that way.
>
> Then UI5 arrived, the path to a screen went through a typed OData service,
> and the entity type had to exist first. Which is exactly right for a
> published business service, and a dead end when the whole point of the tool
> is that you do not know the structure yet.
>
> I wrote about how RTTI-driven UIs work in abap2UI5, and where the line
> between the two approaches actually sits: [link]
>
> The question I keep coming back to: how many ALV-based internal tools are
> still ALV-based purely because there was never a sensible way to give them a
> modern UI?
>
> #ABAP #SAP #UI5
