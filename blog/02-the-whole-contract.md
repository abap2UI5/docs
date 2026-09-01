# The Whole Contract

*abap2UI5 Know-How #2 — draft*

The useful question about a UI framework is not what it can do. It is what it
wants from you.

Most of them want a good deal: a structure to follow, a lifecycle to fit into,
layers to fill in. abap2UI5 fits its answer on a page, so here it is, complete:

```abap
INTERFACE z2ui5_if_app PUBLIC.
  INTERFACES if_serializable_object.

  METHODS main
    IMPORTING
      client TYPE REF TO z2ui5_if_client.

ENDINTERFACE.
```

One interface, one method. The framework calls `main( )` on every roundtrip,
the application decides what to display and how to react, and the conversation
ends there. Nothing else to implement, extend, register or configure.

The more interesting half is what the contract does **not** contain. No data
model to declare. No behavior definition. No service to define, no binding to
create, no annotation model. No BSP application per app, and no frontend
artefact to transport. Activating the class and calling the ICF endpoint with
`?app_start=zcl_my_app` is the deployment.

That is a statement about scope, not about size. The framework never learns
what your application *is* — only what it wants on screen.

Which is also the answer to the question everyone asks first, whether this
competes with what they already run. A framework that asks for one method
cannot reorganise an architecture, because it never learns enough about it to
try.

And plainly, what it is not: abap2UI5 has no data model, no transactional
buffer, and no generated user interface. It will not derive a screen from
annotations and it will not manage drafts. Applications that need those need
something that provides them — abap2UI5 has never been a candidate.

Next in the series: what the small contract means when the data behind the
screen comes from a RAP business object.

Happy ABAPing! 🦖🦕🦣

---

## LinkedIn teaser post

Plain text — LinkedIn renders no markdown.

> The useful question about a UI framework is not what it can do. It is what it
> wants from you: a structure to follow, a lifecycle to fit into, layers to fill
> in.
>
> abap2UI5 fits its answer on a page. One interface, one method — and no data
> model to declare, no service to define, no binding, no annotations, no BSP per
> app, no frontend artefact to transport. Activating the class and calling the
> ICF endpoint is the deployment.
>
> That is a statement about scope, not about size: the framework never learns
> what your application is, only what it wants on screen.
>
> New article 🎉
>
> What does your UI framework ask of your architecture?
>
> #ABAP #SAP #UI5
