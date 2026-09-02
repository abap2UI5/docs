# Where Your Own JavaScript Goes

*abap2UI5 Know-How #12 — draft*

Sooner or later an app needs something the XML view cannot express. A chart
library. A control nobody has wrapped. A UI5 method that exists only as a
method — `sap.m.Carousel` moves through `setActivePage( )` and through nothing
else, and no property will do it for you.

abap2UI5 has no plugin system for this, and that is deliberate. It has three
seams, each one a declared place rather than an escape hatch.

**An imperative method needs no JavaScript at all.** The client can call one on
a control by id:

```abap
    " t_arg is positional: control id, method, parameters
    client->follow_up_action( val   = z2ui5_if_client=>cs_event-control_by_id
                              t_arg = VALUE #( ( `myCarousel` )
                                               ( `setActivePage` )
                                               ( `page2` ) ) ).
```

The whitelist decides what is reachable. Check it before writing anything —
a method it already declares costs one call, and an argument it does not
declare is dropped in silence.

**A custom control lives in its own BSP.** The frontend resolves two reserved
resource roots — `z2ui5_cci` for the custom-controls addon, `z2ui5_ccc` for a
customer's own extension — so a control loads through the UI5 loader like any
other module, not as a string smuggled through a view.

**A view attribute can compute.** UI5 expression binding is available in an
app view — `{= ${STATUS} === 'E' ? 'Error' : 'None' }` — evaluated in the
browser against the model that just arrived, with no roundtrip and no module to
load. Write it as a backtick literal rather than a string template: a template
has to escape every brace, and one missed escape is a parser error on the whole
statement instead of a wrong string.

**Everything else is a system decision, not an app decision.** Extra JavaScript
for the initial page is `custom_js` in the HTTP GET configuration, set through
`z2ui5_if_exit` — one place, reviewable, and the same for every app in the
system.

None of them lets an app change the framework, and none requires the framework
to change for an app. No plugin registry to learn, and no pull request to wait
for either.

**Extensibility is not the absence of a boundary. It is knowing exactly where
the boundary is.**

Happy ABAPing! 🦖🦕🦣

---

## LinkedIn teaser post

Plain text — LinkedIn renders no markdown.

> Sooner or later an app needs what an XML view cannot express: a chart library,
> an unwrapped control, or a UI5 method that exists only as a method —
> sap.m.Carousel moves through setActivePage( ) and nothing else.
>
> abap2UI5 has no plugin system for that, on purpose. It has three declared
> seams: an imperative method reached by control id from ABAP, a custom control
> living in its own BSP behind a reserved resource root, and extra JavaScript
> for the initial page set once through the framework exit.
>
> None lets an app change the framework. None makes the framework change for an
> app.
>
> New article 🎉
>
> When you last needed a custom control, what did it cost to get it in?
>
> #ABAP #SAP #UI5
