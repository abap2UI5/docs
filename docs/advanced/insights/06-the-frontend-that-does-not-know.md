# The Frontend That Does Not Know What It Shows

The first GET request delivers a UI5 app to the browser. It is worth asking
what is actually in it.

Not the screen. Not the fields, not the table, not the buttons, and not what
any of them do. The app that arrives is a shell: enough UI5 to take a view and
a model from the server, render them, and send events back. It cannot tell
whether it is currently showing a table, a form or a wizard, and it does not
know which action follows the next click.

Everything it needs arrives per request. Everything it decides is decided on
the server.

The pattern has a name: a **Hypermedia-Driven Application**. Between the
multi-page app, where every interaction fetches a whole document, and the
single-page app, which holds routes and state in the browser and needs a
rebuild to change either, an HDA renders what the server sends and knows
nothing else. It is the sweet spot those two leave open, and it is where htmx,
Hotwire and LiveView all sit.

![Multi-page, single-page, hypermedia-driven — and what each has to rebuild.](/insights/06-mpa-spa-hda.svg)

*Multi-page, single-page, hypermedia-driven — and what each has to rebuild.*

That is the same division of labour SAP GUI had. PBO builds the screen, the
user acts, PAI receives the event and decides what happens next. The dialog
step moved to AJAX and the screen became an XML view, but the shape of the
conversation did not change — which is why the flow reads as familiar to
anyone who has written a module pool, and as strange to anyone who has only
written SPAs.

The consequence is the part worth keeping. There is **one** shell, and every
app in the system shares it. Not one deployed frontend per app, drifting to a
different UI5 version, a different bootstrap, a different set of libraries,
each pinned to whenever someone last had time to touch it. One artefact to
keep current, and every app is current with it.

**A frontend that knows nothing about the app is a frontend that never needs
to be redeployed when the app changes.**

Happy ABAPing! 🦖🦕🦣
