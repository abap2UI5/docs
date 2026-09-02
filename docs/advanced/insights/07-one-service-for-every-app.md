# One Service for Every App

Count what a screen usually costs in backend artefacts. A CDS view or two, a
service definition, a service binding, a behavior definition and its
implementation, a projection — each one named, transported, reviewed, and each
one belonging to exactly one application.

An abap2UI5 app adds none of them — and there is no SEGW project underneath
either. Every app in the system is served by the same generic HTTP handler, and
it is generic in a specific way: it does not know the app, the view or the
model. It moves two strings.

![A service per screen, against one handler that knows none of them.](/insights/07-one-service.svg)

*A service per screen, against one handler that knows none of them.*

That is visible from inside. Set a breakpoint in an app class and look at the
call stack — there is one frame between the app and the handler. No OData
runtime, no SADL, no Gateway.

The reason it can be generic is that nothing about the data is agreed in
advance. In an OData conversation the metadata comes first and fixes the shape;
data follows within that shape for the rest of the session. Here the model
travels **with** every response, so each response may carry a different one.
There is no contract to violate because there is no contract to register.

What that removes is not effort — the app still decides everything — but
artefacts. A screen stops being a set of objects to create, name, transport and
govern, and becomes a class. In a landscape where every new object is something
someone has to review, keep clean and eventually migrate, the cheapest artefact
is the one that was never created.

**A service that knows nothing about the application never has to be written
again for the next one.**

Happy ABAPing! 🦖🦕🦣
