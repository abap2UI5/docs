# #7 One Handler for Every App

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

## How Much Handler That Is

The communication core of abap2UI5 is one HTTP handler class, two interfaces
and one database table. Originally about 2,300 lines of ABAP. The framework has
grown since — most visibly the optional view builder — but the part that
carries every request is still that.

It is small because of what it leaves to the apps. The framework does not build
views; apps do. It does not decide program flow; apps do. It does not wrap UI5
controls, so it does not grow when UI5 does. It does not implement a protocol,
because the protocol is a POST with two strings.

That has a practical consequence. A framework in the request path of a business
application sees every input, every response and every user, and a dependency
that cannot be read cannot really be reviewed. Here one class holds the logic,
and reading it is an afternoon. The audit answers are short for the same
reason: no CDS artefacts, no RAP objects, no generated code, no build step, no
transitive package tree. The system footprint is the source in the repository.

What that removes is not effort — the app still decides everything — but
artefacts. A screen stops being a set of objects to create, name, transport and
govern, and becomes a class.

**A handler that knows nothing about the application never has to be written
again for the next one.**

Happy ABAPing! 🦖🦕🦣
