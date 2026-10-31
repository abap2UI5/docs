---
description: Why no abap2UI5 app has a service of its own - one generic HTTP handler moves two strings for every app, and what the framework leaves to the apps to stay that small.
---
# #7 One Handler for Every App

Which backend artifacts does a screen need before a request can reach it? For
an OData-backed app the answer is a service: a CDS view or two, a service
definition, a service binding, a behavior definition and its implementation, a
projection — each belonging to exactly one application.

An abap2UI5 app has none of them, and there is no SEGW project underneath
either. Every app in the system is served by the same generic HTTP handler, and
it is generic in a specific way: it does not know the app, the view or the
model. It moves two strings.

![A service per screen, against one handler that knows none of them.](/insights/07-one-service.svg)

*A service per screen, against one handler that knows none of them.*

That is visible from inside, and it is the first thing worth trying in a
debugger. Set a breakpoint in an app class and look at the call stack — between
the ICF and the app there are only the framework's own two handler classes, a
few frames each. No OData runtime, no SADL, no Gateway. The stack fits on a
screenshot.

The reason it can be generic is that nothing about the data is agreed in
advance. In an OData conversation the metadata comes first and fixes the shape;
data follows within that shape for the rest of the session. Here the model
travels **with** every response — [#5](/advanced/insights/05-ui5-over-the-wire) —
so each response may carry a different one.
There is no contract to violate because there is no contract to register.

## How Much Handler That Is

The communication core of abap2UI5 started as one HTTP handler class, two
interfaces and one database table. The framework has grown since — most
visibly the optional view builder — and the part that carries every request is
now a dozen small classes in the core package (handler, action, binding, model,
events, draft) behind the same two interfaces and the same one table,
`z2ui5_t_01`.

It is small because of what it leaves to the apps. The framework does not build
views; apps do. It does not decide program flow; apps do. It does not wrap UI5
controls, so it does not grow when UI5 does. It does not implement a protocol,
because the protocol is a POST with two strings.

The system footprint is the source in the repository: no CDS artifacts, no
RAP objects, no generated code, no build step. What that means for a review
of the dependency is on
[What you keep](https://abap2ui5.github.io/why/#what-you-keep){target="_self"}.

What that removes is not effort — the app still decides everything — but
artifacts. A screen stops being a set of objects to create, name, transport and
govern, and becomes a class.
