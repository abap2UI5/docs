# #21 One App, Many Systems

A side-by-side app already runs outside the system it serves. Which raises a
question with a more interesting answer than it looks: how many systems can it
serve?

More than one. The app lives on the SAP BTP ABAP Environment and reaches each
S/4 system through its released APIs, so the connection is configuration rather
than code.

![One abap2UI5 app on the SAP BTP ABAP Environment serves several S/4HANA systems through their released APIs](/advanced/use_cases/saas.svg){ width=60% }

That is a real SaaS shape, in ABAP. One codebase, deployed once, serving several
customer tenants — each with its own system, its own data and its own release,
none of them needing anything installed. Nobody transports into a customer
system, nobody schedules a downtime there, and a fix reaches every tenant at
once.

It works because of what the earlier articles already established rather than
because of anything added for it. The frontend is a shell that renders whatever
arrives, so it does not care which system produced it. The app is one class with
no artefacts beside it, so there is nothing per-tenant to deploy. And the state
that makes a roundtrip work travels in the request, so no server holds a session
belonging to one customer.

The limits are the ones the shape implies. Every tenant needs its released APIs
reachable, latency is now on the wire, and the tenant's data boundary is
something the app has to enforce rather than something the system enforces for
it — one app now sees several customers.

**A framework with nothing to install per system is a framework that can serve
systems it was never installed on.**

Happy ABAPing! 🦖🦕🦣
