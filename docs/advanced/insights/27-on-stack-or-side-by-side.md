# On-Stack or Side-by-Side

Two places to run an abap2UI5 app, and the choice is about lifecycles rather
than about code.

**On-stack** means the app runs inside the SAP system. It reads the data
directly, there is no remote call and no second system to operate, and
deployment is the transport you already use. What it inherits is that system's
lifecycle: its release, its upgrade window, its maintenance slots.

**Side-by-side** means the app runs on the SAP BTP ABAP Environment and calls
the S/4 system remotely — released OData, RFC or SOAP services. Everything on
the BTP side is Level A by construction, and it works against S/4HANA Public
Cloud, where on-stack custom code is not an option.

![The apps run on the SAP BTP ABAP Environment and call released remote APIs of S/4HANA](/advanced/use_cases/side_by_side_level_a.svg){ width=60% }

The separation is the point. A side-by-side app is released, upgraded and
restarted on its own schedule, and the S/4 system does not have to agree. Where
a release cycle is the actual constraint — a locked system, a long change
window, a team that cannot get a transport through this quarter — that is worth
more than the directness on-stack gives.

The cost is equally plain: a remote call is slower than a local read, the
released remote APIs are narrower than what a local `SELECT` can reach, and
there is a second system to run. When they do not cover the case, a service on
the S/4 side closes the gap and is graded on its own.

**Neither choice touches the app class. The same code renders in both places —
only what it reads changes.**

Happy ABAPing! 🦖🦕🦣
