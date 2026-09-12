# #27 One Codebase, 7.02 to ABAP Cloud

Most SAP landscapes are currently two landscapes. Something old that runs the
business, something new that is being built beside it, and a migration between
them measured in years rather than sprints. Anything written for one side is
usually written again for the other. It is why so much internal tooling exists
twice and works in one and a half places.

abap2UI5 asks very little of the release it runs on, and that is not a design
achievement so much as a consequence of what the framework does. It produces
two strings and moves them over HTTP. There is no CDS artifact, no RAP object,
no OData service in the middle, so there is almost nothing a given release has
to support.

Almost. The one genuine SAP dependency is GUID creation, and it is written to
satisfy both language versions at once — ABAP Cloud and Standard ABAP, from a
single code line. That is the entire list.

The rest is arithmetic. New ABAP syntax is written normally in the main
repository, and abaplint generates the low-syntax branch automatically, down to
7.02. Hand-downporting would produce a second codebase that drifts and that
nobody can debug; a generated one cannot drift, because it is regenerated.

And the frontend does not age with the backend. UI5 is bootstrapped from a CDN,
so the UI5 version is a configuration decision rather than a property of the
SAP release. A control released this year works on a system that has never
heard of this year.

![Where UI5 comes from decides whether its version follows the SAP release.](/insights/27-bootstrap.svg)

*Where UI5 comes from decides whether its version follows the SAP release.*

What that buys is portability in both directions. An app written on ABAP Cloud
runs on the older system. An app written on 7.02 runs on BTP ABAP Environment.
The screen built for the system being replaced is not thrown away with it —
which, for anybody in the middle of a conversion project, is the interesting
sentence in this article.

A framework that needs nothing from the release does not have to be ported
when the release changes.
