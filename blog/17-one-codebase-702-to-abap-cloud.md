# One Codebase, 7.02 to ABAP Cloud

*abap2UI5 Know-How #17 — draft*

Most SAP landscapes are currently two landscapes. Something old that runs the
business, something new that is being built beside it, and a migration between
them measured in years rather than sprints. Anything written for one side is
usually written again for the other.

abap2UI5 asks very little of the release it runs on, and that is not an
accident of design — it follows from what the framework actually does. It
produces two strings and moves them over HTTP. There is no CDS artefact, no RAP
object, no OData service in the middle, so there is almost nothing that a given
release has to support.

Almost. The one genuine SAP dependency is GUID creation, and it is written to
satisfy both language versions at once — ABAP Cloud and Standard ABAP, from a
single code line.

The rest is arithmetic. New ABAP syntax is written normally in the main
repository, and abaplint generates the low-syntax branch automatically, down to
7.02. Hand-downporting would produce a second codebase that drifts and that
nobody can debug; a generated one cannot drift, because it is regenerated.

And the frontend does not age with the backend. UI5 is bootstrapped from a CDN,
so the UI5 version is a configuration decision rather than a property of the
SAP release. A current UI5 control works on a system that has never heard of it.

What that buys is portability in both directions. An app written on ABAP Cloud
runs on the older system. An app written on 7.02 runs on BTP ABAP Environment.
The screen built for the system being replaced is not thrown away with it.

**A framework that needs nothing from the release does not have to be ported
when the release changes.**

Happy ABAPing! 🦖🦕🦣

---

## LinkedIn teaser post

Plain text — LinkedIn renders no markdown.

> Most SAP landscapes are two landscapes right now: something old running the
> business, something new beside it, and a migration measured in years. Anything
> written for one side usually gets written again for the other.
>
> abap2UI5 needs almost nothing from the release, because it only produces two
> strings and moves them over HTTP. The one real SAP dependency is GUID
> creation, written for both language versions in a single code line — and
> abaplint generates the 7.02 branch automatically, so the downport cannot drift.
>
> UI5 comes from a CDN, so the UI5 version is configuration, not a property of
> the release.
>
> New article 🎉
>
> How much of what you build today will survive your next release upgrade?
>
> #ABAP #SAP #UI5
