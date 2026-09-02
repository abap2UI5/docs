# #29 When the API Is Not Released

SAP grades an extension, and since August 2025 it does so on four **clean core
levels**:

| Level | Language version | What the extension uses | Clean core |
|---|---|---|---|
| **A** | ABAP for Cloud Development | released APIs only | yes, and upgrade-stable |
| **B** | Standard ABAP | classic APIs and the frameworks SAP recommends for them: BAdIs, user exits, ALV, SAP GUI | yes, with less upgrade stability |
| **C** | Standard ABAP | SAP-internal objects, neither released nor classified | conditionally — re-check before every upgrade |
| **D** | Standard ABAP | modifications, implicit enhancements, objects SAP marks as no API | no |

They replaced the three-tier model, and nothing written under the tiers has to
be migrated: tier 1 is Level A, tier 2 was never a layer of its own — its
wrappers still exist and an app calling one stays Level A — and tier 3 is split
into B, C and D by what it touches. The ABAP Test Cockpit and the Cloudification
Repository Viewer say which level an object lands on.

abap2UI5 is Level A. It is written in ABAP for Cloud Development and calls
released APIs only.

That says nothing about the app. Which level an app reaches is decided by what
the app calls, and a screen built on a released API is Level A whether it
renders through abap2UI5 or anything else.

Which leaves the case that actually comes up: the API you need is not released.

The move is a wrapper. A class in Standard ABAP that calls the classic API and
is itself released for ABAP for Cloud Development. It is graded on its own — B,
as long as it stays with classic APIs — and the app calling it stays A.

![The app stays in ABAP for Cloud Development and reaches a classic API through a wrapper written in Standard ABAP](/advanced/use_cases/on_stack_wrapper.svg){ width=90% }

The value of that is not the grade. It is that the part of the system which is
not upgrade-stable has a name, a size and a boundary. One class to re-check when
SAP changes the classic API underneath it, instead of a search through every app
that ever touched it.

On S/4HANA Private Cloud and on-premise the app itself may also be Standard
ABAP, and on releases that do not know ABAP for Cloud Development yet that is
the only option. The framework stays Level A either way, so moving an app up a
level later means changing what it calls, not how it renders:

![Apps written in Standard ABAP call classic APIs or SAP-internal objects, while abap2UI5 itself stays Level A](/advanced/use_cases/on_stack_level_b.svg){ width=90% }

**A wrapper does not make the dependency clean. It makes it findable.**

Happy ABAPing! 🦖🦕🦣

*The levels are SAP's, from the
[Clean Core Extensibility white paper](https://community.sap.com/t5/technology-blog-posts-by-sap/why-clean-core-matters-get-some-insights-into-our-brand-new-extensibility/ba-p/14163750)
of August 2025; the
[ABAP Extensibility Guide](https://community.sap.com/t5/technology-blog-posts-by-sap/abap-extensibility-guide-clean-core-for-sap-s-4hana-cloud-august-2025/ba-p/14175399)
carries the technical rules behind each one, and
[Clean Core maturity and the new extensibility levels](https://community.sap.com/t5/technology-blog-posts-by-sap/clean-core-maturity-and-the-new-extensibility-levels/ba-p/14293974)
covers grading code that already exists.*
