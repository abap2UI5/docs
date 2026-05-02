---
outline: [2, 3]
---
# FAQ

Recurring questions, with pointers to where the full answer lives in the docs.

## Getting Started

### Do I need to know JavaScript?

No. abap2UI5 apps are pure ABAP classes — view, events, and logic are all written in ABAP. You only touch JavaScript when you build [Custom Controls](/advanced/extensibility/custom_control) or use the [Custom JS](/advanced/extensibility/custom_js) escape hatch.

### Does it work without a SAP system?

The samples run [in your browser](https://abap2ui5.github.io/web-abap2ui5-samples/) without any backend, powered by [open-abap](/technical/tools/open_abap). For your own apps, you need an ABAP system to host the framework and the HTTP service.

### Which ABAP releases are supported?

ABAP 7.02 through ABAP Cloud:

- **S/4 Public Cloud**, **BTP ABAP Environment** (ABAP for Cloud)
- **S/4 Private Cloud**, **S/4 On-Premise** (ABAP for Cloud, Standard ABAP)
- **R/3 NetWeaver AS ABAP 7.02+** (Standard ABAP, downported version)

See the [downport page](/advanced/downporting) for older releases.

### How long does setup take?

About 10 minutes: install via abapGit, create one HTTP handler class, register it in `SICF`. Full walkthrough in the [Quickstart](/get_started/quickstart).

## Architecture and Comparisons

### How is this different from RAP?

RAP defines apps via CDS Views, Behavior Definitions, and OData services; the UI comes from Fiori Elements. abap2UI5 ships the View and Model from a single ABAP class on every roundtrip — no OData, no annotations, full control over the UI from the backend.

→ Detailed comparison: [RAP vs. abap2UI5](/technical/technology/rap)

### How is this different from UI5 freestyle?

UI5 freestyle apps live as deployed frontend artifacts (JS, XML, controllers). abap2UI5 apps are ABAP classes — there's no frontend project to deploy. The UI5 shell is shared and stays static; the backend defines everything else.

→ Detailed comparison: [UI5 Freestyle vs. abap2UI5](/technical/technology/ui5)

### Is it really cloud-ready?

The framework itself is fully cloud-ready and uses only released APIs. **Your apps** become cloud-ready when they also stick to released APIs. See [Cloud Readiness](/technical/cloud) for examples and pitfalls.

### Why no OData?

A single generic HTTP handler serves every app, with the View and Model shipped as strings. This drops the need to design and deploy an OData service per app — and makes runtime model changes (like SE16-style dynamic tables) trivial. The [architecture deep dive](/technical/deep_dive/architecture) covers the trade-offs.

## Production

### Can I use abap2UI5 in production?

Yes. Treat it like any other UI5 app or ABAP program — include the framework and your apps in transports and ship.

→ [Productive Usage](/configuration/productive_usage), [Production Checklist](/configuration/checklist)

### Do I need a UI5 license?

abap2UI5 apps **are** UI5 freestyle apps under the hood. License them the same way you license other UI5 apps at your organization. abap2UI5 itself is MIT-licensed.

→ [Licensing](/get_started/about#licensing)

### Is it fast?

A single HTTP roundtrip carries both the view and the data, so there's no separate metadata or per-entity OData call. For very large views, see the [Performance](/configuration/performance) page for tuning options.

### Can I run it offline?

No. Every user interaction goes back to the server to decide what happens next. Offline use cases are explicitly out of scope — see [Trade-offs](/technical/deep_dive/tradeoffs#downsides-vs-ui5-and-rap).

### Can I embed apps in a Fiori Launchpad?

Yes — on S/4 On-Premise, S/4 Public Cloud, and BTP Work Zone.

→ [Fiori Launchpad](/configuration/launchpad), [BTP Work Zone](/configuration/btp), [S/4 Public Cloud](/configuration/s4_public_cloud)

## Customization and Extensibility

### Can I run abap2UI5 under my own namespace?

Yes — `abaplint --rename` rewrites all artifacts. Up to 9-character namespaces are supported.

→ [Renaming](/advanced/renaming), [Builder](/advanced/builds)

### Can I add my own JavaScript or custom UI5 controls?

Yes. The framework forwards JS and HTML you provide as-is. See [Custom JS](/advanced/extensibility/custom_js), [Custom Controls](/advanced/extensibility/custom_control), and the architectural background in [Sending JavaScript Over the Wire](/technical/deep_dive/lifecycle#sending-javascript-over-the-wire).

### Can I lock business objects across user actions?

The default is stateless, so locks would be re-acquired each roundtrip. Two alternatives: **stateful sessions** (private cloud / on-premise only) or **infinite transactions**. Trade-off table on the [Statefulness, Locks](/advanced/stateful) page.

### Can I call abap2UI5 across systems via RFC?

Yes — the [RFC Connector](/advanced/rfc) wraps the generic HTTP handler so you can ship apps in a backend system and reach them remotely from a frontend system.

## Community

### How do I report a bug or request a feature?

Open an issue on GitHub: [abap2UI5/abap2UI5/issues](https://github.com/abap2UI5/abap2UI5/issues).

### How can I contribute?

Pull requests, sample apps, doc improvements — all welcome. Style guide and review process live on the [Contribution](/resources/contribution) page.

### How do I get support?

Community-driven. Open an issue, join the abap2UI5 Slack, or reach out via the channels listed on the [Support](/resources/support) page.
