---
outline: [2, 4]
---
# Low-Code Platforms vs. abap2UI5
_Two answers to the same question_

Commercial low-code platforms and abap2UI5 address the same need — modern UIs for SAP systems without building a full frontend stack per app — with fundamentally different models: a visual designer on a licensed platform on one side, plain ABAP code in an open-source framework on the other. This page compares the two models, including the points where a commercial platform is ahead.

## The Two Models

| Aspect | Low-Code Platform | abap2UI5 |
|--------|-------------------|----------|
| **App Definition** | Visual designer, drag & drop, platform-specific format | ABAP classes, written in any ABAP IDE |
| **App Storage** | Platform repository | Your system, like any other ABAP development object |
| **Versioning & Transport** | Platform mechanisms | CTS or abapGit — the tools you already use |
| **Testing** | Platform tooling | ABAP Unit, plus the project's [linter](/advanced/linter) |
| **License** | Commercial, typically priced per end user | MIT, unlimited users, free |
| **Installation** | Platform installation or add-on, plus client components | One abapGit pull ([installation](/configuration/installation)) |
| **Release Coverage** | Defined by the vendor | NW 7.02 to ABAP Cloud ([downporting](/advanced/downporting)) |
| **Exit Path** | Apps exist in the platform format | Apps remain plain ABAP classes in your system |

## Where a Commercial Platform Is Ahead

An honest comparison names both directions. A commercial low-code platform typically offers:

- **Native mobile clients with offline support** — abap2UI5 apps run in the browser and cover [camera, barcode scanning, geolocation and file handling](/cookbook/device_capabilities/info) through web APIs, but offline-first scenarios with local storage and synchronization are out of scope
- **Bundled suites** — workflow engines, API management and portal products in one package; abap2UI5 is deliberately only the UI layer
- **Contractual support** — SLAs, certified trainings and a vendor to hold accountable; abap2UI5 offers [community and partner-based support](/resources/support)
- **Citizen development** — visual tools aimed at non-developers; abap2UI5 is a framework for ABAP developers

## Where abap2UI5 Is Ahead

- **Apps are code.** Every app is a plain ABAP class — diffable, transportable, unit-testable, reviewable in a pull request. There is no second format to govern and no designer artifact that drifts from the system it describes.
- **No per-user economics.** An app that serves ten users costs the same as one that serves ten thousand: nothing. The cost of an app is the time to build it — and nothing recurs per seat, per year.
- **No platform between you and your apps.** The framework is a set of ABAP classes and one HTTP handler in your own system ([security](/configuration/security)). If the project disappeared tomorrow, your apps would keep running from your own repository — the MIT license makes the code permanently yours to fork.
- **Full release range.** From NW 7.02 to ABAP Cloud, on-premise and BTP, with every commit CI-tested across the release lines ([productive usage](/configuration/productive_usage)).
- **AI agents can operate it.** A visual designer needs a human. Code-first development is exactly what AI coding agents do best: with the [MCP server](/advanced/mcp_server) an agent writes an app class, validates the view with the [linter](/advanced/linter), runs the app and checks its own screenshot — without an SAP system. Several hundred ports of official UI5 demo kit samples were generated this way and are guarded by CI.
- **Fiori Launchpad integration** without an intermediate portal product ([launchpad](/configuration/launchpad)).

## Deciding Between the Two

Choose a **commercial low-code platform** when you need offline-capable native mobile apps, a bundled workflow or portal suite, contractual SLAs, or app building by non-developers.

Choose **abap2UI5** when you have ABAP developers (or AI agents assisting them), want apps to live in your system as ordinary code, and prefer paying for development once over licensing per user forever.

The two also coexist: abap2UI5 is not a platform to migrate to, but a framework to adopt one app at a time — the first app costs an abapGit pull and an afternoon, and [trying it in the browser](https://abap2ui5.github.io/web-abap2UI5-build/) costs nothing at all.
