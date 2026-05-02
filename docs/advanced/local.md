---
outline: [2, 3]
---
# Local

`abap2UI5-local` is a special build that bundles all framework artifacts into a single HTTP handler class. With one additional database table for session storage, you can run abap2UI5 self-contained — no separate framework installation required.

For full sources and the latest build, see the [abap2UI5-local repository](https://github.com/abap2UI5/abap2UI5-local).

## When to Use

- **Restricted systems** — environments where you cannot install full abap2UI5 via abapGit but can still create a class and an HTTP handler.
- **Demos and PoCs** — drop a single class into a sandbox system and have a working UI5 app within minutes.
- **Distribution** — ship a self-contained app that does not depend on an existing abap2UI5 installation on the target system.

## When Not to Use

- **Productive multi-app setups** — for systems running several abap2UI5 apps, the standard installation is leaner: one shared framework instead of a copy embedded into every handler.
- **Frequent framework updates** — `abap2UI5-local` is a generated build. Updating means replacing the bundled class instead of pulling abapGit changes.

## How It Works

The `abap2UI5-local` build is produced by [abapmerge](/technical/tools/abapmerge): all framework classes are merged into one class. You install that class plus a database table for session storage, then point an HTTP service at it the same way as in the [Quickstart](/get_started/quickstart).

The resulting app behaves like any other abap2UI5 app — same APIs, same view DSL, same event flow. The difference is purely in how the framework reaches the system.

## See Also

- **[abapmerge](/technical/tools/abapmerge)** — the tool that produces the local build.
- **[Builder](/advanced/builds)** — for full custom builds with namespace + add-ons.
- **[Trade-offs → System Footprint](/technical/deep_dive/tradeoffs#system-footprint)** — why the framework is small enough to bundle into a single class.
