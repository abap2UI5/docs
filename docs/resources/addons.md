---
outline: [2, 4]
description: The optional add-ons around abap2UI5 - popups, HTTP and RFC connectors, a lock manager, table maintenance, launchpad KPIs - and the open-source projects it builds on.
---
# Add-ons

abap2UI5 focuses on core capabilities — rendering views, handling events, and exchanging data. This keeps the framework lean and easy to maintain. The real strength lies in the ecosystem: **ready-to-use add-ons and community projects** that extend abap2UI5 for specific needs.

## Available Add-ons

Fifteen of them, in the [abap2UI5-addons](https://github.com/abap2UI5-addons)
organization. Each installs with abapGit like the framework does, and each is
versioned on its own rather than riding along with a framework release.

| Add-on | What it is | Covered here |
| --- | --- | --- |
| [popups](https://github.com/abap2UI5-addons/popups) | Value help, search help, transport selection — and what the framework's built-in popups [moved to](/resources/deprecations#built-in-popups-%E2%86%92-popups-add-on) | [Popup](/cookbook/popup_popover/popup), [Value Help](/cookbook/expert_more/value_help) |
| [selection-screen](https://github.com/abap2UI5-addons/selection-screen) | Selection screens and variants | |
| [layout-management](https://github.com/abap2UI5-addons/layout-management) | Customizable table and form layouts with persistent variant management | |
| [table-maintenance](https://github.com/abap2UI5-addons/table-maintenance) | Table maintenance in your browser | |
| [table-content-loader](https://github.com/abap2UI5-addons/table-content-loader) | Upload and download table content | |
| [se16n](https://github.com/abap2UI5-addons/se16n) | Cloudy SE16N | |
| [sql-console](https://github.com/abap2UI5-addons/sql-console) | Run SQL commands in your browser | |
| [lock-manager](https://github.com/abap2UI5-addons/lock-manager) | The lock logic as a reusable class — stale-lock cleanup and a "locked by X since…" message included | [Lock](/cookbook/expert_more/lock) |
| [config-management](https://github.com/abap2UI5-addons/config-management) | Runtime customization of UI5 application settings | |
| [custom-controls](https://github.com/abap2UI5-addons/custom-controls) | Custom controls for abap2UI5, delivered in their own BSP | [Custom Controls](/advanced/extensibility/custom_control) |
| [custom-controls-customer](https://github.com/abap2UI5-addons/custom-controls-customer) | Your own UI5 artifacts — icon fonts, CSS and custom controls — without touching the framework | [Frontend](/advanced/extensibility/frontend) |
| [http-connector](https://github.com/abap2UI5-addons/http-connector) | Call abap2UI5 apps remotely over HTTP | [HTTP Connector](/advanced/http) |
| [rfc-connector](https://github.com/abap2UI5-addons/rfc-connector) | Call abap2UI5 apps remotely over RFC | [RFC Connector](/advanced/rfc) |
| [launchpad-kpi](https://github.com/abap2UI5-addons/launchpad-kpi) | Show KPIs of abap2UI5 apps on the Fiori Launchpad | [Fiori Launchpad](/configuration/launchpad) |
| [rap-ext](https://github.com/abap2UI5-addons/rap-ext) | Display RAP and CDS artifacts with abap2UI5 | [RAP](/cookbook/eml_cds_sql/rap) |

An empty right-hand column is not a gap in the add-on — it is one this manual
has not written a chapter about. The repository's own README is the reference
in that case.

## Open Source Projects

Other open-source projects use abap2UI5 — try them out:

|  Repository | Description |
| ------------- | ------------- |
| [The Quest](https://github.com/nomssi/axage)  | A wizard's adventure game built on the AXAGE game engine |
| [Advent of Code](https://github.com/joltdx/abap-advent-2023-template) | Template for the Advent of Code 2023 in ABAP  |
| [Generic DDIC Searchhelp](https://github.com/axelmohnen/a2UI5-generic_search_hlp) | Generic DDIC searchhelp builder  |
| [sql-console](https://github.com/abap2UI5-apps/sql-console) | Run SQL commands in your browser  |
| [table-content-loader](https://github.com/abap2UI5-apps/table-content-loader) | Upload & download table content  |
| [table-maintenance](https://github.com/abap2UI5-addons/table-maintenance) | Table maintenance in your browser |

## Contribution

Built a feature or your own open-source project with abap2UI5? Contribute to existing repositories or start your own. Add your project here so others can find, use, and contribute to your work.
