---
outline: [2, 3]
---
# Production Checklist

A linear path from a working development install to a production-ready abap2UI5 setup. Use it as a roadmap — each step links to the dedicated page that covers the details.

## 1. Customize the Default Setup

Start by configuring the framework for your environment: theme, UI5 bootstrap source, app title, and other global settings.

→ [General Setup](/configuration/setup)

## 2. Lock Down Security

Decide how clients reach the HTTP service, which authentication mechanism applies, and how cross-origin requests are handled.

→ [Security](/configuration/security)

## 3. Wire Up Authorization

Map your existing ABAP authorization model onto abap2UI5 apps so end users only see and trigger what they are entitled to.

→ [Authorization](/configuration/authorization)

## 4. Tune Performance

Most apps run fast out of the box. For large views, frequent roundtrips, or constrained networks, the performance guide covers the levers worth pulling.

→ [Performance](/configuration/performance)

## 5. Pin a UI5 Version

Pick a UI5 version that fits your supported browsers and SAP support window — pinning the version avoids surprises when SAP rolls out new releases.

→ [UI5 Versions](/configuration/ui5_versions)

## 6. Plan the Installation Topology

Decide where abap2UI5 lives: directly on S/4 Private Cloud or NetWeaver, on the BTP ABAP Environment, on S/4 Public Cloud, or embedded into a Fiori Launchpad or Work Zone.

→ [Installation](/configuration/installation) — covers [S/4 Public Cloud](/configuration/s4_public_cloud), [Fiori Launchpad](/configuration/launchpad), and [BTP Work Zone](/configuration/btp).

## 7. Productive Usage

Final step before go-live: transports, monitoring, support contacts, and operating concerns that come with running abap2UI5 as part of your application landscape.

→ [Productive Usage](/configuration/productive_usage)

## When Things Go Wrong

If anything breaks during setup or operation, start with the configuration troubleshooting guide. For app-level issues during development, see the [development troubleshooting page](/development/trouble).

→ [Configuration Troubleshooting](/configuration/troubleshooting)
