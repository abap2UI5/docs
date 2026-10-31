---
outline: [2, 4]
description: The RFC Connector add-on - a consumer system forwards every abap2UI5 roundtrip through an SM59 RFC destination to the source system that runs the apps; installation, endpoints, limitations and who authenticates whom.
---
# RFC Connector

The [RFC Connector](https://github.com/abap2UI5-addons/rfc-connector) calls
abap2UI5 apps on one ABAP system from the browser of another: the browser
talks to a **consumer system**, which forwards every request through an SM59
RFC destination to the **source system** where the apps run. The apps stay
where they are and run unchanged.

## When to use

- Frontend/backend split: the apps live on a system the users cannot reach
  directly; the consumer system is the one entry point they have.
- Older releases: abap2UI5 [downported](/advanced/downporting) to an older
  system, called from a newer one over RFC.
- Several systems behind one entry point, for example one launchpad that
  mixes on-stack apps with apps called over RFC.

## Architecture

The consumer system receives the browser request, forwards method, body, path
and query string through the RFC-enabled function module
`Z2UI5_FM_RFC_CONECTOR` to the source system, and returns the body and HTTP
status of the response. The consumer system serves the UI5 bootstrap; the
source system needs neither an ICF node nor the UI5 libraries.

| System | Entry | Object |
| --- | --- | --- |
| Consumer | ICF node `/sap/bc/z2ui5_rfc` | `Z2UI5_CL_RFC_CONNECTOR_HANDLER` |
| Source | RFC-enabled function module | `Z2UI5_FM_RFC_CONECTOR` (function group `Z2UI5_FG_RFC_CONNECTOR`) |
| Both | DDIC structures exchanged over RFC | `Z2UI5_S_HTTP_REQ`, `Z2UI5_S_HTTP_RES`, `Z2UI5_S_HTTP_CONFIG` |

<img width="600" height="179" alt="abap2UI5 RFC Connector architecture across frontend and backend systems" src="https://github.com/user-attachments/assets/6f885f04-2b70-4cb3-83d0-123473b72262" />

## Installation

Requirements, from the add-on's README:

- Standard ABAP 7.50 or higher on both systems.
- abap2UI5 1.143.0 or newer on both systems — the connector calls
  `z2ui5_cl_ui5_http_handler`, which first shipped with that release. URL
  parameters (`?z2ui5-bundle`, `app_start` for the user exit) are forwarded
  only when the consumer runs a release newer than 1.146.0.
- On the consumer system an SM59 destination of type 3 to the source system,
  with logon data maintained.

Steps, with abapGit:

1. Install abap2UI5 on both systems.
2. Install the connector repository (branch `main`) on **both** systems, the
   same version on each: consumer and source exchange the DDIC structures of
   the repository over RFC.
3. On the consumer system, replace the destination `NONE` in the handler class
   `Z2UI5_CL_RFC_CONNECTOR_HANDLER` with your destination.
4. On the consumer system, activate the ICF node `/sap/bc/z2ui5_rfc` in
   transaction SICF. The source system needs no ICF node.

Then open `.../sap/bc/z2ui5_rfc` of the **consumer system** in the browser.
The consumer bootstraps UI5 for the source system's apps:

<img width="600" height="189" alt="UI5 bootstrap loaded from calling system while backend hosts the app logic" src="https://github.com/user-attachments/assets/8e7dd3dd-abd3-413f-8ec6-8a7a2be7a7ec" />

## Who authenticates whom

The browser authenticates against the consumer system's ICF node, like on
any abap2UI5 installation. The source system sees the RFC call, and it
authenticates whatever the SM59 destination carries: with logon data
maintained on the destination — the shape the README names — every app on
the source system runs under that destination user, whoever is in front of
the browser. The README does not describe user propagation (a trusted RFC
destination with the current user, for instance), so whether the browser
user reaches the source system is an open question of your destination
setup: check in SM59 which user the destination logs on with, and check what
`sy-uname` holds in an app on the source system. Authorization checks in the
apps run on the source system against that user.

Headers are not forwarded: a request header an app reads through the user
exit on the source system does not arrive.

## Limitations

- **Stateless apps only.** A stateful app (`client->set_session_stateful( )`)
  needs the ICF session of the system it runs on, and every RFC call gets a
  fresh context on the source system. Apps that keep their state in the draft
  table — the default — work unchanged.
- **Method, body, path, query string and status travel — headers do not.**

## Multi-System Launchpad

The same mechanism builds one Fiori Launchpad over apps installed on-stack and
apps called over RFC from other systems:

<img width="700" height="321" alt="Centralized Fiori Launchpad combining on-stack and RFC-triggered remote apps" src="https://github.com/user-attachments/assets/a010440d-bd82-46f9-b597-c3ad2273dbde" />

## Next Steps

- [HTTP Connector](/advanced/http) — the same approach over an HTTP
  destination
- [Fiori Launchpad](/configuration/launchpad) — the tile that opens an
  abap2UI5 endpoint
