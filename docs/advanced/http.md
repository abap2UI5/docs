---
outline: [2, 4]
description: The HTTP Connector add-on - a consumer system forwards every abap2UI5 roundtrip through an SM59 HTTP destination to the source system that runs the apps; installation, the two ICF nodes, limitations and who authenticates whom.
---
# HTTP Connector

The [HTTP Connector](https://github.com/abap2UI5-addons/http-connector) calls
abap2UI5 apps on one ABAP system from the browser of another, like the
[RFC Connector](/advanced/rfc) but over an HTTP destination: the browser
talks to a **consumer system**, which forwards every request through an SM59
destination to the **source system** where the apps run.

## When to use

- Frontend/backend split: the apps live on a system the users cannot reach
  directly; the consumer system is the one entry point they have.
- Older releases: abap2UI5 [downported](/advanced/downporting) to an older
  system, called from a newer one.
- Networks where HTTP is routed — through a reverse proxy, a gateway or a
  firewall rule — and RFC is not.

## Architecture

The consumer system receives the browser request, forwards method, body and
query string over HTTP to the source system and returns the body and HTTP
status of the response. The consumer system serves the UI5 bootstrap; the
source system exposes one ordinary abap2UI5 endpoint.

| System | ICF node | Handler |
| --- | --- | --- |
| Consumer | `/sap/bc/z2ui5_http` | `Z2UI5_CL_HTTP_CON_HANDLER` |
| Source | `/sap/bc/z2ui5_http_srv` | `Z2UI5_CL_HTTP_CON_SERVER` — an ordinary abap2UI5 endpoint calling `z2ui5_cl_ui5_http_handler` |

## Installation

Requirements, from the add-on's README:

- Standard ABAP 7.50 or higher on both systems.
- abap2UI5 on both systems: on the **consumer** a release newer than
  1.146.0 (the connector forwards the query string through a utility that
  learned it after that release), on the **source** 1.143.0 or newer (the
  first release with `z2ui5_cl_ui5_http_handler`).
- On the consumer system an SM59 destination of type G or H to the source
  system, with path prefix `/sap/bc/z2ui5_http_srv/` and logon data
  maintained.

Steps, with abapGit:

1. Install abap2UI5 on both systems.
2. Install the connector repository, branch `standard`, on **both** systems.
   (The `cloud` branch is an unmaintained prototype from 2025-11, consumer
   side only, without CI.)
3. On the consumer system, replace the destination `NONE` in the handler class
   `Z2UI5_CL_HTTP_CON_HANDLER` with your destination, or maintain the URL
   constant instead.
4. Activate the ICF nodes in transaction SICF: `/sap/bc/z2ui5_http` on the
   **consumer** system, `/sap/bc/z2ui5_http_srv` on the **source** system.

Then open `.../sap/bc/z2ui5_http` of the **consumer system** in the browser.

## Who authenticates whom

The browser authenticates against the consumer system's ICF node
`/sap/bc/z2ui5_http`, like on any abap2UI5 installation. The source system's
node `/sap/bc/z2ui5_http_srv` is called by the consumer system, and it
authenticates whatever the SM59 destination carries: with logon data
maintained on the destination — the shape the README names — every app on
the source system runs under that destination user, whoever is in front of
the browser. The README does not describe user propagation (an assertion
ticket or a certificate for the current user, for instance), so whether the
browser user reaches the source system is an open question of your
destination setup: check in SM59 which logon the destination uses, and check
what `sy-uname` holds in an app on the source system. Authorization checks
in the apps run on the source system against that user. The source node is
an abap2UI5 endpoint in its own right — secure it like the one on the
consumer system, see [Security](/configuration/security).

Headers are not forwarded, and neither is the consumer's path: a request
header an app reads through the user exit on the source system does not
arrive, and the exit sees the path of the source node.

## Limitations

- **Stateless apps only.** A stateful app (`client->set_session_stateful( )`)
  needs the ICF session of the system it runs on; the consumer opens a new
  HTTP connection per request and does not forward the `sap-contextid` header
  the frontend uses to address that session. Apps that keep their state in
  the draft table — the default — work unchanged.
- **Method, body, query string and status travel — headers and the consumer's
  path do not.**

## Next Steps

- [RFC Connector](/advanced/rfc) — the same approach over an RFC destination
- [Security](/configuration/security) — what to check on both ICF nodes
