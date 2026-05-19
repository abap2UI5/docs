---
outline: [2, 4]
---
# HTTP Connector

The [abap2UI5 HTTP Connector](https://github.com/abap2UI5-addons/http-connector) calls abap2UI5 apps remotely over HTTP between two ABAP systems. It works similarly to the [RFC Connector](./rfc.md), but uses HTTP instead of RFC as the communication channel.

#### When to use
The HTTP Connector enables several useful cases:
- Frontend/Backend Split: abap2UI5 apps live in the backend, but users access them through a separate frontend system
- Legacy Systems (e.g., ECC): downport abap2UI5 to older systems and call it over HTTP from a newer system
- Multi-System Landscapes: one entry point reaches abap2UI5 apps spread across multiple systems
- Cross-Network Scenarios: HTTP is often easier to route through firewalls, reverse proxies, and gateways than RFC

#### Architecture

The system that calls the app (the frontend system) ships with the UI5 library and bootstraps the UI. The system that runs the app (the backend system) doesn't need to host the UI5 runtime — it only exposes its abap2UI5 endpoint over HTTP.

#### Installation

_Prerequisite: Set up an SM59 HTTP destination pointing to the source system. Install abap2UI5 on both systems._

Steps:
1. Install the [abap2UI5 HTTP Connector](https://github.com/abap2UI5-addons/http-connector) via abapGit on both systems
2. In the HTTP handler, configure the destination to point to your source system
3. In your browser, call the endpoint of the HTTP Connector

#### Further Information
For the latest details, source code, and updates, see the [HTTP Connector repository on GitHub](https://github.com/abap2UI5-addons/http-connector).
