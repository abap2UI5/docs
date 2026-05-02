---
outline: [2, 3]
---
# RFC Connector

The [abap2UI5 RFC Connector](https://github.com/abap2UI5-addons/rfc-connector) calls abap2UI5 apps remotely over RFC between two ABAP systems. This is handy in distributed system landscapes or with legacy systems that can't directly host UI5 frontend logic.

## When to use
The RFC Connector enables several useful cases:
- Frontend/Backend Split: abap2UI5 apps live in the backend, but users can only access the frontend server
- Legacy Systems (e.g., ECC): downport abap2UI5 to older systems and call it from a newer system over RFC
- Multi-System Landscapes: one entry point reaches abap2UI5 apps spread across multiple systems

## Architecture

The system that calls the app (the frontend system) ships with the UI5 library and bootstraps the UI. The system that runs the app (the backend system) doesn't need to host the UI5 runtime.

<img width="600" alt="abap2UI5 RFC Connector architecture across frontend and backend systems" src="https://github.com/user-attachments/assets/6f885f04-2b70-4cb3-83d0-123473b72262" />

## Installation

_Prerequisite: Set up an SM59 destination to call the source system over RFC. Install abap2UI5 on both systems._

Steps:
1. Install the [abap2UI5 RFC Connector](https://github.com/abap2UI5-addons/rfc-connector) via abapGit on both systems
2. In the HTTP handler, replace the destination `NONE` with your Source System Destination
3. In your browser, call the endpoint `.../sap/bc/2ui5_rfc`

## UI5 Bootstrapping
The backend system that holds the abap2UI5 apps needs no UI5 libraries. The calling system bootstraps them automatically:

<img width="600" alt="UI5 bootstrap loaded from calling system while backend hosts the app logic" src="https://github.com/user-attachments/assets/8e7dd3dd-abd3-413f-8ec6-8a7a2be7a7ec" />

## Multi-System Launchpad
You can also use this approach to build a unified Fiori Launchpad that combines:
- Apps installed on-stack (on the current system)
- Apps called remotely over RFC from other systems

<img width="700" alt="Centralized Fiori Launchpad combining on-stack and RFC-triggered remote apps" src="https://github.com/user-attachments/assets/a010440d-bd82-46f9-b597-c3ad2273dbde" />
