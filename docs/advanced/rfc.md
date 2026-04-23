---
outline: [2, 4]
---
# RFC Connector

The [abap2UI5 RFC Connector](https://github.com/abap2UI5-addons/rfc-connector) lets you trigger abap2UI5 apps remotely via RFC between two ABAP systems. This is particularly useful in distributed system landscapes or when working with legacy systems that can't directly host UI5 frontend logic.

#### When to use
The RFC Connector enables several useful scenarios:
- Frontend/Backend Split: abap2UI5 apps are implemented in the backend, but users only have access to the frontend server
- Legacy Systems (e.g., ECC): abap2UI5 can be downported and installed on older systems and then triggered from a newer system via RFC
- Multi-System Landscapes: abap2UI5 apps distributed across multiple systems can be accessed centrally from a single entry point

#### Architecture

The system that calls the app (the frontend system) includes the UI5 library and bootstraps the UI. The system where the app is implemented (the backend system) doesn't need to host the UI5 runtime.

<img width="600" alt="abap2UI5 RFC Connector architecture across frontend and backend systems" src="https://github.com/user-attachments/assets/6f885f04-2b70-4cb3-83d0-123473b72262" />

#### Installation

_Prerequisite: Set up a destination in SM59 so the source system can be called via RFC. abap2UI5 needs to be installed in both systems._

Steps:
1. Install the [abap2UI5 RFC Connector](https://github.com/abap2UI5-addons/rfc-connector) via abapGit on both systems
2. In the HTTP handler, replace the destination `NONE` with your Source System Destination
3. In your browser, call the endpoint `.../sap/bc/2ui5_rfc`

#### UI5 Bootstrapping
The backend system that holds the abap2UI5 apps doesn't need to include the UI5 libraries. These are automatically bootstrapped from the calling system:

<img width="600" alt="UI5 bootstrap loaded from calling system while backend hosts the app logic" src="https://github.com/user-attachments/assets/8e7dd3dd-abd3-413f-8ec6-8a7a2be7a7ec" />

#### Multi-System Launchpad
You can also use this approach to build a centralized Fiori Launchpad that includes both:
- Apps installed on-stack (on the current system)
- Apps executed remotely via RFC from other systems

<img width="700" alt="Centralized Fiori Launchpad combining on-stack and RFC-triggered remote apps" src="https://github.com/user-attachments/assets/a010440d-bd82-46f9-b597-c3ad2273dbde" />
