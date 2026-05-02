---
outline: [2, 3]
---
# Glossary

Short reference for the terms used across the technical pages. Listed alphabetically.

## ABAP Cloud

A restricted ABAP language version designed for cloud readiness and upgrade stability. Code that follows ABAP Cloud rules runs on SAP BTP ABAP Environment and SAP S/4HANA Public Cloud ABAP Environment. Only released APIs are allowed; classic UI technologies (Web Dynpro, ALV, GUI CFW) and direct database access are excluded. See [Cloud Readiness](/technical/cloud).

## abapGit

Open-source Git client for ABAP. abap2UI5 distributes the framework and apps as abapGit projects so installation is just an abapGit pull — no separate frontend deployment. See the [abapGit page](/technical/tools/abapgit).

## abaplint

Static code analyzer for ABAP. abap2UI5 uses abaplint for code-quality checks **and** for the automatic downport from the main branch (NW 7.50+) to the `702` branch (NW 7.02+). See [abaplint](/technical/tools/abaplint).

## ajson

Open-source ABAP JSON library used by abap2UI5 for all frontend/backend communication. Replaces SAP's `/UI2/CL_JSON` to stay release-independent. See [ajson](/technical/tools/ajson).

## CDS View

Core Data Services view — declarative ABAP-side query definition with optional UI annotations. The backbone of RAP and Fiori Elements. abap2UI5 can SELECT from CDS Views but doesn't depend on them.

## Clean Core

SAP's principle of keeping customer extensions out of the SAP standard objects. Both abap2UI5 and RAP support clean-core extension by relying only on released APIs.

## Draft

A persistence mechanism for interim user state. RAP uses typed draft tables per business object. abap2UI5 uses a single generic draft table (`z2ui5_t_draft`) and serializes the app instance via `if_serializable_object`. See [Inside an App → Drafts](/technical/deep_dive/lifecycle#drafts-stateful-feel-stateless-backend).

## Fiori Elements

UI5 application framework that renders apps from CDS annotations and predefined floorplans (List Report, Object Page, Analytical, …). Part of the RAP stack. See [RAP vs. abap2UI5](/technical/technology/rap).

## Generic HTTP Handler

The single ABAP class that serves every abap2UI5 app. Two strings flow through it: the XML View and the JSON View Model. The handler does not know which app is running — that lives in the user's `z2ui5_if_app` class. See [The abap2UI5 Architecture → The Generic HTTP Handler](/technical/deep_dive/architecture#the-generic-http-handler).

## HDA — Hypermedia-Driven Application

An app where the browser only renders HTML/CSS/JS and never holds the application state. All routing decisions live on the server. abap2UI5's frontend shell behaves as an HDA. See [Foundations → Hypermedia-Driven Apps](/technical/deep_dive/foundations#hypermedia-driven-apps).

## HTML Over the Wire

Server-centric web architecture where the server builds HTML fragments and ships them to the browser, which inserts them into the DOM. The pattern that inspired abap2UI5. Frameworks: htmx, Hotwire, Phoenix LiveView, Laravel Livewire. See [Foundations → HTML Over the Wire](/technical/deep_dive/foundations#html-over-the-wire).

## MPA — Multi-Page Application

Traditional web app where every interaction reloads a full page from the server. ITS Mobile and SAP GUI for HTML are SAP examples.

## OData

REST-based data access protocol used by SAP for client-server communication in UI5 freestyle and RAP. abap2UI5 deliberately does **not** use OData — it sends an XML View and a JSON View Model directly.

## one-code-line

The convention that abap2UI5 keeps a single codebase compatible with both ABAP Cloud and Standard ABAP. abaplint then produces the downport branch automatically. See [Trade-offs → One Code Line](/technical/deep_dive/tradeoffs#one-code-line).

## PAI / PBO

_Process After Input / Process Before Output._ Classic SAP GUI screen lifecycle. abap2UI5's roundtrip model intentionally feels similar — events come in, the backend decides what happens next.

## RAP

_RESTful Application Programming Model._ SAP's standardized framework for cloud-native ABAP applications, built on CDS Views, Behavior Definitions, OData V4, and Fiori Elements UIs. See [RAP vs. abap2UI5](/technical/technology/rap).

## RTTI

_Run Time Type Information / Runtime Type Identification Services._ ABAP API for inspecting and creating types at runtime. abap2UI5 uses RTTI to ship dynamic data models to the frontend (e.g., SE16-style table apps where columns aren't known at design time). See [Defining the Model at Runtime](/technical/deep_dive/architecture#defining-the-model-at-runtime).

## S-RTTI

Open-source extension of standard RTTI. abap2UI5 uses S-RTTI to serialize data references whose types are created at runtime — the standard SAP transformation can't handle that case. See [S-RTTI](/technical/tools/srtti).

## SPA — Single Page Application

Web app where all UI logic lives in JavaScript on the client and the backend only serves data (typically via OData/REST/JSON). Standard UI5 freestyle and RAP/Fiori Elements both follow this model.

## SSR — Server-Side Rendering

Web architecture where the server builds the full HTML document for every page request. Used by ITS, BSP, and Web Dynpro. See [Technology Overview](/technical/technology/overview).

## Stateful Session

A backend session that survives across user interactions, holding work-process state (locks, open cursors, in-memory data). Available for abap2UI5 on private cloud and on-premise systems but not on Public Cloud or BTP. See [Statefulness, Locks](/advanced/stateful).

## UI5 Freestyle

UI5 application built without Fiori Elements — full control over views, controllers, and frontend logic. The browser holds the entire app and fetches data from the backend (typically via OData). See [UI5 Freestyle vs. abap2UI5](/technical/technology/ui5).

## UI5 Over the Wire

The pattern abap2UI5 implements: backend ships the XML View **and** the View Model on every roundtrip; the UI5 frontend shell only renders. The UI5 equivalent of HTML Over the Wire. See [The abap2UI5 Architecture](/technical/deep_dive/architecture).

## XML View

UI5's declarative view definition format. In a normal UI5 app, XML Views live in the frontend project. In abap2UI5, they're built in ABAP via `z2ui5_cl_xml_view` and shipped to the browser on every roundtrip.

## z2ui5_if_app

The single ABAP interface every abap2UI5 app implements. One method (`main`) with one parameter (`client TYPE REF TO z2ui5_if_client`). All app logic lives behind this interface. See [Inside an App](/technical/deep_dive/lifecycle).
