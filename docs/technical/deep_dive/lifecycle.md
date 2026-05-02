---
outline: [2, 3]
---
# Inside an App

_Part 3 of the architectural deep dive._

The previous part showed how a single generic HTTP handler ships every View and Model. That works because all the app-specific intelligence lives in **one place**: an ABAP class implementing `z2ui5_if_app`. This page walks through that class — its lifecycle, how the framework keeps state across stateless roundtrips with drafts, what the initial request looks like, and how the small `_bind` / `_event` API keeps view-building loosely coupled.

## The abap2UI5 App

The only non-generic part of the concept is the user's app, which carries the `z2ui5_if_app` interface:

<img width="600" alt="abap2UI5 app - one place for everything" src="/img/167b1078-14d7-4354-9561-f4e5c7345544.png" />

_abap2UI5 app — one place for everything._

In this architecture, the app has full freedom to build the view and the model, but it also carries full responsibility for making sure everything else works correctly. The app handles program logic, app state, and remembers where it came from and where it wants to go next. All of this lives in this single layer.

This isn't a problem for ABAP. From an ABAP perspective, it resembles older practices like using selection screens or working with ALVs. Every SAP GUI app was effectively an HDA where ABAP handled all the needed functions (just not in a browser environment). In this architecture, you're no longer limited to building an OData service or bound to a local implementation of a global RAP class with restrictions like commit sequences. The full capabilities of the ABAP stack come back. Building data models from internal tables is simple; working with generic data models is easy at runtime with RTTI; and advanced ABAP concepts like serialization apply too — as the next section shows.

## Drafts: Stateful Feel, Stateless Backend

With RAP, users can save interim results in drafts so they can pause and resume work later. The abap2UI5 architecture acts as if it sent a fresh app to the frontend after every event, but the user's previous inputs and state still need to survive. To handle this, the `z2ui5_if_app` interface inherits the `if_serializable_object` interface, which lets the framework serialize and store all key information from every request (like the current view or its status):

<img width="600" alt="z2ui5_t_draft - the abap2UI5 persistence for interim results" src="/img/fc13f32d-3145-4510-a2d8-a0b646fdd6c4.png" />

_`z2ui5_t_draft` — the abap2UI5 persistence for interim results._

Drafts also let you jump back to previous states with little effort, cutting the complexity an HDA usually faces when building cancel or exit events. Like the HTTP service, drafts are generic — there's no need to create typed draft tables for every data model by hand, as RAP does — which further cuts the number of backend artifacts:

<img width="600" alt="RAP vs. Single (generic) Draft Table in abap2UI5" src="/img/c32335ae-6d10-4b12-9fd1-786a0da595fe.png" />

_RAP vs. Single (generic) Draft Table in abap2UI5._

This gives a stateful-like PAI/PBO feel similar to SAP GUI apps, even though every roundtrip is stateless AJAX. And because every request can land on any app server, abap2UI5 fits scalable cloud environments well, ready for future compatibility:

<img width="600" alt="SAP GUI (stateful) vs. abap2UI5 (restful)" src="/img/0c62e222-06b7-4f16-af2d-3663fd6df796.png" />

_SAP GUI (stateful) vs. abap2UI5 (RESTful)._

::: warning Use drafts only for interim results
Drafts are designed for app state during a user's session, not as a general-purpose persistence mechanism. Be careful what you serialize.
:::

## The Initial Request

The first GET request delivers the artifacts of the UI5 (HDA) app to the browser. Normally a BSP would be deployed to the ABAP stack for this, but in abap2UI5 the code lives as a string inside the HTTP handler's initial-request implementation:

<img width="600" alt="index.html stored in ABAP Source Code instead of using a BSP" src="/img/e69d5a12-c0e3-4e17-be8a-4da3bc740c97.png" />

_`index.html` stored in ABAP source code instead of using a BSP._

This gives a fully abapGit-based project that uses only ABAP source code, easy to install on any ABAP system — no separate frontend artifacts or deployments needed.

## Everything Maintained and Built in the Backend

Since all user apps are also in pure ABAP, everything can be maintained and built in the backend. Duplicating, editing, renaming, or any other refactoring takes seconds. Deployment shrinks to activating an ABAP class, so many apps can be built quickly. For example, all the apps in the samples section were built fast, mostly by copy and paste — which wouldn't be realistic for separately built and deployed frontend apps. This is a big drop in complexity and a shared advantage of all "Over the Wire" apps.

## No Extra Layer

Another way to cut complexity: skip the extra customizing layers. Only one stack frame sits between the user's app and the HTTP handler — no OData, SADL, or Gateway involved. This keeps the UI5 frontend framework and its functionality intact in abap2UI5 apps on the backend.

UI5 evolves fast, and extra layers can become outdated quickly. With this approach, all future UI5 controls work in abap2UI5 automatically. The tradeoff: you have to deal with the complexity of the frontend UI5 API and learn the concepts of XML views and UI5 controls. In the end, it's a matter of preference: learn UI annotations, or learn the concepts of SAP UI5 directly.

## No Hiding of Complexity

Having no extra layer also means the framework doesn't always hide complexity — unlike what other frameworks try to do. In abap2UI5, you send your XML view directly to the frontend, and you're responsible for making sure it's valid and executable:

<img width="600" alt="XML-View created by the user and ready for the 'Wire'" src="/img/cbfdc72f-31f1-460b-afa3-d03179e9b173.png" />

_XML view created by the user and ready for the "Wire"._

Utility classes greatly simplify the build process. For example, `z2ui5_cl_xml_view` offers a class-based approach to create views with access to the UI5 API via ADT code completion:

<img width="600" alt="z2ui5_cl_xml_view - UI5 API (frontend) used for Code Completion in ADT (backend)" src="/img/b8aa5f41-d958-4181-bdc3-bc92a4a57b4b.png" />

_`z2ui5_cl_xml_view` — UI5 API (frontend) used for code completion in ADT (backend)._

This contrasts with RAP, where you benefit from well-documented and organized extra layers, though they sometimes have limited functionality. Take side effects, for example: RAP limits you to the `+`, `-`, and `*` operators. In abap2UI5, you write JavaScript directly, which takes more knowledge but in return gives you full expression binding on the frontend:

<img width="600" alt="Expression Binding (Side Effects) in abap2UI5 - Mixture of ABAP and JavaScript" src="/img/c8be7e94-c4e1-445e-b1f4-f79d81d421ac.png" />

_Expression Binding (Side Effects) in abap2UI5 — a mixture of ABAP and JavaScript._

## Separated `_bind` and `_event`

In the earliest version of the framework, every method call carried the event and data binding:

<img width="600" alt="First approach - Data binding and events are not separated from the view" src="/img/3bc268e0-e08f-40b3-b152-b3fa375c0faf.png" />

_First approach — Data binding and events are not separated from the view._

In the current approach, they are separated from the view and created with extra methods. The framework then handles data binding and transfer automatically:

<img width="600" alt="Current Approach - extra methods for the event and binding" src="/img/4708b4c1-a031-48d5-823c-5b8434a98c0c.png" />

_Current approach — extra methods for the event and binding._

This differs from many other UI rendering processes, which usually handle data and UI together. Separating them here makes view building simpler, avoids data duplication, and keeps the framework from becoming tangled. The current approach uses fewer lines of code than the earliest version, because it now cleanly separates the entire view-building process from the rest and keeps it outside the framework.

## Sending JavaScript Over the Wire

You can add extra functionality (JS, HTML, CSS) with no need to extend the framework or change the abap2UI5 frontend app. For example, the Upload Files App has its own custom control that isn't part of the framework and ships "Over the Wire" when the app is called:

<img width="600" alt="App delivering its own JavaScript 'Over the Wire'" src="/img/5960c1c9-1675-440f-80f9-a3e52db31c1c.png" />

_App delivering its own JavaScript "Over the Wire"._

On any request, you can send your own JavaScript or custom controls to the frontend. The abap2UI5 framework forwards them as-is. All later requests can then use this JavaScript — for example, to render custom controls in their UI5 views:

<img width="600" alt="abap2UI5 app sending custom JavaScript to the client" src="/img/66345bd4-6208-4dfc-9870-c82e3a45f74a.png" />

_abap2UI5 app sending custom JavaScript to the client._

## What's Next

So far, the deep dive has focused on what the architecture **enables**. The final part is honest about what it costs: the downsides versus UI5 and RAP, the system footprint, and how the framework manages compatibility from ABAP 7.02 up to ABAP Cloud.

→ Continue with **[Trade-offs and Compatibility](/technical/deep_dive/tradeoffs)**.

← Back to **[The abap2UI5 Architecture](/technical/deep_dive/architecture)**.
