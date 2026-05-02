---
outline: [2, 3]
---
# The abap2UI5 Architecture

_Part 2 of the architectural deep dive._

The previous part covered the patterns that inspired abap2UI5: HTML Over the Wire, hypermedia-driven apps, and the role of XML Views in UI5. This page shows how those ideas turn into a concrete architecture — one where the backend ships **both** the View and the data, a single generic HTTP service serves every app, and Model and View are defined at runtime.

## UI5 Over the Wire

abap2UI5 introduces a key change: the backend also sends the view. This shifts the frontend's role toward an HDA that shows views and data received from the server.

<img width="600" alt="'UI5 Over the Wire' - ABAP delivers Data & View together" src="/img/9717f500-c0de-4428-a996-11fc131c073c.png" />

_"UI5 Over the Wire" — ABAP delivers Data & View together._

Although the app still depends on frontend HTML rendering, it now fetches everything it needs (view and data) over AJAX from the backend. The UI5 app stays a SPA, but its role shrinks to that of an HDA — responsible only for showing the view and its data:

<img width="600" alt="UI5 app downgraded to an HDA - Displaying Data & View received from the server" src="/img/17a3a301-b698-4704-9cbc-43798c5bd600.png" />

_UI5 app downgraded to an HDA — displaying Data & View received from the server._

This means the frontend app doesn't know what it's showing (whether a table, list, or input), nor does it know which actions come next. The app logic lives entirely on the server, and the first request delivers the frontend app as just a small HDA:

<img width="600" alt="'UI5 Over the Wire' - Server to Client Communication" src="/img/2c9f8dc1-c6d8-4e93-80a2-b50bfc1d5ec1.png" />

_"UI5 Over the Wire" — Server to Client Communication._

The HDA shows the view with its data and sends each event back to the server, which decides the next action and output. This resembles the PAI/PBO process from classic SAP GUI apps:

<img width="600" alt="UI5 vs. 'UI5 Over the Wire' - Communication" src="/img/3b464d0b-19fd-400c-a7e4-3eec893f7724.png" />

_UI5 vs. "UI5 Over the Wire" — Communication._

The framework borrows the AJAX roundtrip pattern from "HTML Over the Wire", but UI5 doesn't allow direct HTML injection. Instead, every response carries a **View** along with its **Data** — a pattern best described as "UI5 View Over the Wire".

## Merging Data and Presentation

A typical "UI5-View Over the Wire" response looks like this:

<img width="600" alt="'UI5 Over the Wire' - Response with View & Data together" src="/img/d52112e6-b9b7-4e7f-ac7f-825c20620240.png" />

_"UI5 Over the Wire" — Response with View & Data together._

But isn't this the same as RAP, just in a different format?

## How RAP Compares

RAP also aims for a "sweet spot" between SPA and MPA. The exact mechanism RAP uses to bring its view and model to the frontend isn't fully documented, but it enriches responses either in the JSON itself or in the metadata of the initial OData request — and CDS Views on the backend define the view and model up front:

<img width="600" alt="RAP - Definition of Views with UI Annotations" src="/img/a79f07ff-594d-422c-b66f-8acf8058c81a.png" />

_RAP — Definition of Views with UI Annotations._

<img width="600" alt="RAP - Definition of Data Models with DDL" src="/img/66b8935f-f23a-4b08-bd1d-6ec79f220499.png" />

_RAP — Definition of Data Models with DDL._

This also yields an architecture with a thin frontend and a strong backend, similar to an HDA. But RAP aims for this in a well-organized, controlled way: every API relies on the OData protocol, UI annotations define the views, DDL defines the data models, local implementations of RAP classes handle model updates, and everything splits into layers managed via a Virtual Data Model.

But when major Model and View changes are needed at runtime, this approach can be too rigid. Model changes with RTTI aren't supported, and extending the view quickly exceeds the scope of backend annotations — needing Fiori Elements apps (which again require extra deployment).

Overall, RAP doesn't mix View, Model, and Logic as boldly as the "Over the Wire" approaches. As an open-source project, abap2UI5 can take the next steps without that organizational ceremony.

## One HTTP Service for All Apps

First, abap2UI5 doesn't define a specific HTTP service for sending the View and Data. Instead, every app uses the **same generic HTTP handler** with two strings (one for the View and one for the Data), dropping the need to build individual OData services with SEGW or CDS. At runtime, the framework converts ABAP variables and tables into a JSON model and sends them as a string to the frontend. JavaScript then parses it back into a JSON model and binds it to the UI5 View:

<img width="600" alt="Data Transfer in abap2UI5 - ABAP variables & tables are automatically synchronized with the UI5-Model" src="/img/163ca12b-fe37-43e8-80b6-a5eaae703d69.png" />

_Data Transfer in abap2UI5 — ABAP variables & tables are automatically synchronized with the UI5 Model._

A second twist: the framework sends **not only the data but also the metadata** (Data Model) with every request. This differs from classic OData communication, where the initial OData request carries the metadata to set up the model up front, and only data moves afterward. With this approach, you can send a different model on every request:

<img width="600" alt="OData vs. UI5 Over the Wire - Model & Data transfer" src="/img/95fe59c3-7e8a-4e21-8690-12de1110779f.png" />

_OData vs. UI5 Over the Wire — Model & Data transfer._

## Defining the Model at Runtime

This lets the framework define models not just at design time, but at runtime too. The user needs no extra work — abap2UI5 handles the entire process in the background on every AJAX request:

<img width="600" alt="abap2UI5 - Dynamic Data Binding & Model Creation" src="/img/e9f1bf8c-6d8c-44ad-ba89-c3648b638335.png" />

_abap2UI5 — Dynamic Data Binding & Model Creation._

In apps, RTTI returns to the toolkit, like ALVs used to do. There is no longer a separate app per model. In the demo below, an abap2UI5 app holds a table whose type is created and changed at runtime (similar to SE16):

![SE16-like runtime table where the data model is generated at runtime via RTTI](/img/20b4a140-7954-45b0-8d0e-8aa1e8a6f1f5.gif)

_Replacing the Model (metadata) at runtime._

## Defining the View at Runtime

The same applies to the view. In RAP, only certain predefined control attributes change at runtime, while CDS artifacts with UI annotations define the view itself up front. In an abap2UI5 app, by contrast, you can swap entire view controls. For example, the app below swaps a table control for a list control and back:

![Swapping a table control for a list control at runtime from the ABAP backend](/img/b6e081e4-2eae-4175-aca8-fc761b145762.gif)

_Replacing the View at runtime._

## View and Model Independent of the HTTP Service

As a result, the View and Model stay independent of the HTTP service, and the framework doesn't ship a predefined static OData service for every app, as RAP does. This greatly cuts the number of backend artifacts:

<img width="600" alt="RAP vs. Model & View decoupled from the (single & generic) HTTP-Service" src="/img/6fb61790-87bc-47fa-855e-83d5292b70f3.png" />

_RAP vs. Model & View decoupled from the (single & generic) HTTP service._

Let's look at the HTTP handler that gives this flexibility.

## The Generic HTTP Handler

All apps and data models share the same generic HTTP handler — set a breakpoint in your app and check the call stack to confirm it.

<img width="600" alt="Call stack of an abap2UI5 app" src="/img/1ce80652-4105-4ee5-84e8-35a87eb47556.png" />

_Call stack of an abap2UI5 app._

Every app implementation is a REST-based HTTP POST implementation that keeps no session between requests.

## REST and Live Editing

This makes abap2UI5 compatible with all mobile use cases and devices, and with RESTful environments like the BTP ABAP Environment and the ABAP Cloud language version. Like an OData implementation, where the app reflects data changes without a restart, the entire app can be edited and its view changed without restarting the frontend app:

![Live editing the ABAP class with the UI updating without reloading the frontend app](/img/c2c1afce-7d72-46a2-b0a7-7725c70bf5f4.gif)

_Developing the ABAP class without restarting the frontend app._

This also brings the advantage shared by all over-the-wire approaches: no more cache busting, since the frontend app stays unchanged throughout the development cycle.

So far, the abap2UI5 frontend app doesn't know the specific app — and the generic HTTP service on the server doesn't know the specific model and view it's sending. So which layer actually decides what happens?

## What's Next

The answer lives in a single ABAP class per app. The next part walks through the lifecycle of an app, how state survives between stateless roundtrips via drafts, and how the framework keeps the `_bind` and `_event` mechanism small.

→ Continue with **[Inside an App](/technical/deep_dive/lifecycle)**.

← Back to **[Foundations](/technical/deep_dive/foundations)**.
