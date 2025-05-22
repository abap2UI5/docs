# Key Concept: UI5 Over-the-Wire?

This page introduces the technical foundation of abap2UI5. 

At its core lies an architectural pattern known as HTML Over-the-Wire, adapted for the SAP technology stack. The key idea is to simplify UI5 application development by shifting both UI rendering and application logic entirely to the ABAP backend.

#### What is HTML Over-the-Wire?

HTML Over-the-Wire describes a server-centric web architecture in which the user interface is generated on the server and sent to the browser as ready-to-render HTML.

Instead of building and maintaining complex JavaScript frontends, managing APIs, and exchanging JSON, the server handles everything — from business logic to UI generation. The browser simply receives and renders HTML fragments. This approach eliminates the need for client-side MVC frameworks, data transformation layers, and frontend deployment processes [(1)](https://signalvnoise.com/svn3/html-over-the-wire/):

> You can write fast, modern, responsive web applications by generating your HTML on the server, and delivering that directly to the browser. You don’t need JSON as an in-between format. You don’t need client-side MVC frameworks. You don’t need complicated bundling and transpiling pipelines.

> This is what HTML Over The Wire is all about. It’s a celebration of the simplicity in HTML as the format for carrying data and presentation together, whether this is delivered on the first load or with subsequent dynamic updates.

In this architecture the initial request delivers JavaScript and CSS assets, Subsequent interactions trigger AJAX calls to fetch HTML fragments and the browser inserts these fragments into the DOM without reloading the full page:

<p align="center">
<img width="400" alt="image" src="https://github.com/user-attachments/assets/a9fde24a-c572-4e5c-b203-59a0667b9931" />
</p>

_HTML "Over the Wire" Lifecycle [(2)](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763)_

This results in a clean and lightweight frontend — a pure rendering layer — while all logic remains under full control of the backend.

Several modern frameworks follow this pattern:
* [htmx](https://htmx.org) Progressive enhancement via HTML partials (Any web stack)
* [Hotwire (Turbo)](https://hotwired.dev) HTML-over-the-wire for Rails apps (Ruby on Rails)
* [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view) Real-time UI with server rendering (Elixir/Phoenix)
* [Livewire](https://livewire.laravel.com) Server-driven UI components in PHP  (Laravel/PHP)
* [Unpoly](https://unpoly.com) Simplified partial page updates (Any web stack)


#### But isn't this just old-school server rendering?

In the early days of web development, Server-Side Rendering (SSR) was the standard. Every user interaction triggered a full-page reload, and the server responded with a complete HTML document. Within the SAP ecosystem, this approach was seen in technologies like ITS Mobile and SAP GUI for HTML.

The emergence of Single-Page Applications (SPAs) shifted the rendering logic to the client. SPAs fetch raw data — through OData services in SAP systems — and dynamically construct the UI in the browser using JavaScript frameworks such as React, Angular, or Vue. SAP adopted this paradigm with the introduction of UI5 in 2010.

However, SPAs come with their own set of challenges: complex API layers, separation of frontend and backend development teams, and intricate build and deployment pipelines.

As a counter-approach, HTML Over-the-Wire reintroduces a server-driven approach to UI updates:
- The server sends only HTML fragments, not entire pages, the browser updates specific parts of the DOM
- The frontend remains simple and declarative, all logic and artifacts reside in the backend

Architectural Comparison:

| Approach      | Data Flow                        | Rendering Location         | Period           |
|---------------|----------------------------------|---------------------------|------------------|
| **SSR**       | Full-page HTML responses         | Entirely on the server    | 1990s – 2010s    |
| **SPA**       | Raw data (JSON), client builds UI| Client-side (JavaScript)  | 2010s – today    |
| **Over-the-Wire** | HTML fragments for partial updates | Server renders, browser inserts | 2020s (re-emerging) |

In short: Over-the-Wire sends only partial HTML updates, while SSR reloads the full page on every interaction.

#### How can we adapt this to UI5?

UI5 freestyle apps follow the SPA model. All rtifacts are stored at the frontend and the backend supplies data through OData, while all rendering and logic execution occur in the browser:

<img width="400" alt="image" src="https://github.com/user-attachments/assets/3b2a884e-e899-4b60-8a95-79b418f33657" />

UI5 Freestyle - ABAP Stack delivers only Data

As we need UI5 for rendering the HTML and it is a client side framework we are limited here, we can not build HTML in the backend and send it from there. But there is a defining feature of UI5 and its use of XML views to generate HTML. These views reside on the frontend and are populated with server JSON data. XML-Views and JSON Darta is used by the UI5 framework to generate the HTML at the frontend.

abap2UI5 introduces here the first a small shift: what if the server now also delivers the xml view? The frontend becomes a passive display layer for views and data received from the server:

<img width="400" alt="image" src="https://github.com/user-attachments/assets/9717f500-c0de-4428-a996-11fc131c073c" />

"UI5 Over the Wire" - ABAP delivers Data & View

Although the frontend still renders HTML, all relevant information (view and data) is obtained from the backend via AJAX. The UI5 application technically remains an SPA but now functions solely as a rendering layer for the server-defined UI and Data:

<img width="400" alt="image" src="https://github.com/user-attachments/assets/17a3a301-b698-4704-9cbc-43798c5bd600" />

UI5 app downgraded - Displaying Data & View received from the server

The frontend is unaware of the current view (e.g., table, list, input) or the next actions. All logic is handled on the backend. The frontend app is a static UI5 application delivered with the first request:

<img width="400" alt="image" src="https://github.com/user-attachments/assets/2c9f8dc1-c6d8-4e93-80a2-b50bfc1d5ec1" />

The app renders the provided view and data, then returns any triggered events to the backend, which decides what should happen next. This process resembles the classical PAI/PBO model from SAP GUI applications:

<img width="400" alt="image" src="https://github.com/user-attachments/assets/3b464d0b-19fd-400c-a7e4-3eec893f7724" />

Communication relies on AJAX roundtrips akin to HTML Over the Wire, but pure HTML cannot be sent since UI5 still requires XML views and JSON models. abap2UI5 leverages UI5's capability to render HTML from these constructs. This results in a model referred to as _UI5-XML-View Over-the-Wire_

_UI5-XML-View and View-Model over-the-wire_:
Here is the second shift, abap2UI5 sends a View Model from the bachend. you dont need odata you can also use view models at the frontend, this is used to send it from the backend.

A typical response in this pattern includes both view and model data:

<img width="400" alt="image" src="https://github.com/user-attachments/assets/d52112e6-b9b7-4e7f-ac7f-825c20620240" />

"UI5 Over the Wire" - Response with View & Data together


#### Partial HTML Updates

A central feature of HTML Over the Wire is that only the affected parts of the page are updated, rather than the entire document. Can this be achieved in UI5? While altering the XML view would typically trigger a full re-render, updating only the view model and binding attributes accordingly allows UI5 to update just the relevant UI elements. Consider this example:

```abap
CLASS z2ui5_cl_app_partly_rerender DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA text TYPE string.

ENDCLASS.

CLASS z2ui5_cl_app_partly_rerender IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
      client->view_display( z2ui5_cl_xml_view=>factory(
        )->input( value = client->_bind( text )
        )->button( text = 'partly rerender html' press = client->_event( 'POST' ) ).
    ELSE.
      text = text && ` text`.
      client->view_model_update( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
```

```abap
client->view_model_update( ).
```
#### What about RAP?

Although RAP [(3)](https://pages.community.sap.com/topics/abap/rap) shifts more logic and development to the backend, it cannot be considered an “over-the-wire” approach:
- RAP focuses on defining data models (CDS), services, and transactional logic in ABAP, exposing them via OData endpoints consumed by the frontend
- It remains a traditional API-driven model, not one based on server-rendered UI. RAP applications treat the frontend as a consumer of data, not as a passive renderer of server-generated views
- The UI is delivered initially as part of an SPA; subsequent changes are limited to annotation-driven updates or small adjustments—not full UI replacements. RAP apps require deployment for its generated SPA [(4)](https://developers.sap.com/mission.sap-fiori-abap-rap100.html)

#### Summary

The core idea behind abap2UI5 is to bring the simplicity and efficiency of the HTML Over-the-Wire paradigm into the SAP ecosystem.

Key Benefits:
- Static UI5 Frontend Application: Delivered with the initial HTTP request; generic and consistent across all use cases
- Backend-Driven UI Control: UI definitions and business logic are implemented entirely in ABAP classes
- ABAP-Centric Development: Eliminates the need for additional JavaScript or dedicated frontend development
- Simplified Deployment Model: No SPA-specific tooling or build processes; application logic and artifacts are maintained via abapGit and standard transport mechanisms
- Seamless SAP Integration: Fully compatible with UI5 and ABAP, supports ERP and S/4, ABAP Standard & ABAP Cloud ready 
- Efficient for Business Applications: Ideal for CRUD operations, forms, dashboards, and all typical enterprise use cases

Limitations:
- Not designed for highly interactive or collaborative real-time applications
- Offline functionality or complex client-side interactions are not covered
- Less effective if frontend and backend teams work independently

By relocating UI control to the ABAP backend and using UI5 purely for HTML rendering, abap2UI5 enables pragmatic, maintainable business application development — without the complexity of SPA architectures. For common enterprise use cases like dashboards, transactional forms, or reporting apps, it offers a clean, backend-driven alternative with shorter development cycles.

**References:**
- [htmx in a nutshell](https://htmx.org/docs/#introduction)
- [HTML Over the Wire](https://signalvnoise.com/svn3/html-over-the-wire/)
- [Fiori-like web app development in pure ABAP with htmx and Fundamental](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763)
