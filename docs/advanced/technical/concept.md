# Key Concept: UI5 Over-the-Wire

This page explains the basic architecture behind abap2UI5.

The core idea is based on a pattern called HTML Over-the-Wire, adapted for the SAP technology stack. It simplifies UI5 development by moving both UI rendering and application logic entirely to the ABAP backend.

#### What is HTML Over-the-Wire?

HTML Over-the-Wire describes a server-centric web architecture in which the user interface is generated on the server and sent to the browser as ready-to-render HTML.

Instead of building and maintaining complex JavaScript frontends, managing APIs, and exchanging JSON, the server handles everything — from business logic to UI generation. The browser simply receives and renders HTML fragments. This approach eliminates the need for client-side MVC frameworks, data transformation layers, and frontend deployment processes [(1)](https://signalvnoise.com/svn3/html-over-the-wire/):

> You can write fast, modern, responsive web applications by generating your HTML on the server, and delivering that directly to the browser. You don’t need JSON as an in-between format. You don’t need client-side MVC frameworks. You don’t need complicated bundling and transpiling pipelines.

> This is what HTML Over The Wire is all about. It’s a celebration of the simplicity in HTML as the format for carrying data and presentation together, whether this is delivered on the first load or with subsequent dynamic updates.

In this architecture the initial request delivers JavaScript and CSS assets, Subsequent interactions trigger AJAX calls to fetch HTML fragments and the browser inserts these fragments into the DOM without reloading the full page [(2)](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763):

<p align="center">
<img width="600" alt="image" src="https://github.com/user-attachments/assets/db393f3a-940d-4bd3-aec0-5523e8d58fa0" />
    <br/>
      HTML "Over the Wire" Lifecycle
</p>

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


#### How Does UI5 work?

UI5 freestyle apps follow the Single Page Application (SPA) model. All UI artifacts are stored on the frontend, while the backend provides data via OData. Both rendering and logic execution occur entirely in the browser.

<p align="center">
<img width="500" alt="image" src="https://github.com/user-attachments/assets/afb6e1bd-b60d-4890-ba47-5edd00da26c7" />
    
UI5 Freestyle - ABAP Stack delivers only Data
</p>

Because UI5 is a client-side rendering framework, the HTML cannot be generated on the backend and sent to the client. The html needs to necessarily generated at the frontend in Javascript.


#### How can we send the View from the backend?

Luckily there is a defining feature of UI5 and how it generates HTML which we can use to verlangern the view generation to the backend. Usually every view relies on a xml definition the so called XML View. The ui5 framework used this xml together with the data from the backend an generates the HTML at the frontend:

<p align="center">
<img width="500" alt="image" src="https://github.com/user-attachments/assets/a9bf8f85-54e7-476d-be87-2cd028334d2d" />
<br/>
  <em>UI5 Freestyle – HTML is generated from XML View and data sent via OData</em>
</p>

abap2UI5 introduces here the first a small shift: what if the server now also delivers the xml view? The frontend becomes a passive display layer for views and data received from the server:

<p align="center">
    <img width="500" alt="image" src="https://github.com/user-attachments/assets/ec1ac3f8-65fb-4155-84f6-1ec61a088c40" />
<br/>
  <em>UI5 Freestyle – HTML is generated from XML View and data sent bot form the server</em>
</p>
Although the frontend still renders HTML, all relevant information (view and data) is obtained from the backend via AJAX. The UI5 application technically remains an SPA but now functions solely as a rendering layer for the server-defined UI and Data.

#### How can we bring the logic to the backend?

We can now easily add some interaction with putting some static code at the frontend which sends events to the backend,  This process resembles the classical PAI/PBO model from SAP GUI applications. The app renders the provided view and data, then returns any triggered events to the backend, which decides what should happen next. The frontend is unaware of the current view (e.g., table, list, input) or the next actions. All logic is handled on the backend. The frontend app is a static UI5 application delivered with the first request:

<p align="center">
<img width="500" alt="image" src="https://github.com/user-attachments/assets/ecd6e798-b6f6-4816-89ca-90f20647eb04" />
</p>

with that we get a static logoi containe at the frontend and the backend gets in abap classes the lgoc hadles. every app becomes an abap class no need for frontend apps. Before we had a lot of fronten daritfats:

<img width="500" alt="image" src="https://github.com/user-attachments/assets/8b5c9b5b-3014-489f-90b4-55222744ba8a" />

Now we just use a single dummy app and can store all view and logic at the backend. 

<img width="500" alt="image" src="https://github.com/user-attachments/assets/79c7c6be-6424-4c33-ab3c-9c7799a74747" />

#### How can we exchange data, amke it editable?

So far we can display data and have a backend driven approach,but how can we get teh data back when someone mackes a change at the frontend, while we still relie on odata, the request would be go into our backen dodata implementtion and not in aour newly abapo classes in the backend which also sends the view. 

abap2UI5 sends a View Model from the bachend. in uui cu acan also vie model. ususally they are used to bound atribute (visible/nuvisibe) and they a reused on json models:

<img width="635" alt="image" src="https://github.com/user-attachments/assets/df92711f-abd1-4bfd-b84a-268bb452503f" />


Here is the second shift: wha if we dont bin odata, we jus bind a josn model which we explicite budk together at the backend and send wiet 

<img width="1047" alt="image" src="https://github.com/user-attachments/assets/461f08c2-0f0f-424e-a7f8-008af3610258" />


we can not consume odata or cds directly in the abap layer and only send the view + view model to the frontend. furthermore we can use make tings editable and send it directly to the backend and exchange data.

the abap2ui5 framework opffers binding ethods and let the attribute via classes, everyhting is handled in the backgroudn and the app developer does not need to pay attentiojto to that,


A typical response in this pattern includes both view and model data:

<img width="400" alt="image" src="https://github.com/user-attachments/assets/d52112e6-b9b7-4e7f-ac7f-825c20620240" />

ac complete piucture looks liek this:

PICPICPICPIC

#### Partial HTML Updates

A central feature of HTML Over the Wire is that only the affected parts of the page are updated, rather than the entire document. Can this be achieved in UI5? While altering the XML view would typically trigger a full re-render, updating only the view model and binding attributes accordingly allows UI5 to update just the relevant UI elements. Consider this example:

```abap
CLASS z2ui5_cl_demo_app_025 DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA text TYPE string.
    DATA partly TYPE abap_bool.

ENDCLASS.

CLASS z2ui5_cl_demo_app_025 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    text = text && ` text`.

    IF client->check_on_init( ) OR partly = abap_false.
      client->view_display( z2ui5_cl_xml_view=>factory(
        )->input( client->_bind( text )
        )->input( submit = client->_event( )
        )->checkbox( selected = client->_bind_edit( partly ) text = `partly` ) ).

    ELSE.
      client->view_model_update( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
```
you can see in comparison, partly vs not:
<p align="center">
  <img src="https://github.com/user-attachments/assets/79a8c531-b9a0-4bf4-bb1c-7d9019ef8707" width="400" />
  <br/>
  <em>You can see the difference: partly vs. not</em>
</p>
All credits go here to the beatigul ui5 framework, it checks automatically for view model updates and only rerenderd the necessary controls. always keep in min to resend the view as selten as pssible but becaue i ttriggers w whole rerender cisotn gperfomrance and a bda ux experience. always try to only use 

```abap
client->view_model_update( ).
```

you also see the that the fcus stays stable giving the user has a great expereince, and done with a couple of lines of abap. this is all wenn need to call it UI5 over-the-wire.

#### Isnt RAo aso also a bakcend driven and similar?

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
