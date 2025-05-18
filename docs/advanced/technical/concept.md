# Key Concept: UI5 Over-the-Wire?

This article provides a technical dive into abap2UI5, a framework that simplifies SAP UI5 application development by moving both UI rendering and logic back to the ABAP backend. It introduces the key architectural concept — HTML Over the Wire — and explains how this idea is adapted to the SAP ecosystem.


##### What is HTML Over-the-Wire?

HTML Over the Wire refers to a web architecture where the server renders ready-to-use HTML and sends it directly to the browser. Instead of sending raw data (e.g., JSON) and rendering UIs with JavaScript frameworks on the client, the server handles both the application logic and HTML generation and sending it to the browser — without relying on JSON, client-side MVC frameworks, bundling, or transpiling pipelines.

> You can write fast, modern, responsive web applications by generating your HTML on the server, and delivering that directly to the browser. You don’t need JSON as an in-between format. You don’t need client-side MVC frameworks. You don’t need complicated bundling and transpiling pipelines.

> This is what HTML Over The Wire is all about. It’s a celebration of the simplicity in HTML as the format for carrying data and presentation together, whether this is delivered on the first load or with subsequent dynamic updates.

Unlike Single Page Applications (SPAs), which shift most responsibilities to the browser, HTML Over the Wire maintains control on the backend. UI updates are performed by sending partial HTML fragments over AJAX, which the browser inserts without full page reloads.

<img width="400" alt="image" src="https://github.com/user-attachments/assets/a9fde24a-c572-4e5c-b203-59a0667b9931" />

_HTML "Over the Wire" Lifecycle [(Quelle)](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763)_

#### Where does it come from?

In early web development, **Server-Side Rendering (SSR)** was the default. Every user action triggered a full-page reload with a complete HTML response.

The rise of **Single Page Applications (SPAs)** shifted rendering to the browser. SPAs fetch raw data (often via OData in SAP) and dynamically build UIs with JavaScript frameworks like React, Angular, Vue, or, in the SAP world, UI5.

However, SPAs introduced new challenges:
- API layers and data contracts
- Separate frontend-backend development workflows
- Complex build and deployment pipelines

As a reaction, the **Over-the-Wire** paradigm re-emerged:
- Servers send UI fragments, not full pages
- Browsers update only specific parts of the page
- Frontends stay simple and declarative

Architectural Comparison:

| Approach      | Data Flow                        | Rendering Location         | Period           |
|---------------|----------------------------------|---------------------------|------------------|
| **SSR**       | Full-page HTML responses         | Entirely on the server    | 1990s – 2010s    |
| **SPA**       | Raw data (JSON), client builds UI| Client-side (JavaScript)  | 2010s – today    |
| **Over-the-Wire** | HTML fragments for partial updates | Server renders, browser inserts | 2020s (re-emerging) |

Several frameworks successfully implement the HTML Over-the-Wire approach:

| Framework             | Focus                                | Tech Stack           |
|-----------------------|--------------------------------------|----------------------|
| [htmx](https://htmx.org)                | Progressive enhancement via HTML partials  | Any web stack        |
| [Hotwire (Turbo)](https://hotwired.dev) | HTML-over-the-wire for Rails apps          | Ruby on Rails        |
| [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view) | Real-time UI with server rendering         | Elixir / Phoenix     |
| [Livewire](https://livewire.laravel.com)         | Server-driven UI components in PHP         | Laravel / PHP        |
| [Unpoly](https://unpoly.com)            | Simplified partial page updates            | Any web stack        |


#### How can we adapt this to UI5?

UI5 applications typically follow an SPA architecture. The backend delivers data via OData, while all logic and UI rendering occur on the frontend. But one specific characteristic we should examine closely is how the UI5 framework creates views. Each HTML output is rendered from an XML-View (let's ignore the former HTML/JS/JSON-Views), with its associated data from the server. The view is stored at the frontend as part of the app:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/3b2a884e-e899-4b60-8a95-79b418f33657" />
UI5 normally - ABAP delivers only Data

abap2UI5 introduces a pivotal change: the backend also sends the view. This shifts the frontend’s role towards an HDA, displaying views and data received from the server:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/9717f500-c0de-4428-a996-11fc131c073c" />

"UI5 Over the Wire" - ABAP delivers Data & View together

Despite still relying on frontend HTML rendering, all necessary information (view & data) is now retrieved via AJAX from the backend. As a result, the UI5 app remains a SPA, but its role is now reduced to that of a HDA, which is responsible solely for displaying the view and its data:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/17a3a301-b698-4704-9cbc-43798c5bd600" />

UI5 app downgraded to an HDA - Displaying Data & View received from the server

This means that the frontend app is not aware of what it is currently displaying (whether it's a table, list or input) and neither is it aware of what actions will be taken next. The app logic remains completely on the server and the frontend app is just a small HDA transmitted with the first request:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/2c9f8dc1-c6d8-4e93-80a2-b50bfc1d5ec1" />

"UI5 Over the Wire" - Server to Client Communication

The HDA displays the view with its data and sends back each event to the server for determination of the next action and output. This process is somewhat similar to the PAI/PBO process used in former SAP GUI apps:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/3b464d0b-19fd-400c-a7e4-3eec893f7724" />

UI5 vs. "UI5 Over the Wire" - Communication

We use an AJAX roundtrip logic similar to "HTML Over the Wire" approaches, but in this case, we cannot send HTML directly. Instead, we send a View combined with its Data. This results in a concept that we could refer to as "UI5-View Over the Wire".

##### 7. Merging Data & Presentation

A typical "UI5-View Over the Wire" response looks like this:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/d52112e6-b9b7-4e7f-ac7f-825c20620240" />

"UI5 Over the Wire" - Response with View & Data together




















But is this maybe just the same like RAP, but in a different format?











This pattern has gained popularity through frameworks like htmx, Hotwire, Phoenix LiveView, and Laravel Livewire. They aim to simplify development, reduce dependencies, and return to server-driven UIs.


This article provides a technical deep dive into **abap2UI5**. It is aimed at developers who want to understand how abap2UI5 works "under the hood" and how it simplifies UI5 development by keeping both logic and UI generation on the backend.

It covers the core ideas behind the framework — including its architecture, codebase, and compatibility — and shows how concepts like **"HTML Over the Wire"** are adapted to the ABAP environment, offering a new approach to building UI5 applications.

##### 1. HTML Over the Wire

One of the core ideas behind abap2UI5 is inspired by the concept of **"HTML Over the Wire"**. This approach suggests rendering HTML directly on the server and sending it to the browser — without relying on JSON, client-side MVC frameworks, bundling, or transpiling pipelines.





The idea was introduced in the SAP community through examples using the JavaScript library **htmx** to build Fiori-like apps. Unlike typical Single Page Applications (SPAs), where state and logic reside on the frontend, the **HTML Over the Wire** principle keeps all application logic and state on the server.

After the initial page load, only small HTML fragments are sent asynchronously via AJAX to update parts of the page — avoiding full reloads.

<img width="400" alt="image" src="https://github.com/user-attachments/assets/a9fde24a-c572-4e5c-b203-59a0667b9931" />

_HTML "Over the Wire" Lifecycle [(Quelle)](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763)_

This approach contrasts with the common separation of concerns, where HTML, CSS, and JavaScript are managed independently on the frontend while the backend only delivers data.

##### 2. Hypermedia Driven App

This concept evolves into what is termed a Hypermedia-Driven Application (HDA). In HDAs, the browser focuses solely on rendering HTML, CSS, and JavaScript without knowledge of the application's state. All logic is maintained on the server.

In contrast, SPAs define all routes and actions upfront on the frontend, requiring a full rebuild for any modification. The following illustration compares MPAs, SPAs, and HDAs:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/8117dc10-f0ba-4c52-9d1d-6b9d0986401d" />

MPA vs. SPA vs. HDA [(Quelle)](https://craftcms.com/events/dot-all-2022/sessions/a-practical-guide-to-html-over-the-wire)


##### 3. Rethinking Separation of Concerns

Unlike traditional architectures, HDAs do not prioritize strict separation of CSS, JavaScript, and HTML. The backend generates the UI and handles program flow, much like SAP GUI applications in the past. This centralized approach simplifies customization and maintenance.

##### 4. Dive Deeper

Frameworks like Phoenix LiveView (2018) and Laravel Livewire (2019) were among the first to adopt this principle. Tools like htmx, hotwire, and unpoly followed, aiming to reduce complexity while maintaining high UI fidelity. These frameworks seek a "sweet spot" between SPA and MPA architectures:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/41af4a41-829e-4289-82f5-18ee7408054b" />
"Sweet Spot" between SPA and MPA (Quelle)

A recommended video offers an excellent introduction to these ideas.

##### 5. UI5 Architecture

UI5 applications typically follow an SPA architecture. The backend delivers data via OData, while all logic and UI rendering occur on the frontend. But one specific characteristic we should examine closely is how the UI5 framework creates views. Each HTML output is rendered from an XML-View (let's ignore the former HTML/JS/JSON-Views), with its associated data from the server. The view is stored at the frontend as part of the app:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/3b2a884e-e899-4b60-8a95-79b418f33657" />

UI5 normally - ABAP delivers only Data




















#### new

The key concept of **abap2UI5** is to apply the **HTML Over-the-Wire** approach to SAP UI5 application development.

#### Concept

**HTML Over-the-Wire** refers to a web architecture where the server renders user interfaces and sends ready-to-use HTML fragments to the browser. This avoids complex client-side frameworks and keeps the frontend lightweight and maintainable. Both the UI and business logic remain on the server.

```plaintext
    +-------------------+       +------------------+       +-------------------+
    |     Server        |       |     Browser      |       |       User        |
    |-------------------|       |------------------|       |-------------------|
    | HTML Definition   |  -->  | Receives Response|  -->  | Interacts with UI |
    |                   |       | Renders UI       |       | (clicks, inputs)  |
    +-------------------+       +------------------+       +-------------------+
```

Flow:
- The server defines HTML
- The browser inserts these definitions
- Users interact with the UI — the backend handles logic and updates

While popular frameworks like Hotwire, Livewire, and Phoenix LiveView follow this approach, **abap2UI5 adapts it to SAP’s technological environment**, using SAP UI5 for frontend rendering and ABAP for backend-driven UI definitions.


#### Frameworks

Several frameworks successfully implement the HTML Over-the-Wire approach:

| Framework             | Focus                                | Tech Stack           |
|-----------------------|--------------------------------------|----------------------|
| [htmx](https://htmx.org)                | Progressive enhancement via HTML partials  | Any web stack        |
| [Hotwire (Turbo)](https://hotwired.dev) | HTML-over-the-wire for Rails apps          | Ruby on Rails        |
| [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view) | Real-time UI with server rendering         | Elixir / Phoenix     |
| [Livewire](https://livewire.laravel.com)         | Server-driven UI components in PHP         | Laravel / PHP        |
| [Unpoly](https://unpoly.com)            | Simplified partial page updates            | Any web stack        |


#### UI5 Over-the-Wire

So what does a UI5 Over-the-Wire approach look like? 

In this approach, the UI5 View and UI5 View-Model is defined on the ABAP server and transmitted to the browser. The browser renders the interface using a static UI5 application — without requiring additional JavaScript logic, OData services, or frontend frameworks.

**abap2UI5** brings the Over-the-Wire idea into the SAP world by combining:
- ABAP-based UI definitions (UI5 views & view-models)
- A static SAP UI5 frontend app for rendering
- Backend-controlled UI and business logic
- Targeted view-model updates, so only specific UI5 controls are re-rendered

```plaintext
+---------------------+       +------------------+       +-------------------+
|     ABAP Backend    |       |     Browser      |       |       User        |
|---------------------|       |------------------|       |-------------------|
| UI5 View Definition |  -->  | Receives Response|  -->  | Interacts with UI |
| UI5 View-Model      |       | Renders UI       |       | (clicks, inputs)  |
+---------------------+       +------------------+       +-------------------+
```

Flow:
- The backend defines UI5 XML views and JSON view-models
- The browser renders these definitions using a static UI5 application
- Users interact with the UI — the backend handles logic and updates

Key Benefits:
- **Static UI5 Frontend App:** Delivered with the first HTTP request and remains generic and stable across all applications.
- **Backend-Driven UI Control:** All UI definitions and business logic are implemented in ABAP classes.
- **Pure ABAP Development:** No need for custom JavaScript or separate frontend development.
- **Simplified Architecture:** No frontend builds or complex SPA frameworks — everything is managed through backend artifacts using abapGit and transports.
- **Seamless SAP Integration:** Fully aligned with SAP’s UI5 and ABAP technology stack, compatible with all ERP and S/4HANA releases.
- **Efficient for Business Applications:** Particularly well-suited for CRUD-heavy apps, forms, tables, dashboards, and typical enterprise use cases.

Limitations:
- Not suitable for highly interactive, real-time collaboration apps.
- Offline functionality or complex client-side interactions are not covered.
- Less effective if frontend and backend teams work independently.

#### Partial HTML Updates

A key feature of HTML over-the-wire is that the browser does not re-render the entire HTML page, but only specific parts. Can we achieve this with UI5? While modifying the XML view would typically trigger a complete re-render, focusing solely on updating the view model and binding UI attributes to it allows the UI5 framework to automatically update only the affected parts. Try out this snippet:

```abap
CLASS z2ui5_cl_app_partly_rerender DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA text TYPE string.
    DATA enabled TYPE abap_bool.

ENDCLASS.

CLASS z2ui5_cl_app_partly_rerender IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      client->view_display( z2ui5_cl_xml_view=>factory(
        )->input( enabled = client->_bind( enabled ) value = client->_bind( text )
        )->button( text  = 'partly rerender html'    press = client->_event( 'POST' )
        )->stringify( ) ).

    ELSE.
    
      enabled = xsdbool( enabled = abap_false ).
      text = text && ` text`.
      client->view_model_update( ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
```
Isn't that beautiful?

#### Summary

The **key concept of abap2UI5** is to bring the simplicity and efficiency of the HTML Over-the-Wire pattern into the SAP ecosystem.

By shifting UI control back to the ABAP backend and leveraging SAP UI5 for rendering, abap2UI5 enables maintainable, pragmatic business applications — without the overhead of SPA architectures.

For typical enterprise apps — forms, dashboards, transactions — abap2UI5 offers a clean, backend-driven alternative with faster time-to-market and lower complexity.

**Further Reading:**
- [htmx in a nutshell](https://htmx.org/docs/#introduction)
- [HTML Over the Wire](https://signalvnoise.com/svn3/html-over-the-wire/)
- [Fiori-like web app development in pure ABAP with htmx and Fundamental](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763)
