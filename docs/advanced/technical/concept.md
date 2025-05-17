# Key Concept: UI5 Over-the-Wire?

The key concept of **abap2UI5** is to apply the **HTML Over-the-Wire** concept to SAP UI5 application development.

**HTML Over-the-Wire** refers to a web architecture where the server renders user interfaces and sends ready-to-use HTML fragments to the browser. This avoids complex client-side frameworks and keeps the frontend lightweight and maintainable. The UI and Business Logic stays on the server.

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
- The browser inserts these Definitions
- Users interact with the UI — the backend handles logic and updates

While popular frameworks like Hotwire, Livewire, and Phoenix LiveView follow this approach, **abap2UI5 adapts it to SAP’s technological environment**, using SAP UI5 for frontend rendering and ABAP for backend-driven UI definitions.

#### From SSR to HTML Over-the-Wire

In early web development, **Server-Side Rendering (SSR)** was the default. Every user action triggered a full-page reload with a complete HTML response.

The rise of **Single Page Applications (SPAs)** shifted rendering to the browser. SPAs fetch raw data (often via OData in SAP) and dynamically build UIs with JavaScript frameworks like React, Angular, Vue or in the SAP world UI5.

However, SPAs brought new challenges:
- API layers and data contracts
- Separate frontend-backend development workflows
- Complex build & deployment pipelines

As a reaction, the **Over-the-Wire** paradigm re-emerged:
- Servers send UI fragments, not full pages
- Browsers update only specific parts of the page
- Frontends stay simple and declarative

#### Architectural Comparison

| Approach | Data Flow | Rendering Location | Period |
|----------|-----------|-------------------|--------|
| **SSR** | Full-page HTML responses | Entirely on the server | 1990s – 2010s |
| **SPA** | Raw data (JSON), client builds UI | Client-side (JavaScript) | 2010s – today |
| **Over-the-Wire** | HTML fragments for partial updates | Server renders, browser inserts | 2020s (re-emerging) |

#### Related Frameworks

Several frameworks successfully implement the HTML over-the-wire approach:

| Framework             | Focus                                | Tech Stack           |
|-----------------------|--------------------------------------------------|----------------------|
| [htmx](https://htmx.org)                | Progressive enhancement via HTML partials  | Any web stack        |
| [Hotwire (Turbo)](https://hotwired.dev) | HTML-over-the-wire for Rails apps          | Ruby on Rails        |
| [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view) | Real-time UI with server-rendering         | Elixir / Phoenix     |
| [Livewire](https://livewire.laravel.com)         | Server-driven UI components in PHP         | Laravel / PHP        |
| [Unpoly](https://unpoly.com)            | Simplified partial page updates            | Any web stack        |


#### UI5 Over-the-Wire

So how can a UI5 over-the-wire approach look like? In this architecture, the UI is defined on the ABAP server and transmitted to the browser. The browser renders the interface using a static UI5 application — without requiring additional JavaScript logic, OData services, or frontend frameworks.

```plaintext
+---------------------+       +------------------+       +-------------------+
|     ABAP Backend    |       |     Browser      |       |       User        |
|---------------------|       |------------------|       |-------------------|
| UI5 View Definition |  -->  | Receives Response|  -->  | Interacts with UI |
| UI5 View-Model      |       | Renders UI       |       | (clicks, inputs)  |
+---------------------+       +------------------+       +-------------------+
```
Flow:
- The backend defines UI5 XML Views and JSON View-Models
- The browser renders these definitions using a static UI5 application
- Users interact with the UI — the backend handles logic and updates


**abap2UI5** transfers the Over-the-Wire idea into SAP’s world by combining:
- ABAP-based UI definitions (UI5 Views & UI5 View-Models)
- A static SAP UI5 frontend app for rendering
- Backend-controlled UI & Business logic
- Targeted ViewModel updates ensure that only specific UI5 controls are re-rendered

#### Key Benefits

- Static UI5 Frontend App: Delivered with the first HTTP request and remains generic and stable across all applications.
- Backend-Driven UI Control: All UI definitions and business logic are implemented in ABAP classes.
- Pure ABAP Development: No need for custom JavaScript or separate frontend development.
- Simplified Architecture: No frontend builds or complex SPA frameworks — everything is managed through backend artifacts using abapGit and transports.
- Seamless SAP Integration: Fully aligned with SAP’s UI5 and ABAP technology stack, compatible with all ERP and S/4HANA releases.
- Efficient for Business Applications: Particularly well-suited for CRUD-heavy apps, forms, tables, dashboards, and typical enterprise use cases.

#### Limitations to Consider

- Not suitable for highly interactive, real-time collaboration apps.
- Offline functionality or complex client-side interactions are not covered.
- Less effective if frontend and backend teams work independently.

#### Partly updates of the HTML page
One key feature is that the browser does not re-render the entire HTML page, but only specific parts. Can we achieve this with UI5?
While modifying the XML view would typically trigger a complete re-render, focusing solely on updating the view model and binding UI attributes to it allows the UI5 framework to automatically update only the affected parts. Try out this snippet:

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
Isn't that beatuiful?

#### Summary

The **key concept of abap2UI5** is to bring the simplicity and efficiency of the HTML Over-the-Wire pattern into the SAP ecosystem.

By shifting UI control back to the ABAP backend and leveraging SAP UI5 for rendering, abap2UI5 enables maintainable, pragmatic business applications — without the overhead of SPA architectures.

For typical enterprise apps — forms, dashboards, transactions — abap2UI5 offers a clean, backend-driven alternative with faster time-to-market and lower complexity.

#### Further Reading
- [htmx in a nutshell](https://htmx.org/docs/#introduction)
- [HTML Over the Wire](https://signalvnoise.com/svn3/html-over-the-wire/)
- [Fiori-like web app development in pure ABAP with htmx and Fundamental](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763)
