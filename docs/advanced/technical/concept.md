# Key Concept: UI5 Over-the-Wire?

The key concept of **abap2UI5** is to apply the **HTML Over-the-Wire** concept to SAP UI5 application development.

**HTML Over-the-Wire** refers to a web architecture where the server renders user interfaces and sends ready-to-use HTML fragments to the browser. This avoids complex client-side frameworks and keeps the frontend lightweight and maintainable.

While popular frameworks like Hotwire, Livewire, and Phoenix LiveView follow this approach, **abap2UI5 adapts it to SAP’s technological environment**, using SAP UI5 for frontend rendering and ABAP for backend-driven UI definitions.

#### Concept Overview

```plaintext
+------------------------+        +-----------------------+        +--------------------+
|     ABAP Backend       |        |       Browser         |        |       User         |
|------------------------|        |-----------------------|        |--------------------|
| - UI5 View Definition  |  -->   | Receives Response     |  -->   | Interacts with UI  |
| - UI5 View-Model       |        | Renders with UI5      |        | (clicks, inputs)   |
+------------------------+        +-----------------------+        +--------------------+
```
Flow:
- The backend defines UI5 XML Views and JSON View-Models
- The browser renders these definitions using a static UI5 application
- Users interact with the UI — the backend handles logic and updates

### From SSR to HTML Over-the-Wire

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

#### Over-the-Wire Frameworks

Several frameworks successfully implement this pattern:

| Framework | Key Use Case | Technology |
|-----------|--------------|------------|
| [htmx](https://htmx.org/) | Progressive enhancement for server-rendered apps | Any web stack |
| [Hotwire (Turbo)](https://hotwired.dev/) | Modern Over-the-Wire for Rails apps | Ruby on Rails |
| [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view) | Real-time server-rendered UIs | Elixir / Phoenix |
| [Livewire](https://livewire.laravel.com/) | Server-driven UI components | PHP / Laravel |
| [Unpoly](https://unpoly.com/) | Simplified partial page updates | Any web stack |
| [Blazor Server](https://learn.microsoft.com/en-us/aspnet/core/blazor/) | Server-side UI rendering with SignalR | .NET / C# |
| [Inertia.js](https://inertiajs.com/) | SPA-like experience with server-driven UI | JavaScript + Laravel/Rails |

### abap2UI5: UI5 Over-the-Wire

**abap2UI5** transfers the Over-the-Wire idea into SAP’s world by combining:
- ABAP-based UI definitions (UI5 Views & UI5 View-Models)
- A static SAP UI5 frontend app for rendering
- Backend-controlled UI logic and state management
- Targeted ViewModel updates ensure that only specific UI5 controls are re-rendered

#### Key Benefits and Characteristics
- Static UI5 Frontend App: Delivered tieh the first HTTP request, remaining generic and stable for all apps
- Backend-Driven UI Control: All UI definitions and business logic is defined in ABAP classes
- Pure ABAP Development: No need for custom JavaScript or separate frontend development
- Simplified Architecture: No frontend builds, no complex SPA frameworks — everything managed through backend artifacts with abapGit and transports
- Seamless SAP Integration: Fully aligned with SAP’s UI5 and ABAP technology stack, compatible with all ERP and S/4HANA releases
- Efficient for Business Applications: Especially suitable for CRUD-heavy apps, forms, tables, dashboards, and typical enterprise use cases.

#### Limitations to Consider

- Not suitable for highly interactive, real-time collaboration apps.
- Offline functionality or complex client-side interactions are not covered.
- Less effective if frontend and backend teams work independently.

### Summary

The **key concept of abap2UI5** is to bring the simplicity and efficiency of the HTML Over-the-Wire pattern into the SAP ecosystem.

By shifting UI control back to the ABAP backend and leveraging SAP UI5 for rendering, abap2UI5 enables maintainable, pragmatic business applications — without the overhead of SPA architectures.

For typical enterprise apps — forms, dashboards, transactions — abap2UI5 offers a clean, backend-driven alternative with faster time-to-market and lower complexity.

### Further Reading
- [htmx in a nutshell](https://htmx.org/docs/#introduction)
- [HTML Over the Wire](https://signalvnoise.com/svn3/html-over-the-wire/)
- [Fiori-like web app development in pure ABAP with htmx and Fundamental](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763)
