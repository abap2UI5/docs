# Key Concept: UI5 Over-the-Wire in abap2UI5

This page explains the basic architecture behind abap2UI5. The core idea is based on a pattern called _HTML Over-the-Wire_, adapted for the SAP technology stack. It simplifies UI5 development by moving both UI rendering and application logic entirely to the ABAP backend.

#### What is HTML Over-the-Wire?

_HTML Over-the-Wire_ describes a server-centric web architecture in which the user interface is generated on the server and sent to the browser as ready-to-render HTML.

Instead of building and maintaining complex JavaScript frontends, managing APIs, and exchanging JSON, the server handles everything — from business logic to UI generation. The browser simply receives and renders HTML fragments. This approach eliminates the need for client-side MVC frameworks, data transformation layers, and frontend deployment processes [(1)](https://signalvnoise.com/svn3/html-over-the-wire/):

> You can write fast, modern, responsive web applications by generating your HTML on the server, and delivering that directly to the browser. You don’t need JSON as an in-between format. You don’t need client-side MVC frameworks. You don’t need complicated bundling and transpiling pipelines.

> This is what HTML Over The Wire is all about. It’s a celebration of the simplicity in HTML as the format for carrying data and presentation together, whether this is delivered on the first load or with subsequent dynamic updates.

In this architecture the initial request delivers JavaScript and CSS, Subsequent interactions trigger AJAX calls to fetch HTML fragments and the browser inserts these fragments into the DOM without reloading the full page [(2)](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763):

<p align="center">
<img width="600" alt="image" src="https://github.com/user-attachments/assets/db393f3a-940d-4bd3-aec0-5523e8d58fa0" />
<br/>
  <em> HTML "Over the Wire" Lifecycle</em>
</p>

This results in a clean and lightweight frontend — a pure rendering layer — while all logic remains under full control of the backend.

Several modern frameworks follow this pattern:
* [htmx](https://htmx.org) Progressive enhancement via HTML partials (Any web stack)
* [Hotwire (Turbo)](https://hotwired.dev) HTML-over-the-wire for Rails apps (Ruby on Rails)
* [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view) Real-time UI with server rendering (Elixir/Phoenix)
* [Livewire](https://livewire.laravel.com) Server-driven UI components in PHP  (Laravel/PHP)
* [Unpoly](https://unpoly.com) Simplified partial page updates (Any web stack)


#### How is this different from classic Server Side Rendering?

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

#### How UI5 Freestyle Works

UI5 freestyle apps follow the Single Page Application (SPA) model. All UI artifacts are stored on the frontend, while the backend provides data via OData — typically based on CDS Views or custom ABAP implementations. Both rendering and logic execution take place entirely in the browser:

<p align="center">
  <img width="500" alt="image" src="https://github.com/user-attachments/assets/7ba09f8b-b8ad-48e3-9693-e12feeb94aed" />
<br/>
  <em>Freestyle - ABAP Stack delivers only Data</em>
</p>

Since UI5 is a client-side rendering framework, the HTML output cannot be generated on the backend and sent to the client. Instead, HTML is produced in the browser using the UI5 framework and its built-in render.

#### Sending Views from Backend

Fortunately, UI5 has a defining characteristic that allows us to shift part of the view generation to the backend. Normally, each view is defined in XML — the so-called UI5 XML View. The UI5 framework uses this XML definition, combined with data from the backend, to render HTML in the browser.

<p align="center">
<img width="500" alt="image" src="https://github.com/user-attachments/assets/a9bf8f85-54e7-476d-be87-2cd028334d2d" />
<br/>
  <em>UI5 Freestyle – The browser renders HTML based on XML View and backend data</em>
</p>

abap2UI5 introduces a subtle but important shift: what if the server also delivers the XML view? 

While HTML rendering still happens on the frontend, both the view definition and the corresponding data are now delivered from the backend via AJAX:

<p align="center">
    <img width="500" alt="image" src="https://github.com/user-attachments/assets/ec1ac3f8-65fb-4155-84f6-1ec61a088c40" />
<br/>
  <em>abap2UI5 – The browser renders HTML based on XML View and data fully delivered by the backend</em>
</p>

The UI5 application remains a single-page application (SPA), but its role changes: it becomes a pure rendering engine. It simply receives server-defined views and data, without needing awareness of the UI structure (e.g., tables, lists, input fields) or the application's flow logic.

####  Handling Frontend Events in the Backend

To enable user interaction, a minimal static UI5 shell is delivered with the initial HTTP request. This shell contains just enough logic to forward frontend events to the backend. The interaction model is inspired by the classic PAI/PBO pattern from SAP GUI applications.

When the user triggers an event (e.g., button press), the event is sent to the backend, where an ABAP class handles the corresponding logic and determines the next step. All business logic resides entirely in the backend:

<p align="center">
<img width="500" alt="image" src="https://github.com/user-attachments/assets/ecd6e798-b6f6-4816-89ca-90f20647eb04" />
<br/>
  <em>subtitle</em>
</p>

With this architecture, the frontend acts as a static UI5 container. It remains unchanged across apps and the actual logic of each app is stored in baclkend abap classes.

Previously, we had to maintain many frontend artifacts:

<p align="center">
<img width="500" alt="image" src="https://github.com/user-attachments/assets/8b5c9b5b-3014-489f-90b4-55222744ba8a" />
<br/>
  <em>UI5 Freestyle – multiple frontend artifacts per app</em>
</p>

With abap2UI5, only one generic UI5 shell is needed. All views and application logic are defined and maintained centrally in the backend:

<p align="center">
<img width="500" alt="image" src="https://github.com/user-attachments/assets/79c7c6be-6424-4c33-ab3c-9c7799a74747" />
<br/>
  <em>abap2UI5 – One static frontend, all views and logic in ABAP</em>
</p>

#### Editable Data Exchange

So far, we’ve seen how to display data in a backend-driven approach. But how do we handle changes made on the frontend? If we still rely on OData, any update would typically be routed into the OData implementation layer, not into the ABAP class that also defines the view in abap2UI5. To solve this, abap2UI5 uses the concept of a View Model.

In standard UI5, view models (often JSON models) are used to bind attributes such as visible or enabled:

<p align="center">
<img width="500" alt="image" src="https://github.com/user-attachments/assets/df92711f-abd1-4bfd-b84a-268bb452503f" />
<br/>
  <em>subtitle</em>
</p>
Here comes the second key shift: Instead of binding to OData, abap2UI5 binds to a custom JSON model, explicitly assembled in the backend. This model is sent together with the view to the frontend:

<p align="center">
<img width="500" alt="image" src="https://github.com/user-attachments/assets/461f08c2-0f0f-424e-a7f8-008af3610258" />
<br/>
  <em>subtitle</em>
</p>
This means we no longer consume CDS Views or OData services directly on the frontend. Instead, we send the view and its data model together from the backend. Any changes made in the UI can then be sent directly back to the backend via simple AJAX, without OData routing.

The abap2UI5 framework provides binding helpers and handles editable states, field values, and validation—all within ABAP classes. App developers do not need to deal with model configuration or UI binding logic manually.

A typical response from the backend now includes both the UI definition (view) and the data model:
```json
{
   "S_FRONT": {
      "APP": "Z2UI5_CL_APP_HELLO_WORLD",
      "ID": "AD94A1CC76F145E986F4DFCB7D183CC5",
      "PARAMS": {
         "S_VIEW": {
            "XML": " <mvc:View displayBlock=\"true\" height=\"100%\" xmlns=\"sap.m\" xmlns:core=\"sap.ui.core\" ..."
         }
      }
   },
   "MODEL": {
      "XX": {
         "NAME": "test"
      }
   }
}
```
With the XML View:
```xml
<mvc:View xmlns="sap.m" xmlns:core="sap.ui.core" xmlns:form="sap.ui.layout.form" xmlns:mvc="sap.ui.core.mvc" displayBlock="true" height="100%">
  <Shell>
    <Page title="abap2UI5 - Hello World">
      <form:SimpleForm editable="true">
        <form:content>
          <Title text="Make an input here and send it to the server..."/>
          <Label text="Name"/>
          <Input value="{/XX/NAME}"/>
          <Button press=".eB(['BUTTON_POST'])" text="post"/>
        </form:content>
      </form:SimpleForm>
    </Page>
  </Shell>
</mvc:View>
```
A complete picture of the architecture looks like this:

<p align="center">
<img width="500" alt="image" src="https://github.com/user-attachments/assets/d52112e6-b9b7-4e7f-ac7f-825c20620240" />
<br/>
  <em>subtitle</em>
</p>

With this model, the frontend simply renders what the backend provides and sends back any updates. No direct use of OData or CDS Views is required in the frontend. Editable fields, validations, and roundtrips are all handled seamlessly within the backend.

#### Efficient Partial HTML Updates

A central feature of the HTML Over the Wire approach is that only the affected parts of the page are updated, rather than the entire document. Can this be achieved in UI5?

While changing the entire XML View in UI5 typically results in a full re-render, abap2UI5 makes partial updates possible by updating only the view model. This allows UI5 to efficiently update only the relevant UI controls through data binding—without rebuilding the entire view. 

Consider this example:

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
In the comparison below, you can see the difference between a full re-render and a partial view model update:

<p align="center">
  <img src="https://github.com/user-attachments/assets/79a8c531-b9a0-4bf4-bb1c-7d9019ef8707" width="500" />
  <br/>
  <em>You can see the difference: partly vs. not</em>
</p>

In the following illustration, you can see the difference between a full re-render and a targeted view model update:

Thanks to UI5's powerful data binding mechanism, only the affected DOM elements are updated. This preserves the UI state—for example, the input focus remains intact—and provides a fluid user experience.

To ensure optimal performance and responsiveness, avoid unnecessary full re-renders. Instead, use:

```abap
client->view_model_update( ).
```
This makes UI5 a perfect team player for the HTML Over-the-Wire approach, where the ABAP backend is responsible for building both the UI structure and its dynamic state.
The result is an efficient, responsive, and low-maintenance application architecture that brings the best of UI5 and ABAP together.

#### Conclusion & Benefits

abap2UI5 brings the simplicity and efficiency of HTML Over-the-Wire into the ABAP ecosystem.

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

By relocating UI control to the ABAP backend and using UI5 purely for HTML rendering, abap2UI5 enables pragmatic, maintainable business application development — without the complexity of SPA architectures. It offers a clean, backend-driven alternative perfect for enterprise use cases with short development cycles.

Happy ABAPing!

**References:**
- [htmx in a nutshell](https://htmx.org/docs/#introduction)
- [HTML Over the Wire](https://signalvnoise.com/svn3/html-over-the-wire/)
- [Fiori-like web app development in pure ABAP with htmx and Fundamental](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763)
