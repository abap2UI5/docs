---
outline: [2, 4]
---
# UI5 Over-the-Wire

_The Architecture of abap2UI5_

This article introduces the core pattern behind abap2UI5: HTML Over-the-Wire — adapted for the ABAP ecosystem. It shows how this approach cuts traditional frontend complexity by moving UI rendering and application logic to the backend. The result: faster development, simpler deployment, and a UI5 frontend shell that's purely a rendering engine.

#### What is HTML Over-the-Wire?

_HTML Over-the-Wire_ describes a server-centric web architecture where the server generates the user interface and sends it to the browser as ready-to-render HTML.

Instead of building and maintaining complex JavaScript frontends, managing APIs, and exchanging JSON, the server handles everything — from business logic to UI generation. The browser only receives HTML fragments and renders them. This approach removes the need for client-side MVC frameworks, data transformation layers, and frontend deployment pipelines [(1)](https://signalvnoise.com/svn3/html-over-the-wire/):

> You can write fast, modern, responsive web applications by generating your HTML on the server, and delivering that directly to the browser. You don't need JSON as an in-between format. You don't need client-side MVC frameworks. You don't need complicated bundling and transpiling pipelines.

> This is what HTML Over The Wire is all about. It's a celebration of the simplicity in HTML as the format for carrying data and presentation together, whether this is delivered on the first load or with subsequent dynamic updates.

In this architecture, the initial request delivers JavaScript and CSS. Later interactions trigger AJAX calls to fetch HTML fragments, and the browser inserts these fragments into the DOM without reloading the full page [(2)](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763):

<p align="center">
<img width="600" alt="HTML Over-the-Wire lifecycle - server sends HTML fragments, browser updates UI without full reload" src="https://github.com/user-attachments/assets/db393f3a-940d-4bd3-aec0-5523e8d58fa0" />
<br/>
  <em>HTML "Over the Wire" Lifecycle - Server sends HTML fragments, browser updates UI without full reload</em>
</p>

The result is a clean, lightweight frontend — a pure rendering layer — while all logic remains under the backend's full control.

Several frameworks adopt this pattern:
- [htmx](https://htmx.org) Progressive enhancement through HTML partials (Any web stack)
- [Hotwire (Turbo)](https://hotwired.dev) HTML-over-the-wire for Rails apps (Ruby on Rails)
- [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view) Real-time UI with server rendering (Elixir/Phoenix)
- [Livewire](https://livewire.laravel.com) Server-driven UI components in PHP (Laravel/PHP)
- [Unpoly](https://unpoly.com) Simplified partial page updates (Any web stack)

#### Comparison to Classic SSR

But isn't this the same as traditional Server-Side Rendering (SSR)?

In the early days of web development, SSR was the standard. Every user interaction triggered a full-page reload, and the server responded with a complete HTML document. In the SAP ecosystem, this approach appeared in technologies like ITS Mobile and SAP GUI for HTML.

The rise of Single-Page Applications (SPAs) shifted UI logic to the client. SPAs fetch raw data — through OData services in SAP systems — and build the UI dynamically in the browser using JavaScript frameworks like React, Angular, or Vue. SAP adopted this approach with UI5 in 2010.

But SPAs come with their own challenges: complex API layers, separation of frontend and backend teams, and intricate build and deployment pipelines. As a counterpoint, HTML Over-the-Wire reintroduces a server-driven model for UI updates:
- The server sends only HTML fragments, not entire pages; the browser updates specific parts of the DOM
- The frontend remains simple and declarative; all logic and artifacts live in the backend

Architectural Comparison:

| Approach      | Data Flow                        | Rendering Location         | Period           |
|---------------|----------------------------------|---------------------------|------------------|
| **SSR**       | Full-page HTML responses         | Entirely on the server    | 1990s – 2010s    |
| **SPA**       | Raw data (JSON), client builds UI| Client-side (JavaScript)  | 2010s – today    |
| **Over-the-Wire** | HTML fragments for partial updates | Server renders, browser inserts | 2020s (re-emerging) |

#### How UI5 Freestyle Works

UI5 freestyle apps follow the Single-Page Application model. The frontend holds all UI and app artifacts, while the backend provides data through OData — usually backed by CDS Views or custom ABAP implementations. The browser handles rendering and logic execution entirely:

<p align="center">
 <img width="500" alt="UI5 freestyle - UI is built on the client, backend delivers only data via OData" src="https://github.com/user-attachments/assets/8043d0d9-5852-4dac-aefb-37ec8d6e66be" />
<br/>
  <em>UI5 freestyle - UI is built on the client; backend delivers only Data via OData</em>
</p>

Since UI5 is a client-side framework, the backend can't generate ready-to-render HTML. Instead, the browser creates the HTML with JavaScript through the UI5 framework.

#### Sending Views from Backend

So how can we generate UI5 HTML in the backend?

Luckily, UI5 has a key trait that lets us shift part of the view generation to the backend. In UI5 freestyle apps, you define each view in XML — the UI5 XML View. The UI5 framework uses this XML definition, combined with data from the backend, to create HTML in the browser.

<p align="center">
<img width="400" alt="UI5 freestyle - HTML created in browser based on frontend XML View and backend data" src="https://github.com/user-attachments/assets/ee0152e7-4f64-4b90-9cf2-3faead53044f" />
<br/>
  <em>UI5 freestyle – HTML created in browser based on frontend XML View and backend Data</em>
</p>

abap2UI5 introduces a small but important shift: what if the backend also delivers the XML View?

While HTML creation still happens on the frontend, the backend now sends both the view definition and its data:

<p align="center">
  <img width="400" alt="abap2UI5 - HTML created in browser based on XML View and data, both sent from backend" src="https://github.com/user-attachments/assets/adef16b7-e98d-476f-9bbc-738685047c5d" />
<br/>
  <em>abap2UI5 – HTML created in browser based on XML View and Data, both sent from the backend</em>
</p>

The UI5 application remains a single-page application, but its role changes: it now focuses only on creating HTML from views and data that the server provides.

#### Frontend Events on the Server

How do we handle user interaction in this setup?

To support user interaction, the initial HTTP request delivers a small, static UI5 freestyle app. This app contains just enough logic to forward frontend events and usually acts as a shell application. The interaction model takes inspiration from the classic PAI/PBO pattern familiar from SAP GUI applications.

When a user triggers an event (e.g., pressing a button), the app sends the event information to the backend, where an ABAP class decides what happens next. All logic runs entirely on the server:

<p align="center">
<img width="500" alt="abap2UI5 - simple shell app, backend handles all logic" src="https://github.com/user-attachments/assets/64ed863f-09bf-4634-8688-5b5382595115" />
<br/>
  <em>abap2UI5 – Simple shell app, backend handles all logic</em>
</p>

The frontend becomes a static shell shared across all applications. The backend fully defines and maintains views and logic, and each application lives entirely in backend ABAP classes. The result: every UI5 project becomes a backend project — no separate frontend deployment needed:

<p align="center">
<img width="400" alt="abap2UI5 - shared shell app with each application defined by backend artifacts only" src="https://github.com/user-attachments/assets/2d8b6441-84f3-464c-980f-2773d619af29" />
<br/>
  <em>abap2UI5 – Shared shell app, with each application defined by backend artifacts only</em>
</p>

By contrast, traditional UI5 freestyle applications need a dedicated set of frontend artifacts for each app:

<p align="center">
<img width="300" alt="UI5 freestyle - each application requires its own set of deployed UI and app artifacts" src="https://github.com/user-attachments/assets/9aa09a7f-5931-496d-bf88-d9b34653784c" />
<br/>
  <em>UI5 freestyle – Each application requires its own set of deployed UI and App artifacts</em>
</p>

This leads to extra deployment effort during both development and go-live.

#### Create and Update Data

So far, we've seen how to display data and handle events through a backend-driven approach. But how do we process user input and send frontend changes back to the server?

If we kept relying on OData, updates would usually route into the OData service layer — bypassing the ABAP class that also defines the view and handles events in abap2UI5.

Let's take a closer look at a key UI5 feature: view models. In UI5 freestyle, view models bind attributes like visible or enabled — mapping control properties in the view directly to model attributes:

<p align="center">
<img width="340" alt="UI5 View Model concept - UI control properties bound to View Model attributes" src="https://github.com/user-attachments/assets/7eaa09d3-e3f7-4ebb-997d-fc68cc68421f" />
<br/>
  <em>UI5 View Model Concept – UI control properties are bound to View Model attributes</em>
</p>

This leads to the second subtle shift in abap2UI5: instead of binding OData to the view, abap2UI5 uses a custom view model built entirely in the backend. The backend builds this model dynamically after each request — tailored to the current view — and sends it together with the view definition to the frontend:

<p align="center">
<img width="385" alt="abap2UI5 - backend delivers XML View and tailored View Model in a single response" src="https://github.com/user-attachments/assets/5bb4d351-4f5e-4ba0-a09a-f17883bd25e6" />
<br/>
  <em>abap2UI5 – Backend delivers an XML View and its tailored View Model in a single response</em>
</p>

This means the frontend no longer reads CDS Views and OData services directly. Instead, the backend sends the entire UI state — view and model — in a single response. Any user changes in the UI then return to the backend through a lightweight AJAX call carrying the updated view model — no OData routing required.

You don't need to configure models or bindings manually — abap2UI5 handles this internally. Expose class attributes through a simple bind method, and abap2UI5 handles the rest.

A typical backend response holds the XML View:

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
And its View Model:
```json
{
   "MODEL": {
      "XX": {
         "NAME": "test"
      }
   }
}
```
#### Application Flow

The application flow looks like this:

<p align="center">
<img width="600" alt="abap2UI5 architecture - UI5 Over-the-Wire application flow" src="https://github.com/user-attachments/assets/f4df9291-c067-495f-bb52-a68e165e15c1" />
<br/>
  <em>abap2UI5 Architecture - UI5 Over-the-Wire</em>
</p>

The initial request delivers the static shell app. After each user interaction, the app calls the backend — in PAI/PBO fashion — to fetch the updated view and model. Frontend and backend remain tightly coupled — not through OData service definitions, but through plain ABAP logic and JSON. The result: a fully backend-driven UI flow.

#### Partial HTML Updates

A core benefit of the HTML Over-the-Wire approach: it updates only the changed parts of the UI — not the entire page. But can we apply this pattern in UI5?

In standard UI5, updating the XML View usually triggers a full re-render. But abap2UI5 enables partial updates by updating only the view model. This lets UI5 refresh only the relevant UI controls through data binding — without recreating the entire view structure.

An example:

```abap
CLASS z2ui5_cl_app_partial_rerendering DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA text TYPE string.
    DATA partly TYPE abap_bool.
ENDCLASS.

CLASS z2ui5_cl_app_partial_rerendering IMPLEMENTATION.
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
The illustration below shows the difference between a full re-render and a targeted view model update:

<p align="center">
  <img alt="Partial HTML re-rendering via View Model updates - only the relevant DOM parts are refreshed" src="https://github.com/user-attachments/assets/79a8c531-b9a0-4bf4-bb1c-7d9019ef8707" width="500" />
  <br/>
  <em>Partial HTML Re-rendering via View Model Updates - Only relevant DOM parts are re-rendered, preserving UI state</em>
</p>

Thanks to UI5's data binding mechanism, UI5 updates only the modified DOM elements. This keeps the current UI state intact — like input focus — and delivers a smooth, uninterrupted user experience.

The XML View and View Model concepts make UI5 a perfect match for the UI5 Over-the-Wire approach, removing the need for full client-side re-renders.

#### Conclusion

abap2UI5 brings the simplicity and efficiency of the HTML Over-the-Wire pattern into the ABAP ecosystem:

Key Benefits:
- One Static UI5 Shell App: Delivered with the initial HTTP GET request; shared and consistent across all applications
- Backend-Driven UI Control: ABAP classes implement UI definitions and business logic entirely
- ABAP-Centric Development: Removes the need for extra JavaScript or separate frontend development
- Simplified Deployment Model: No SPA-specific tooling or build pipelines; abapGit and standard transport mechanisms handle application logic and artifacts
- Clean SAP Integration: Fully compatible with UI5 and ABAP, supports ERP and S/4, Standard ABAP and ABAP Cloud
- Efficient for Business Applications: Ideal for CRUD operations, forms, dashboards, and typical enterprise use cases

Limitations:
- Not designed for heavily interactive or real-time collaborative applications
- Offline functionality and complex client-side interactions aren't covered
- Less effective if frontend and backend teams work independently

By moving UI control to the ABAP backend and using UI5 only for HTML creation, abap2UI5 enables fast and efficient development of business applications — without SPA complexity.

Happy ABAPing!

**References:**
- [htmx in a nutshell](https://htmx.org/docs/#introduction)
- [HTML Over the Wire](https://signalvnoise.com/svn3/html-over-the-wire/)
- [Fiori-like web app development in pure ABAP with htmx and Fundamental](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763)
- [UI5 SDK](https://sapui5.hana.ondemand.com/sdk/#/topic/ec699e0817fb46a0817b0fa276a249f8)
