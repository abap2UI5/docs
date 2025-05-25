# Key Concept: UI5 Over-the-Wire
 
This page explains the basic architecture behind abap2UI5. The core idea is based on a pattern called _HTML Over-the-Wire_, adapted for the ABAP ecosystem. It simplifies UI5 development by moving both UI rendering and application logic entirely to the ABAP backend.

#### What is HTML Over-the-Wire?

_HTML Over-the-Wire_ describes a server-centric web architecture in which the user interface is generated on the server and sent to the browser as ready-to-render HTML.

Instead of building and maintaining complex JavaScript frontends, managing APIs, and exchanging JSON, the server handles everything — from business logic to UI generation. The browser simply receives and renders HTML fragments. This approach eliminates the need for client-side MVC frameworks, data transformation layers, and frontend deployment processes [(1)](https://signalvnoise.com/svn3/html-over-the-wire/):

> You can write fast, modern, responsive web applications by generating your HTML on the server, and delivering that directly to the browser. You don’t need JSON as an in-between format. You don’t need client-side MVC frameworks. You don’t need complicated bundling and transpiling pipelines.

> This is what HTML Over The Wire is all about. It’s a celebration of the simplicity in HTML as the format for carrying data and presentation together, whether this is delivered on the first load or with subsequent dynamic updates.

In this architecture the initial request delivers JavaScript and CSS, Subsequent interactions trigger AJAX calls to fetch HTML fragments and the browser inserts these fragments into the DOM without reloading the full page [(2)](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763):

<p align="center">
<img width="600" alt="image" src="https://github.com/user-attachments/assets/db393f3a-940d-4bd3-aec0-5523e8d58fa0" />
<br/>
  <em> HTML "Over the Wire" Lifecycle - Server sends HTML fragments, browser updates UI without full reload</em>
</p>

This results in a clean and lightweight frontend — a pure rendering layer — while all logic remains under full control of the backend.

Several modern frameworks follow this pattern:
* [htmx](https://htmx.org) Progressive enhancement via HTML partials (Any web stack)
* [Hotwire (Turbo)](https://hotwired.dev) HTML-over-the-wire for Rails apps (Ruby on Rails)
* [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view) Real-time UI with server rendering (Elixir/Phoenix)
* [Livewire](https://livewire.laravel.com) Server-driven UI components in PHP  (Laravel/PHP)
* [Unpoly](https://unpoly.com) Simplified partial page updates (Any web stack)


#### Comparison to Classic SSR

In the early days of web development, Server-Side Rendering (SSR) was the standard. Every user interaction triggered a full-page reload, and the server responded with a complete HTML document. Within the SAP ecosystem, this approach was seen in technologies like ITS Mobile and SAP GUI for HTML.

The emergence of Single-Page Applications (SPAs) shifted the rendering logic to the client. SPAs fetch raw data — through OData services in SAP systems — and dynamically construct the UI in the browser using JavaScript frameworks such as React, Angular, or Vue. SAP adopted this paradigm with the introduction of UI5 in 2010.

However, SPAs come with their own set of challenges: complex API layers, separation of frontend and backend development teams, and intricate build and deployment pipelines. As a counter-approach, HTML Over-the-Wire reintroduces a server-driven approach to UI updates:
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
 <img width="500" alt="image" src="https://github.com/user-attachments/assets/8043d0d9-5852-4dac-aefb-37ec8d6e66be" />
<br/>
  <em>UI5 Freestyle - UI is built on the client; backend delivers only Data via OData</em>
</p>

Since UI5 is a client-side rendering framework, the HTML output cannot be generated on the backend and sent to the client. Instead, HTML is produced in the browser using the UI5 framework and its built-in render.

#### Sending Views from Backend

Fortunately, UI5 has a defining characteristic that allows us to shift part of the view generation to the backend. Normally, each view is defined in XML — the so-called UI5 XML View. The UI5 framework uses this XML definition, combined with data from the backend, to render HTML in the browser.

<p align="center">
<img width="400" alt="image" src="https://github.com/user-attachments/assets/1ae233c6-96ff-4370-ac31-30705c18a0f7" />
<br/>
  <em>UI5 Freestyle – HTML rendered in browser based on Frontend XML View and Backend Data</em>
</p>

abap2UI5 introduces a subtle but important shift: what if the backend also delivers the XML View?

While HTML rendering still happens on the frontend, both the view definition and the corresponding data are now sent from the backend:

<p align="center">
<img width="400" alt="image" src="https://github.com/user-attachments/assets/7886d77e-acef-4e96-bc0f-ed3728e06358" />
<br/>
  <em>abap2UI5 – HTML rendered in browser based on XML View and Data, both send from the Backend</em>
</p>

The UI5 application remains a single-page application (SPA), but its role changes: it becomes a pure rendering engine for server-defined views and data. 

How is user interaction handled in this architecture?

#### Frontend Events on the Server

To support user interaction, a minimal and static UI5 Freestyle app is delivered with the initial HTTP request. This app contains just enough logic to forward frontend events typically acts as a shell app. The interaction model is inspired by the classic PAI/PBO pattern from SAP GUI applications.

When the user triggers an event (e.g., a button press), the event information is sent to the backend, where an ABAP class determines what happens next. All logic is executed entirely in the backend:

<p align="center">
<img width="500" alt="image" src="https://github.com/user-attachments/assets/64ed863f-09bf-4634-8688-5b5382595115" />
<br/>
  <em>abap2UI5 – Simple shell app, backend handles all logic</em>
</p>

In UI5 Freestyle apps, each application required a dedicated set of frontend artifacts:

<p align="center">
<img width="350" alt="image" src="https://github.com/user-attachments/assets/abfe4163-e75d-4eab-ad4c-621f693fa6c3" />
<br/>
  <em>UI5 Freestyle – Each application requires its own set of deployed UI and Logic artifacts</em>
</p>

With abap2UI5, the frontend becomes a static UI5 shell shared across all applications. Views and logic are fully defined and maintained in the backend. Each app is represented by a single ABAP class that generates the view and handles the events:

<p align="center">
<img width="350" alt="image" src="https://github.com/user-attachments/assets/646c10e3-ef82-403f-8538-9efe45836ca2" />
<br/>
  <em>abap2UI5 – Shared shell app, each application is represented by backend ABAP classes</em>
</p>

As a result, every UI5 app becomes a complete ABAP backend project managed through abapGit—eliminating the need for separate frontend deployments entirely.

#### Create & Update Data

So far, we’ve seen how to display data and handle events using a backend-driven approach. But how can user input be processed and changes made in the frontend be transferred back to the backend?

If we continued relying on OData, updates would typically be routed into the OData service layer — bypassing the ABAP class that also defines the view and handles events in abap2UI5.

Let’s take a closer look at a key UI5 feature: the concept of view models. In UI5 Freestyle, view models are used to bind attributes such as visible or enabled — allowing control properties in the view to be mapped precisely to model attributes:

<p align="center">
<img width="350" alt="image" src="https://github.com/user-attachments/assets/7eaa09d3-e3f7-4ebb-997d-fc68cc68421f" />
<br/>
  <em>UI5 View Model Concept – UI control properties are bound to View Model attributes</em>
</p>

This leads to the second key architectural shift in abap2UI5: Instead of binding to OData, abap2UI5 assembles a custom view model entirely in the backend. This model is constructed dynamically after each request — tailored specifically to the current view — and is sent together with the view definition to the frontend:

<p align="center">
<img width="385" alt="image" src="https://github.com/user-attachments/assets/5bb4d351-4f5e-4ba0-a09a-f17883bd25e6" />
<br/>
  <em>abap2UI5 – Backend delivers an XML View and its specifically tailored View Model in a single response</em>
</p>

This means CDS Views and OData services are no longer consumed directly on the frontend. Instead, the complete UI state — both view and model — is sent from the backend in a single response. Any user changes in the UI are then returned to the backend via a lightweight AJAX call containing the updated view model — no OData routing involved.

Developers do not need to manually configure models or bindings. abap2UI5 handles this internally. All the developer needs to do is expose attributes of their ABAP class via a simple binding method — everything else is managed automatically.

A typical response from the backend includes both the XML view and its corresponding view model:

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
And the corresponding XML View might look like this:
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

This architecture enables a clean and unified application model:

<p align="center">
<img width="500" alt="image" src="https://github.com/user-attachments/assets/d52112e6-b9b7-4e7f-ac7f-825c20620240" />
<br/>
  <em>abap2UI5 - Architecture</em>
</p>

Frontend and backend remain tightly coupled — not through OData contracts, but through plain ABAP logic and JSON — resulting in a fully backend-driven, editable UI flow.

#### Partial HTML Updates

A core benefit of the HTML Over-the-Wire approach is that only the affected parts of the UI are updated — not the entire page. But can this pattern be applied in UI5?

In standard UI5 behavior, updating the XML View typically triggers a full re-render. However, abap2UI5 makes partial updates possible by updating only the view model. This enables UI5 to refresh only the relevant UI controls via data binding — without recreating the entire view structure.

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
The illustration below shows the difference between a full re-render and a targeted view model update:

<p align="center">
  <img src="https://github.com/user-attachments/assets/79a8c531-b9a0-4bf4-bb1c-7d9019ef8707" width="500" />
  <br/>
  <em>Only relevant DOM parts are updated via model binding, preserving UI state</em>
</p>

Thanks to UI5’s powerful data binding mechanism, only the modified DOM elements are updated. This preserves the current UI state — such as input focus — and ensures a smooth, uninterrupted user experience. The XML View and View Model concept make UI5 a perfect team player for the UI5 Over-the-Wire approach combining the strengths of ABAP and UI5 — without the complexity of full client-side re-renders.

#### Conclusion

abap2UI5 brings the simplicity and efficiency of the HTML Over-the-Wire pattern into the ABAP ecosystem — combining proven technologies with a smart UI rendering strategy.

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

By relocating UI control to the ABAP backend and using UI5 purely for HTML rendering, abap2UI5 enables pragmatic, maintainable business application development — without the complexity of SPA architectures.

Happy ABAPing!

**References:**
- [htmx in a nutshell](https://htmx.org/docs/#introduction)
- [HTML Over the Wire](https://signalvnoise.com/svn3/html-over-the-wire/)
- [Fiori-like web app development in pure ABAP with htmx and Fundamental](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763)
- [UI5 SDK](https://sapui5.hana.ondemand.com/sdk/#/topic/ec699e0817fb46a0817b0fa276a249f8)
