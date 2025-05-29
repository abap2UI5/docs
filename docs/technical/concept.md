# UI5 Over-the-Wire: The Core Architecture of abap2UI5

If you're new to abap2UI5 and want to understand its core architectural idea, start here. This article introduces the central pattern behind the framework: HTML Over-the-Wire — reimagined for the ABAP world. It explains how abap2UI5 eliminates traditional frontend complexity by moving both UI rendering and application logic into the ABAP backend. The result: faster development, easier deployment, and a UI5 shell that acts purely as a rendering engine. This article lays the technical foundation for everything else.

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

Several modern frameworks adopt this pattern:
* [htmx](https://htmx.org) Progressive enhancement via HTML partials (Any web stack)
* [Hotwire (Turbo)](https://hotwired.dev) HTML-over-the-wire for Rails apps (Ruby on Rails)
* [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view) Real-time UI with server rendering (Elixir/Phoenix)
* [Livewire](https://livewire.laravel.com) Server-driven UI components in PHP  (Laravel/PHP)
* [Unpoly](https://unpoly.com) Simplified partial page updates (Any web stack)


#### Comparison to Classic SSR

But isn't this just the same as traditional Server-Side Rendering (SSR)?

In the early days of web development SSR was the standard. Every user interaction triggered a full-page reload, and the server responded with a complete HTML document. Within the SAP ecosystem, this approach was seen in technologies like ITS Mobile and SAP GUI for HTML.

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

UI5 freestyle apps follow the Single Page Application model. All UI and app artifacts are stored on the frontend, while the backend provides data via OData — typically based on CDS Views or custom ABAP implementations. Both rendering and logic execution take place entirely in the browser:

<p align="center">
 <img width="500" alt="image" src="https://github.com/user-attachments/assets/8043d0d9-5852-4dac-aefb-37ec8d6e66be" />
<br/>
  <em>UI5 freestyle - UI is built on the client; backend delivers only Data via OData</em>
</p>

Since UI5 is a client-side rendering framework, the HTML output cannot be generated as ready-to-render HTML on the backend. Instead, it must be produced in the browser using the UI5 framework and its built-in renderer.

#### Sending Views from Backend

But how can we generated UI5 HTML in the backend?

Fortunately, UI5 has a defining characteristic that allows us to shift part of the view generation to the backend. In UI5 freestyle apps, each view can be defined in XML — the so-called UI5 XML View. The UI5 framework uses this XML definition, combined with data from the backend, to render HTML in the browser.

<p align="center">
<img width="400" alt="image" src="https://github.com/user-attachments/assets/1ae233c6-96ff-4370-ac31-30705c18a0f7" />
<br/>
  <em>UI5 freestyle – HTML rendered in browser based on frontend XML View and backend Data</em>
</p>

abap2UI5 introduces a subtle but important shift: what if the backend also delivers the XML View?

While HTML rendering still happens on the frontend, both the view definition and the corresponding data are now sent from the backend:

<p align="center">
<img width="400" alt="image" src="https://github.com/user-attachments/assets/7886d77e-acef-4e96-bc0f-ed3728e06358" />
<br/>
  <em>abap2UI5 – HTML rendered in browser based on XML View and Data, both sent from the backend</em>
</p>

The UI5 application remains a single-page application (SPA), but its role changes: it becomes a pure rendering engine for server-defined views and data. 

#### Frontend Events on the Server

How can user interaction be handled in this scenario?

To enable user interaction, a minimal and static UI5 freestyle app is delivered with the initial HTTP request. This app contains just enough logic to forward frontend events and typically acts as a shell application. The interaction model is inspired by the classic PAI/PBO pattern known from SAP GUI applications.

When a user triggers an event (e.g., pressing a button), the event information is sent to the backend, where an ABAP class determines what happens next. All logic is executed entirely on the server:

<p align="center">
<img width="500" alt="image" src="https://github.com/user-attachments/assets/64ed863f-09bf-4634-8688-5b5382595115" />
<br/>
  <em>abap2UI5 – Simple shell app, backend handles all logic</em>
</p>

In standard UI5 freestyle applications, each app requires its own dedicated set of frontend artifacts:

<p align="center">
<img width="300" alt="image" src="https://github.com/user-attachments/assets/9aa09a7f-5931-496d-bf88-d9b34653784c" />
<br/>
  <em>UI5 freestyle – Each application requires its own set of deployed UI and App artifacts</em>
</p>

With abap2UI5, the frontend becomes a static UI5 shell shared across all applications. Views and logic are fully defined and maintained in the backend. Each application is represented by a single ABAP class that generates the view and handles events:

<p align="center">
<img width="400" alt="image" src="https://github.com/user-attachments/assets/2d8b6441-84f3-464c-980f-2773d619af29" />
<br/>
  <em>abap2UI5 – Shared shell app, with each application defined by backend artifacts only</em>
</p>

As a result, every UI5 app becomes a complete ABAP backend project managed through abapGit — eliminating the need for separate frontend deployments entirely.

#### Create & Update Data

So far, we’ve seen how to display data and handle events using a backend-driven approach. But how can user input be processed and changes made in the frontend be transferred back to the backend?

If we continued relying on OData, updates would typically be routed into the OData service layer — bypassing the ABAP class that also defines the view and handles events in abap2UI5.

Let’s take a closer look at a key UI5 feature: the concept of view models. In UI5 freestyle, view models are used to bind attributes such as visible or enabled — allowing control properties in the view to be mapped precisely to model attributes:

<p align="center">
<img width="340" alt="image" src="https://github.com/user-attachments/assets/7eaa09d3-e3f7-4ebb-997d-fc68cc68421f" />
<br/>
  <em>UI5 View Model Concept – UI control properties are bound to View Model attributes</em>
</p>

This leads to the second subtle shift in abap2UI5: Instead of binding Views to OData, abap2UI5 uses a custom view model created entirely in the backend. This model is constructed dynamically after each request — tailored specifically to the current view — and is sent together with the view definition to the frontend:

<p align="center">
<img width="385" alt="image" src="https://github.com/user-attachments/assets/5bb4d351-4f5e-4ba0-a09a-f17883bd25e6" />
<br/>
  <em>abap2UI5 – Backend delivers an XML View and its specifically tailored View Model in a single response</em>
</p>

This means CDS Views and OData services are no longer consumed directly on the frontend. Instead, the complete UI state — both view and model — is sent from the backend in a single response. Any user changes in the UI are then returned to the backend via a lightweight AJAX call containing the updated view model — no OData routing involved.

Developers do not need to manually configure models or bindings. abap2UI5 handles this internally. All that’s required is to expose class attributes using a simple bind method — abap2UI5 handles the rest.

A typical backend response includes both the XML View:

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
And its corresponding View Model:
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

The overall application flow looks like this:

<p align="center">
<img width="600" alt="image" src="https://github.com/user-attachments/assets/f4df9291-c067-495f-bb52-a68e165e15c1" />
<br/>
  <em>abap2UI5 Architecture - UI5 Over-the-Wire</em>
</p>

With the initial request, the static shell app is delivered. After each user interaction, the app calls the backend — in a PAI/PBO-like fashion — to fetch the updated view and model. Frontend and backend remain tightly coupled — not via OData service definitions, but through plain ABAP logic and JSON. The result is a fully backend-driven UI flow.

#### Partial HTML Updates

A core benefit of the HTML Over-the-Wire approach is that only the affected parts of the UI are updated — not the entire page. But can this pattern also be applied in UI5?

In standard UI5 behavior, updating the XML View typically triggers a full re-render. However, abap2UI5 makes partial updates possible by updating only the view model. This enables UI5 to refresh only the relevant UI controls via data binding — without recreating the entire view structure.

Consider this example:

```abap
CLASS z2ui5_cl_app_partial_rerendering DEFINITION PUBLIC CREATE PUBLIC.
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
  <img src="https://github.com/user-attachments/assets/79a8c531-b9a0-4bf4-bb1c-7d9019ef8707" width="500" />
  <br/>
  <em>Partly HTML Rerendering via View Model Updates - Only relevant DOM parts are re-rendered, preserving UI state</em>
</p>

Thanks to UI5’s powerful data binding mechanism, only the modified DOM elements are updated. This preserves the current UI state — such as input focus — and ensures a smooth, uninterrupted user experience. 

The XML View and View Model concept make UI5 a perfect team player for the UI5 Over-the-Wire approach elemenating the need of full client-side re-renders.

#### Conclusion

abap2UI5 brings the simplicity and efficiency of the HTML Over-the-Wire pattern into the ABAP ecosystem:

Key Benefits:
- One Static UI5 Shell App: Delivered with the initial HTTP Get request; shared and consistent across all applications
- Backend-Driven UI Control: UI definitions and business logic are implemented entirely in ABAP classes
- ABAP-Centric Development: Eliminates the need for additional JavaScript or dedicated frontend development
- Simplified Deployment Model: No SPA-specific tooling or build processes; application logic and artifacts are maintained via abapGit and standard transport mechanisms
- Seamless SAP Integration: Fully compatible with UI5 and ABAP, supports ERP and S/4, ABAP Standard & ABAP Cloud 
- Efficient for Business Applications: Ideal for CRUD operations, forms, dashboards, and all typical enterprise use cases

Limitations:
- Not designed for highly interactive or collaborative real-time applications
- Offline functionality or complex client-side interactions are not covered
- Less effective if frontend and backend teams work independently

By relocating UI control to the ABAP backend and using UI5 purely for HTML rendering, abap2UI5 enables fast and efficient development of business applications — without the complexity of SPA architectures.

Happy ABAPing! 🦖🦕

**References:**
- [htmx in a nutshell](https://htmx.org/docs/#introduction)
- [HTML Over the Wire](https://signalvnoise.com/svn3/html-over-the-wire/)
- [Fiori-like web app development in pure ABAP with htmx and Fundamental](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763)
- [UI5 SDK](https://sapui5.hana.ondemand.com/sdk/#/topic/ec699e0817fb46a0817b0fa276a249f8)
