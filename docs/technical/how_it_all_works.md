---
outline: [2, 4]
---
# Behind the Scenes

_Technical Deep Dive into abap2UI5_

This article was originally published on the [SAP Community](https://community.sap.com/t5/technology-blog-posts-by-members/abap2ui5-7-technical-background-under-the-hood-of-abap2ui5/ba-p/13566459).

This article is for developers who want to understand how abap2UI5 works under the hood. It explores how the framework achieves its flexibility — including runtime view generation, generic HTTP handling, and decoupling from OData — and offers insight into its architecture, extensibility, and system internals.

##### 1. HTML Over the Wire

The concept of **"HTML Over the Wire"** inspired one of the core ideas behind abap2UI5. This approach suggests rendering HTML directly on the server and sending it to the browser — without relying on JSON, client-side MVC frameworks, bundling, or transpiling pipelines.

> You can write fast, modern, responsive web applications by generating your HTML on the server, and delivering that directly to the browser. You don't need JSON as an in-between format. You don't need client-side MVC frameworks. You don't need complicated bundling and transpiling pipelines. But you do need to think different. [...]

> This is what HTML Over The Wire is all about. It's a celebration of the simplicity in HTML as the format for carrying data and presentation together, whether this is delivered on the first load or with subsequent dynamic updates.

The SAP community introduced this idea through examples that use the JavaScript library **htmx** to build Fiori-like apps. Unlike typical SPAs where state and logic live on the frontend, the **HTML Over the Wire** principle keeps all application logic and state on the server.

After the initial page load, the server sends only small HTML fragments asynchronously via AJAX to update parts of the page — avoiding full reloads.

<img width="400" alt="HTML 'Over the Wire' Lifecycle" src="https://github.com/user-attachments/assets/a9fde24a-c572-4e5c-b203-59a0667b9931" />

_HTML "Over the Wire" Lifecycle [(Source)](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763)_

This approach contrasts with the common separation of concerns, where HTML, CSS, and JavaScript are managed independently on the frontend while the backend only delivers data.

##### 2. Hypermedia-Driven App

This concept evolves into a Hypermedia-Driven Application (HDA). In HDAs, the browser focuses only on rendering HTML, CSS, and JavaScript without knowledge of the application's state. All logic lives on the server.

By contrast, SPAs define all routes and actions upfront on the frontend, requiring a full rebuild for any modification. The illustration below compares MPAs, SPAs, and HDAs:

<img width="600" alt="MPA vs. SPA vs. HDA" src="https://github.com/user-attachments/assets/8117dc10-f0ba-4c52-9d1d-6b9d0986401d" />

_MPA vs. SPA vs. HDA [(Source)](https://craftcms.com/events/dot-all-2022/sessions/a-practical-guide-to-html-over-the-wire)_

##### 3. Rethinking Separation of Concerns

Unlike traditional architectures, HDAs don't enforce strict separation of CSS, JavaScript, and HTML. The backend generates the UI and handles program flow, much like SAP GUI apps did in the past. This centralized approach simplifies customization and maintenance.

##### 4. Dive Deeper

Frameworks like Phoenix LiveView (2018) and Laravel Livewire (2019) were among the first to adopt this principle. Tools like htmx, hotwire, and unpoly followed, aiming to reduce complexity while keeping high UI fidelity. These frameworks seek a "sweet spot" between SPA and MPA architectures:

<img width="600" alt="'Sweet Spot' between SPA and MPA" src="https://github.com/user-attachments/assets/41af4a41-829e-4289-82f5-18ee7408054b" />

_"Sweet Spot" between SPA and MPA_

A recommended video gives an excellent introduction to these ideas.

##### 5. UI5 Architecture

UI5 apps typically follow an SPA architecture. The backend delivers data via OData, while all logic and UI rendering happen on the frontend. But one specific characteristic deserves a closer look: how the UI5 framework creates views. UI5 renders each HTML output from an XML-View (ignoring the older HTML/JS/JSON-Views), with its associated data from the server. The frontend stores the view as part of the app:

<img width="600" alt="UI5 normally - ABAP delivers only Data" src="https://github.com/user-attachments/assets/3b2a884e-e899-4b60-8a95-79b418f33657" />

UI5 normally - ABAP delivers only Data

##### 6. abap2UI5 Architecture

abap2UI5 introduces a key change: the backend also sends the view. This shifts the frontend's role toward an HDA, displaying views and data received from the server:

<img width="600" alt="'UI5 Over the Wire' - ABAP delivers Data & View together" src="https://github.com/user-attachments/assets/9717f500-c0de-4428-a996-11fc131c073c" />

"UI5 Over the Wire" - ABAP delivers Data & View together

Despite still relying on frontend HTML rendering, the app now retrieves all the needed information (view & data) via AJAX from the backend. The UI5 app stays a SPA, but its role shrinks to that of an HDA — responsible only for displaying the view and its data:

<img width="600" alt="UI5 app downgraded to an HDA - Displaying Data & View received from the server" src="https://github.com/user-attachments/assets/17a3a301-b698-4704-9cbc-43798c5bd600" />

UI5 app downgraded to an HDA - Displaying Data & View received from the server

This means the frontend app doesn't know what it's currently displaying (whether a table, list, or input), nor does it know which actions come next. The app logic stays entirely on the server, and the first request delivers the frontend app as just a small HDA:

<img width="600" alt="'UI5 Over the Wire' - Server to Client Communication" src="https://github.com/user-attachments/assets/2c9f8dc1-c6d8-4e93-80a2-b50bfc1d5ec1" />

"UI5 Over the Wire" - Server to Client Communication

The HDA displays the view with its data and sends each event back to the server, which determines the next action and output. This resembles the PAI/PBO process from classic SAP GUI apps:

<img width="600" alt="UI5 vs. 'UI5 Over the Wire' - Communication" src="https://github.com/user-attachments/assets/3b464d0b-19fd-400c-a7e4-3eec893f7724" />

UI5 vs. "UI5 Over the Wire" - Communication

We use AJAX roundtrip logic similar to "HTML Over the Wire" approaches, but here we can't send HTML directly. Instead, we send a View combined with its Data. This produces a concept we could call "UI5-View Over the Wire".

##### 7. Merging Data & Presentation

A typical "UI5-View Over the Wire" response looks like this:

<img width="600" alt="'UI5 Over the Wire' - Response with View & Data together" src="https://github.com/user-attachments/assets/d52112e6-b9b7-4e7f-ac7f-825c20620240" />

"UI5 Over the Wire" - Response with View & Data together

But is this the same as RAP, just in a different format?

##### 8. RAP

RAP also aims to find a "sweet spot" between SPA and MPA. The exact mechanism RAP uses to bring its view and model to the frontend isn't fully transparent, but it enriches responses either in the JSON itself or in the metadata of the initial OData request — and CDS Views in the backend define the view and model beforehand:

<img width="600" alt="RAP - Definition of Views with UI Annotations" src="https://github.com/user-attachments/assets/a79f07ff-594d-422c-b66f-8acf8058c81a" />

RAP - Definition of Views with UI Annotations

<img width="600" alt="RAP - Definition of Data Models with DDL" src="https://github.com/user-attachments/assets/66b8935f-f23a-4b08-bd1d-6ec79f220499" />

RAP - Definition of Data Models with DDL

This approach also leads to an architecture with a thin frontend and a strong backend similar to an HDA. But RAP aims to achieve this in a well-organized, controlled manner: every API relies on the OData protocol, UI annotations define the views, DDL defines the data models, local implementations of RAP classes handle model updates, and everything splits into different layers orchestrated in a Virtual Data Model. This approach ensures a highly organized development process that works effectively in most use cases.

However, when significant Model and View changes are needed at runtime, this approach can prove too inflexible. Model changes with RTTI aren't supported, and extending the view quickly exceeds the functional scope of backend annotations — requiring development of apps with Fiori Elements (which again requires extra deployment).

Overall, RAP doesn't mix View, Model, and Logic as radically as the "Over the Wire" approaches. Luckily, in an open-source project we don't need to worry about conventions and can take a few more risks. As we saw in (6), the first trick was sending Views from the backend instead of storing them on the frontend app — now we can push flexibility even further (9)(10).

##### 9. One HTTP-Service for All Apps

First, we don't define a specific HTTP-Service for transmitting the View and Data. Instead, every app uses the same generic HTTP-Handler with two strings (one for the View and one for the Data), removing the need to develop individual OData-Services with SEGW or CDS. At runtime, the framework transforms ABAP variables & tables into a JSON-Model and transmits them as a string to the frontend. JavaScript then parses it back into a JSON-Model and binds it to the UI5-View:

<img width="600" alt="Data Transfer in abap2UI5 - ABAP variables & tables are automatically synchronized with the UI5-Model" src="https://github.com/user-attachments/assets/163ca12b-fe37-43e8-80b6-a5eaae703d69" />

Data Transfer in abap2UI5 - ABAP variables & tables are automatically synchronized with the UI5-Model

Also, we don't just send the data but also the metadata (Data Model) with every request (7). This differs from classic OData communication, where the initial OData request carries the metadata to establish the model upfront, and only data moves afterward. With this approach, we can send different models with every request:

<img width="600" alt="OData vs. UI5 Over the Wire - Model & Data transfer" src="https://github.com/user-attachments/assets/95fe59c3-7e8a-4e21-8690-12de1110779f" />

OData vs. UI5 Over the Wire - Model & Data transfer

##### 10. Define Model at Runtime

This lets us define models not only at design time, but also at runtime. The user needs no extra work because abap2UI5 handles the entire process in the background during every AJAX request:

<img width="600" alt="abap2UI5 - Dynamic Data Binding & Model Creation" src="https://github.com/user-attachments/assets/e9f1bf8c-6d8c-44ad-ba89-c3648b638335" />

abap2UI5 - Dynamic Data Binding & Model Creation

In apps, we can now use RTTI again, similar to how ALVs used it. We no longer need separate apps for each model. In this demo, you can see an abap2UI5 app with a view that contains a table output whose type we create and modify at runtime (similar to SE16):

![SE16-like runtime table where the data model is generated at runtime via RTTI](https://github.com/user-attachments/assets/20b4a140-7954-45b0-8d0e-8aa1e8a6f1f5)

Replacing the Model (metadata) at Runtime

##### 11. Define View at Runtime

The same applies to the view: in RAP, you can modify only certain predefined control attributes at runtime, while CDS artifacts with UI annotations define the view itself upfront. In an abap2UI5 app, however, you can replace entire view controls. For example, in the following app, we swap a table control for a list control and vice versa:

![Swapping a table control for a list control at runtime from the ABAP backend](https://github.com/user-attachments/assets/b6e081e4-2eae-4175-aca8-fc761b145762)
Replacing the View at Runtime

##### 12. View & Model Independent of the HTTP-Service

As a result, the View & Model stay independent of the HTTP-Service, and we're no longer forced to deliver a predefined static OData-Service for every app, as RAP does. This significantly reduces the number of backend artifacts:

<img width="600" alt="RAP vs. Model & View decoupled from the (single & generic) HTTP-Service" src="https://github.com/user-attachments/assets/6fb61790-87bc-47fa-855e-83d5292b70f3" />

RAP vs. Model & View decoupled from the (single & generic) HTTP-Service

Let's look at the HTTP-Handler that gives us this flexibility.

##### 13. HTTP-Service

All apps and data models use the same single generic HTTP-Handler — you can see this by setting a breakpoint in your app and examining the call stack.

<img width="600" alt="Call stack of an abap2UI5 app" src="https://github.com/user-attachments/assets/1ce80652-4105-4ee5-84e8-35a87eb47556" />

Call stack of an abap2UI5 app

Every app implementation is a REST-based HTTP POST implementation that keeps no session between requests.

##### 14. REST

This makes it compatible with all mobile use cases and devices, as well as with 'RESTful' environments such as the BTP ABAP Environment and the new 'ABAP Cloud' language version. Similar to an OData implementation, where the app reflects data changes without a restart, you can now develop the entire application and modify its view without restarting the frontend app. See this demo:

![Live editing the ABAP class with the UI updating without reloading the frontend app](https://github.com/user-attachments/assets/c2c1afce-7d72-46a2-b0a7-7725c70bf5f4)

Developing the ABAP class without restarting the frontend app

We also get the advantage shared by all over-the-wire approaches: no more cache busting, since the frontend app stays unchanged during the development process.

So far, we've observed that the abap2UI5 frontend app is unaware of the specific application, just like the generic HTTP-Service on the server, which also has no knowledge of the particular model and view it's transmitting. So, which layer ultimately defines what happens in this architecture?

##### 15. The abap2UI5 App

The only non-generic part of this concept is the user's app, which implements the `z2ui5_if_app` interface:

<img width="600" alt="abap2UI5 app - one place for everything" src="https://github.com/user-attachments/assets/167b1078-14d7-4354-9561-f4e5c7345544" />

abap2UI5 app - one place for everything

In this architecture, the app has complete freedom in creating the view and the model, but it also bears full responsibility for making sure everything else functions correctly. The app must handle the program logic, application states, and remember where it came from and where it wants to go next. All of this sits in this single app layer.

However, this isn't a problem for ABAP! From an ABAP perspective, it resembles past practices of using selection screens or working with ALVs. Every SAP GUI app was essentially an HDA where ABAP performed all the needed functions (just not in a browser-based environment). In this architecture, we're no longer limited to implementing an OData-Service or confined to a local implementation of a global RAP class with restrictions such as commit sequences. We can now tap the full capabilities of the ABAP stack again. Creating data models based on internal tables is straightforward; working with generic data models, as seen in (10), comes easily at runtime with RTTI; and extended ABAP concepts like serialization also apply, as we'll see in the next section.

##### 16. Draft

With RAP, users can save interim results in drafts, letting them pause and resume their work later. The abap2UI5 architecture works as if we send a completely new app to the frontend after every event, but we still want to preserve the user's previous inputs and state. To achieve this, the `z2ui5_if_app` interface includes the `if_serializable_object` interface, which lets us serialize and persist all important information from every request (such as the current view or its status):

<img width="600" alt="z2ui5_t_draft - the abap2UI5 persistence for interim results" src="https://github.com/user-attachments/assets/fc13f32d-3145-4510-a2d8-a0b646fdd6c4" />

z2ui5_t_draft - the abap2UI5 persistence for interim results

Furthermore, these drafts help us jump back to previous states with minimal effort, reducing the complexity we would typically encounter in an HDA scenario when implementing a cancel or exit event of a view. Like the HTTP-Service, these drafts are also generic, removing the need to create typed draft tables for every data model manually, as RAP requires, and further reducing the number of backend artifacts:

<img width="600" alt="RAP vs. Single (generic) Draft Table in abap2UI5" src="https://github.com/user-attachments/assets/c32335ae-6d10-4b12-9fd1-786a0da595fe" />

RAP vs. Single (generic) Draft Table in abap2UI5

With this approach, we achieve a stateful-like PAI/PBO feel similar to SAP GUI apps, even though we're still operating within the AJAX roundtrip logic. Furthermore, since every request can go to a different application server, abap2UI5 is compatible with scalable cloud environments, ready for future compatibility:

<img width="600" alt="SAP GUI (stateful) vs. abap2UI5 (restful)" src="https://github.com/user-attachments/assets/0c62e222-06b7-4f16-af2d-3663fd6df796" />

SAP GUI (stateful) vs. abap2UI5 (restful)

However, use this feature only for interim results; be cautious when serializing other parts of your app.

We've gained significant flexibility with (9) (10) (11) (16); the next sections focus on how the framework reduces its complexity, starting with the initial request.

##### 17. Initial Request

The first GET request sends the artifacts of the UI5 (HDA) app to the browser. Typically, we would deploy a BSP to the ABAP stack for this, but in abap2UI5, the code lives as a string inside the HTTP-Handler's initial-request implementation:

<img width="600" alt="index.html stored in ABAP Source Code instead of using a BSP" src="https://github.com/user-attachments/assets/e69d5a12-c0e3-4e17-be8a-4da3bc740c97" />

index.html stored in ABAP Source Code instead of using a BSP

This gives us a 100% abapGit project that uses only ABAP source code, making it easy to install on every ABAP system by removing separate frontend artifacts or deployments.

##### 18. Everything Is Maintained & Developed in the Backend

Since all user apps are also in pure ABAP, we can now maintain and develop everything in the backend. Duplicating, editing, renaming, or other refactoring takes just a few moments. The deployment process shrinks to activating an ABAP class, letting us create many apps quickly. For example, we built all the apps in the sample section fast, mostly by copy-pasting, which would have been impractical for separately developed and deployed frontend apps. This is a major reduction in complexity and an advantage of all 'Over the Wire' apps, as we observed in (3).

##### 19. No Extra Layer

Another way to reduce complexity: avoid creating extra customizing layers. As shown in (13), only one stack call sits between the user's app and the HTTP-Handler, and no additional layers such as OData, SADL, or Gateway exist. This lets us bring the UI5 frontend framework and its functionality as pure as possible to the abap2UI5 apps in the backend.

UI5 evolves rapidly, and additional layers can quickly become outdated. With this approach, all future UI5 controls work automatically in abap2UI5. However, a potential downside is that we have to handle the complexity of the frontend UI5 API and learn the concepts of XML views and UI5 controls. Ultimately, it's a matter of personal preference: learn UI annotations, or learn the concepts of SAP UI5 directly.

##### 20. No Hiding of Complexity

But having no extra layer also means the framework doesn't necessarily abstract away complexity — unlike what other frameworks aim for. In abap2UI5, you send your XML-View directly to the frontend, and you're responsible for making sure it's valid and executable:

<img width="600" alt="XML-View created by the user and ready for the 'Wire'" src="https://github.com/user-attachments/assets/cbfdc72f-31f1-460b-afa3-d03179e9b173" />

XML-View created by the user and ready for the 'Wire'

Luckily, utility classes significantly simplify the creation process. For instance, `z2ui5_cl_xml_view` offers a class-based approach to create views with access to the UI5 API via ADT code completion:

<img width="600" alt="z2ui5_cl_xml_view - UI5 API (frontend) used for Code Completion in ADT (backend)" src="https://github.com/user-attachments/assets/b8aa5f41-d958-4181-bdc3-bc92a4a57b4b" />

z2ui5_cl_xml_view - UI5 API (frontend) used for Code Completion in ADT (backend)

This contrasts with RAP, where you benefit from well-documented and organized extra layers, though sometimes they have limited functionality. Take side effects, for example: RAP restricts you to the `+`, `-`, and `*` operators. In abap2UI5, you write JavaScript directly, which takes much more knowledge, but in return you get access to the full expression binding functionality on the frontend:

<img width="600" alt="Expression Binding (Side Effects) in abap2UI5 - Mixture of ABAP and JavaScript" src="https://github.com/user-attachments/assets/c8be7e94-c4e1-445e-b1f4-f79d81d421ac" />

Expression Binding (Side Effects) in abap2UI5 - Mixture of ABAP and JavaScript

##### 21. Separated `_bind` and `_event` Methods
In the first approach of this framework, every method call included the event and data binding:

<img width="600" alt="First approach - Data binding and events are not separated from the view" src="https://github.com/user-attachments/assets/3bc268e0-e08f-40b3-b152-b3fa375c0faf" />

First approach - Data binding and events are not separated from the view

In the current approach, we separate them from the view and create them via additional methods. The framework then handles the entire process of data binding and transfer automatically (9):

<img width="600" alt="Actual Approach - extra methods for the event and binding" src="https://github.com/user-attachments/assets/4708b4c1-a031-48d5-823c-5b8434a98c0c" />

Actual Approach - extra methods for the event and binding

This differs from many other UI rendering processes, which usually handle data and UI together. Separating them here simplifies the view creation process, avoids data redundancies, and keeps the framework from becoming messy. The current approach uses fewer lines of code than the first approach (which only focused on selection screens), because it now cleanly separates the entire view creation process from the rest and keeps it outside the framework.

##### 22. "Over the Wire" Sending JS, HTML & CSS

We can also add extra functionality (JS, HTML, CSS) without extending the framework itself or changing the abap2UI5 frontend app. For instance, take the Upload Files App — it has its own custom control that isn't part of the framework and ships "Over the Wire" after calling the app:

<img width="600" alt="App delivering its own JavaScript 'Over the Wire'" src="https://github.com/user-attachments/assets/5960c1c9-1675-440f-80f9-a3e52db31c1c" />

App delivering its own JavaScript "Over the Wire"

With any request, you can send your own JavaScript or Custom Controls to the frontend. The abap2UI5 framework forwards it as-is. All subsequent requests can then use this JavaScript — for example, to use Custom Controls in their UI5 Views:

<img width="600" alt="abap2UI5 app sending custom JavaScript to the client" src="https://github.com/user-attachments/assets/66345bd4-6208-4dfc-9870-c82e3a45f74a" />

abap2UI5 app sending custom JavaScript to the client

##### 23. As Simple as Possible

As we saw in (22), we can make apps very complex, but the opposite is also possible — we can make them very simple. One beautifully minimalistic approach uses `if_oo_adt_classrun`. By implementing a single method, we can generate output with one click (`F9`). This is highly efficient and inspired abap2UI5. Here's a comparison of both approaches:

<img width="600" alt="if_oo_adt_classrun vs. abap2UI5" src="https://github.com/user-attachments/assets/28a09830-ba3a-4608-aab9-5f4af8028a18" />

if_oo_adt_classrun vs. abap2UI5

To summarize what we've covered so far: abap2UI5 is built in a highly generic manner, placing most of the responsibility on the user's apps. As a result, we gain considerable flexibility and freedom in the app implementation, but we also take on full responsibility for the view creation and the program flow. We also have to keep the following downsides in mind.

##### 24. Downsides Compared to UI5 & RAP

Most notably, compared to UI5, we can't implement offline capabilities — in such scenarios, we can't continually ask the server after every event to determine what happens next.

Also, using HANA DB capabilities directly on the frontend causes problems. Since every app uses the same generic HTTP-Service, we've decoupled the UI from the rest. RAP, by contrast, uses a typed OData and can directly tap into HANA capabilities via a CDS View (and skip the ABAP layer). This approach lets you easily add pagination or fuzzy searches to UI5 freestyle or RAP apps. An OData-Service directly calling a CDS View of HANA proves highly effective here.

We can also select from CDS Views in an abap2UI5 app and send the result to the frontend. But doing this manually takes more effort, and we can't render a fuzzy search help on the frontend — this approach forces us to replace the entire view after every request. As always, every advantage we gain with abap2UI5 (like flexibility in creating models) comes with a trade-off of reduced capability elsewhere.

Also, Fiori Elements with all its floorplans & templates is very straightforward and will receive many updates in the future. Ultimately, the wide range of UI5 use cases makes comparing the different approaches difficult — and we can't fully discuss them here. Let's now look at the framework's codebase in the final part of this blog post.

##### 25. System Footprint

We keep the system footprint minimal — abap2UI5 relies only on ABAP classes, with no CDS or RAP artifacts. We delegate most of the code to the user outside the framework (21) (22). Overall, the framework comes to around 2,300 lines of code spread over one HTTP-Handler, two interfaces, and one database table:

<img width="600" alt="System footprint of abap2UI5" src="https://github.com/user-attachments/assets/981ab684-d2cf-4f56-b25c-c333db3c6dcc" />

System footprint of abap2UI5

The HTTP-Handler class implements the entire framework logic:

<img width="600" alt="This is all that abap2UI5 does" src="https://github.com/user-attachments/assets/9c54e6b9-18a0-4582-a8cf-345d41d61a00" />

This is all that abap2UI5 does

The framework focuses only on communication between backend and frontend, controlling the application flow, and creating the view model. We saw the initial GET request in (17). This implementation handles the POST request:

<img width="600" alt="AJAX POST Handler" src="https://github.com/user-attachments/assets/d8276aed-f339-4084-97aa-b769a55d73c8" />

AJAX POST Handler

The result is a pure source-code-based framework, which opens up the following possibility.

##### 26. Running Everywhere Apps

At its core, abap2UI5 generates two strings — one containing an XML-View and the other containing the JSON-View-Model. The framework transmits these strings to and from the frontend. As a result, you don't need a recent ABAP release — this works even on old releases. This approach runs on both the latest ABAP Cloud stack and on-premise systems, as well as legacy ones, making it a release-independent solution. Also, we don't necessarily lose access to new UI5 features — we can bootstrap the UI5 framework on the frontend from a Content Delivery Network (CDN) and use the latest UI5 version even on older releases:

<img width="600" alt="Local Bootstrapping - UI5 version depends on the SAP release" src="https://github.com/user-attachments/assets/ff4e308f-4b34-4981-9d8a-ecda39039720" />

Local Bootstrapping - UI5 version depends on the SAP release

<img width="600" alt="CDN Bootstrapping - UI5 version independent of the SAP release" src="https://github.com/user-attachments/assets/a8477539-45c4-4a43-a53b-c4078d5057aa" />

CDN Bootstrapping - UI5 version independent of the SAP release

As a result, you can develop abap2UI5 apps that are portable across various SAP systems, releases, and environments. An app developed once on ABAP Cloud 2305 can also run on earlier releases. Similarly, apps developed on older NetWeaver releases can run on BTP ABAP Environment or S/4 Public Cloud ABAP Environment. However, to achieve this compatibility, abap2UI5 and its apps must work with both language versions, 'ABAP Cloud' and 'Standard ABAP'. To avoid redundancy, abap2UI5 achieves this via a single code line.

##### 27. One-Code-Line

With this approach, dependencies stay limited to APIs and functions that are both cloud-released and available in earlier NetWeaver releases. To handle this, abap2UI5 uses SAP dependencies only when strictly needed — for instance, in GUID creation:

<img width="600" alt="GUID creation compatible with ABAP Cloud and Standard ABAP" src="https://github.com/user-attachments/assets/8039f152-1f39-4428-93a3-6cf80b38da5d" />

GUID creation compatible with ABAP Cloud and Standard ABAP

As you can see, creating methods compatible with both 'ABAP Cloud' and 'Standard ABAP' is much more complex. Fortunately, abap2UI5 only requires GUIDs as a dependency. However, when developing apps, you must stay aware of this (and practical feasibility remains to be seen). It does have one key advantage: abap2UI5 runs on ABAP 2305 and remains portable down to NetWeaver 7.02.

##### 28. Compatibility & Downporting

Downporting abap2UI5 code normally would produce a release difficult to maintain and debug. To avoid this, abap2UI5 splits into two repositories: a main repository (compatible from NW 7.50 to ABAP 2305) and a downport repository (compatible down to NW 7.02).

abaplint automatically generates the low-syntax branch. The separate branch enables development with all new ABAP expressions available since ABAP 7.50 while still guaranteeing that all future abap2UI5 features get automatically downported and made available for ABAP 7.02.

Automated ABAP downporting greatly improves efficiency. Check out the abaplint dashboard of this project and the abaplint tool. Beyond abaplint, this framework uses abapGit and benefits from the work of the people who built it:

<img width="600" alt="abaplint dashboard and abapGit - tools powering abap2UI5 downporting" src="https://github.com/user-attachments/assets/b0ae2acd-9446-48ca-9459-13d4bffa8f72" />

##### 29. Summary

Long blog post short: inspired by "HTML Over the Wire" (1)(2)(3), we mixed UI and Data together (7) and created a "UI5 Over the Wire" approach by sending the XML-View from the server (6). We then used a single generic HTTP-Service for all apps (13), independent of the View and Data Model (12). This gives us great flexibility, letting us dynamically create Data Models (10) and Views (11) at runtime, which significantly reduces the number of backend artifacts.

Next, we explored various ideas on how the framework reduces its own complexity by avoiding frontend artifacts (17), removing extra customizing layers (19), and separating the view from the framework (21), as well as app-specific JS or HTML (22). The result is a pure source-code approach with only one database table, two interfaces, one class, and just 2,300 lines of code (25). We develop it in a single code line (27), making it cloud and on-premise ready and downportable to old releases (28). Together with abapGit, you can develop its apps to run on nearly every release.

Overall, with abap2UI5, you need to disregard some common rules: no separation between View and Model in the HTTP communication (12), HTML & JavaScript live directly in the source code (17) (22), we don't use OData or RAP (7), and there are other downsides to consider (24). However, if you accept all of this, you get a highly minimalistic approach where you only need to implement a single method to develop standalone UI5 applications (15).

Happy ABAPing!
