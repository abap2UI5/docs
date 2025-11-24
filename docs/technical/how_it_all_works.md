#  Behind the Scenes

_Technical Deep Dive into abap2UI5_

This article was originally published on the [SAP Community.](https://community.sap.com/t5/technology-blog-posts-by-members/abap2ui5-7-technical-background-under-the-hood-of-abap2ui5/ba-p/13566459)

It's intended for developers who want to understand how abap2UI5 works under the hood. We'll explore how the framework achieves its flexibility — including runtime view generation, generic HTTP handling, and decoupling from OData — and provide insight into its architecture, extensibility, and system internals.

##### 1. HTML Over the Wire

One of the core ideas behind abap2UI5 is inspired by the concept of **"HTML Over the Wire"**. This approach suggests rendering HTML directly on the server and sending it to the browser — without relying on JSON, client-side MVC frameworks, bundling, or transpiling pipelines.

> You can write fast, modern, responsive web applications by generating your HTML on the server, and delivering that directly to the browser. You don’t need JSON as an in-between format. You don’t need client-side MVC frameworks. You don’t need complicated bundling and transpiling pipelines. But you do need to think different. [...]

> This is what HTML Over The Wire is all about. It’s a celebration of the simplicity in HTML as the format for carrying data and presentation together, whether this is delivered on the first load or with subsequent dynamic updates.

The idea was introduced in the SAP community through examples using the JavaScript library **htmx** to build Fiori-like apps. Unlike typical SPAs where state and logic live on the frontend, the **HTML Over the Wire** principle keeps all application logic and state on the server.

After the initial page load, only small HTML fragments are sent asynchronously via AJAX to update parts of the page — avoiding full reloads.

<img width="400" alt="image" src="https://github.com/user-attachments/assets/a9fde24a-c572-4e5c-b203-59a0667b9931" />
  
_HTML "Over the Wire" Lifecycle [(Quelle)](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763)_


This approach contrasts with the common separation of concerns, where HTML, CSS, and JavaScript are managed independently on the frontend while the backend only delivers data.

##### 2. Hypermedia Driven App

This concept evolves into what is termed a Hypermedia-Driven Application (HDA). In HDAs, the browser focuses solely on rendering HTML, CSS, and JavaScript without knowledge of the application's state. All logic is maintained on the server.

In contrast, SPAs define all routes and actions upfront on the frontend, requiring a full rebuild for any modification. The following illustration compares MPAs, SPAs, and HDAs:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/8117dc10-f0ba-4c52-9d1d-6b9d0986401d" />

_MPA vs. SPA vs. HDA [(Quelle)](https://craftcms.com/events/dot-all-2022/sessions/a-practical-guide-to-html-over-the-wire)_

##### 3. Rethinking Separation of Concerns

Unlike traditional architectures, HDAs don't prioritize strict separation of CSS, JavaScript, and HTML. The backend generates the UI and handles program flow, much like SAP GUI apps did in the past. This centralized approach simplifies customization and maintenance.

##### 4. Dive Deeper

Frameworks like Phoenix LiveView (2018) and Laravel Livewire (2019) were among the first to adopt this principle. Tools like htmx, hotwire, and unpoly followed, aiming to reduce complexity while maintaining high UI fidelity. These frameworks seek a "sweet spot" between SPA and MPA architectures:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/41af4a41-829e-4289-82f5-18ee7408054b" />

_"Sweet Spot" between SPA and MPA_

A recommended video offers an excellent introduction to these ideas.

##### 5. UI5 Architecture

UI5 apps typically follow an SPA architecture. The backend delivers data via OData, while all logic and UI rendering happen on the frontend. But there's one specific characteristic we should look at closely: how the UI5 framework creates views. Each HTML output is rendered from an XML-View (let's ignore the former HTML/JS/JSON-Views), with its associated data from the server. The view is stored on the frontend as part of the app:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/3b2a884e-e899-4b60-8a95-79b418f33657" />

UI5 normally - ABAP delivers only Data

##### 6. abap2UI5 Architecture

abap2UI5 introduces a pivotal change: the backend also sends the view. This shifts the frontend’s role towards an HDA, displaying views and data received from the server:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/9717f500-c0de-4428-a996-11fc131c073c" />

"UI5 Over the Wire" - ABAP delivers Data & View together

Despite still relying on frontend HTML rendering, all the necessary info (view & data) is now retrieved via AJAX from the backend. The UI5 app remains a SPA, but its role is now reduced to that of an HDA – responsible solely for displaying the view and its data:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/17a3a301-b698-4704-9cbc-43798c5bd600" />

UI5 app downgraded to an HDA - Displaying Data & View received from the server

This means the frontend app doesn't know what it's currently displaying (whether it's a table, list, or input) and it doesn't know what actions will be taken next. The app logic stays completely on the server, and the frontend app is just a small HDA transmitted with the first request:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/2c9f8dc1-c6d8-4e93-80a2-b50bfc1d5ec1" />

"UI5 Over the Wire" - Server to Client Communication

The HDA displays the view with its data and sends back each event to the server for determination of the next action and output. This process is somewhat similar to the PAI/PBO process used in former SAP GUI apps:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/3b464d0b-19fd-400c-a7e4-3eec893f7724" />

UI5 vs. "UI5 Over the Wire" - Communication

We use AJAX roundtrip logic similar to "HTML Over the Wire" approaches, but here we can't send HTML directly. Instead, we send a View combined with its Data. This results in a concept we could call "UI5-View Over the Wire".

##### 7. Merging Data & Presentation

A typical "UI5-View Over the Wire" response looks like this:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/d52112e6-b9b7-4e7f-ac7f-825c20620240" />

"UI5 Over the Wire" - Response with View & Data together

But is this just the same as RAP, but in a different format?

##### 8. RAP

RAP also aims to find a "sweet spot" between SPA and MPA. I'm not certain of the exact approach they use to bring their view and model to the frontend, but they enrich responses either within the JSON itself or within the metadata of the initial OData request – and the view and model are defined beforehand in CDS Views in the backend:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/a79f07ff-594d-422c-b66f-8acf8058c81a" />

RAP - Definition of Views with UI Annotations

<img width="600" alt="image" src="https://github.com/user-attachments/assets/66b8935f-f23a-4b08-bd1d-6ec79f220499" />

RAP - Definition of Data Models with DDL

This approach also leads to an architecture with a thin frontend and a strong backend similar to an HDA. But RAP aims to achieve this in a well-organized and controlled manner: Every API is based on the OData-Protocol, Views are defined with UI Annotations, Data Models are defined in DDL, Model updates are developed in local implementations of RAP classes and everything is separated in different layers that are orchestrated in a Virtual Data Model. Finally, this approach ensures a highly organized development process which is effective in most use cases.

However, when significant Model and View changes are needed at runtime, this approach can be a bit too inflexible. Model changes with RTTI aren't supported, and extending the view quickly goes beyond the functional scope of backend annotations – requiring development of Apps with Fiori Elements (with the need for extra deployment again).

Overall, RAP doesn't mix View, Model, and Logic as radically as the "Over the Wire" approaches. Luckily, in an open-source project we don't need to worry about conventions and can take a few more risks. As we saw in (6), the first trick was sending Views from the backend instead of storing them on the frontend app – now we can enhance flexibility even further (9)(10).

##### 9. One HTTP-Service for all Apps

First, we don't define a specific HTTP-Service for transmitting the View and Data. Instead, every app uses the same generic HTTP-Handler with two strings (one for the View and one for the Data), eliminating the need to develop individual OData-Services with SEGW or CDS. During runtime, ABAP variables & tables are transformed into a JSON-Model and transmitted as a string to the frontend. In JavaScript, it's parsed back into a JSON-Model and bound to the UI5-View:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/163ca12b-fe37-43e8-80b6-a5eaae703d69" />

Data Transfer in abap2UI5 - ABAP variables & tables are automatically synchronized with the UI5-Model

Furthermore, we don't just send the data but also the metadata (Data Model) with every request (7). This differs from classic OData communication, where metadata is sent with the initial OData request to establish the model upfront, and only data is exchanged afterward. With this approach, we can send different models with every request:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/95fe59c3-7e8a-4e21-8690-12de1110779f" />

OData vs. UI5 Over the Wire - Model & Data transfer

##### 10. Define Model at Runtime

This enables the possibility to define models not only at design time, but also at runtime. The user doesn't have to do any extra work because abap2UI5 handles the entire process in the background during every AJAX request:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/e9f1bf8c-6d8c-44ad-ba89-c3648b638335" />

abap2UI5 - Dynamic Data Binding & Model Creation

In apps we can use RTTI now again in a way that is similar to how it was used with ALVs. This means that there is no need to create separated apps for each model. In this demo, you can see an abap2UI5 app with a view including a table output that displays a generic table and its type is created and modified at runtime (similar to SE16):

![gif_se16_2](https://github.com/user-attachments/assets/20b4a140-7954-45b0-8d0e-8aa1e8a6f1f5)

Replacing the Model (metadata) at Runtime

##### 11. Define View at Runtime

Same for the view: In RAP, only certain predefined control attributes can be modified at runtime, while the view is defined in CDS artifacts with UI annotations previously. However, in an abap2UI5 app, it is possible to replace entire view controls. For example, in the following app, a table control is replaced with a list control and vice versa:

![gif_ui_change2-1](https://github.com/user-attachments/assets/b6e081e4-2eae-4175-aca8-fc761b145762)
Replacing the View at Runtime

##### 12. View & Model independent from the HTTP-Service

In the end, the View & Model are defined independent from the HTTP-Service and we are no longer forced to deliver a predefined static OData-Service for every app, as is the case in RAP. The number of backend artifacts is significantly reduced:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/6fb61790-87bc-47fa-855e-83d5292b70f3" />

RAP vs. Model & View decoupled from the (single & generic) HTTP-Service

Let's take a look to the HTTP-Handler that provides us with this flexibility.

##### 13. HTTP-Service

All apps and data models use the same single generic HTTP-Handler, which can be observed by setting a breakpoint in your app and examining the call stack.

<img width="600" alt="image" src="https://github.com/user-attachments/assets/1ce80652-4105-4ee5-84e8-35a87eb47556" />

Call stack of an abap2UI5 app

Every app implementation is a REST-based HTTP-Post implementation, in which no session is maintained between two requests.

##### 14. REST

This makes it compatible with all mobile use cases and devices, as well as with 'RESTful' Environments such as the BTP ABAP Environment and the new language version 'ABAP Cloud'. Similar to an OData-Implementation, where data changes are reflected in the app without requiring an app restart, it is now possible to develop the entire application and modify its view without restarting the frontend app. Take a look at this demo:

![gif_dev](https://github.com/user-attachments/assets/c2c1afce-7d72-46a2-b0a7-7725c70bf5f4)

Developing the ABAP class without restarting the frontend app

We get also the advantage shared by all over-the-wire approaches that there is no need for cache busting anymore, as the frontend app remains unchanged during the development process.

Up until now, we have observed that the abap2UI5 frontend app is unaware of the specific application, just like the generic HTTP-Service on the server, which has also no knowledge of the particular model and view it is transmitting. So, which layer ultimately defines what happens in this architecture?

##### 15. The abap2UI5 App

The only non-generic part of this concept is the app of the user implementing the interface z2ui5_if_app:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/167b1078-14d7-4354-9561-f4e5c7345544" />

abap2UI5 app - one place for everything

In this architecture, the app has complete freedom in creating the view and the model, but it also bears full responsibility for ensuring that everything else functions correctly. The app must handle the program logic, application states, and remember where it was coming from and where it want to go next. All of this is concentrated in this single app layer.

However, this is not a big deal for ABAP! From an ABAP perspective, this is similar to past practices of using selection screens or working with ALVs. Every SAP GUI app was, in a way, an HDA where ABAP performs all the necessary functions (it was just not a browser-based environment). Moreover, in this architecture, we are not limited to implementing an OData-Service or confined to a local implementation of a global RAP class with restrictions, such as commit sequences, anymore. We can now leverage the full capabilities of the ABAP stack again. Creating data models based on internal tables is straightforward, working with generic data models, as seen in (10), is easily achievable at runtime with RTTI and extended ABAP concepts like serialization are also applicable, as we will see in the next section.

##### 16. Draft

With RAP, users can save interim results in drafts, giving them the opportunity to interrupt their work and continue later. The abap2UI5 architecture works as if we send a completely new app to the frontend after every event, but we still want to preserve the inputs and state that the user has made before. To achieve this, the z2ui5_if_app interface includes the if_serializable_object interface, which enables us to serialize and persist all important information of every request (such as the current view or its status):

<img width="600" alt="image" src="https://github.com/user-attachments/assets/fc13f32d-3145-4510-a2d8-a0b646fdd6c4" />

z2ui5_t_draft - the abap2UI5 persistence for interim results

Furthermore, these drafts help us jump back to previous states with minimal effort, reducing the complexity that we would typically encounter in an HDA scenario when implementing a cancel or exit event of a view. Similar to the HTTP-Service, these drafts are also defined only in a generic way, eliminating the need to manually create typed draft tables for every data model, as required in RAP, and reducing again the number of backend artifacts:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/c32335ae-6d10-4b12-9fd1-786a0da595fe" />

RAP vs. Single (generic) Draft Table in abap2UI5

With this approach, we achieve a stateful-like PAI/PBO feeling similar to SAP GUI apps, even though we are still operating within the AJAX roundtrip logic. Furthermore since every request can be made to a different application server, abap2UI5 is compatible with scalable cloud environments, ensuring compatibility for future use:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/0c62e222-06b7-4f16-af2d-3663fd6df796" />

SAP GUI (stateful) vs. abap2UI5 (restful)

However, it is important to note that this feature should only be used for interim results and be cautious when serializing other parts of your app.

We have gained a lot of flexibility with (9) (10) (11) (16), now the next sections will focus more on how the framework tries to reduce its complexity. Let's begin by taking a look at the initial request.

##### 17. Initial Request

The first GET request sends the artifacts of the UI5 (HDA) app to the browser. Typically, we would deploy a BSP to the ABAP stack for this, but in abap2UI5, the code is copied as a string into the implementation of the initial request of the HTTP-Handler:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/e69d5a12-c0e3-4e17-be8a-4da3bc740c97" />

index.html stored in ABAP Source Code instead of using a BSP

This provides us a 100% abapGit project that solely uses ABAP source code, making it easily installable on every ABAP system by eliminating the need for separated frontend artifacts or deployments.

##### 18. Everything is maintained & developed in the Backend

Considering the fact that all user apps are also in pure ABAP, we can now maintain and develop everything in the backend. Duplicating apps, making changes, renaming or other refactoring takes only a few moments. The deployment process is reduced to just activating an ABAP class, enabling us to create many apps in a short amount of time. For example, all the apps of the sample section were created rapidly using mostly copy-pasting, which would have been unfeasible for separately developed and deployed frontend apps. This represents a significant reduction in complexity and an advantage of all 'Over the Wire' apps, as we observed in (3).

##### 19. No Extra Layer 

Another way to reduce complexity is by avoiding the creation of extra customizing layers. As shown in (13), there is only one stack call between the user's app and the HTTP-Handler, and there are no additional layers such as OData, SADL or Gateway. This allows us to bring the UI5 frontend framework and its functionality as pure as possible to the abap2UI5 apps in the backend.

UI5 is evolving rapidly, and additional layers can quickly become outdated. With this approach, all UI5-Controls which will be released in the future will also be automatically useable in abap2UI5. However, a potential downside is that we have to deal with the complexity of the frontend UI5 API and learn the concepts of XML-Views and UI5 Controls. Ultimately, it comes down to personal preference whether you prefer to learn UI Annotations or directly learn the concepts of SAP UI5.

##### 20. No Hiding of Complexity

But not having an extra layer also means the framework doesn't necessarily abstract away complexity – unlike what other frameworks aim for. In abap2UI5, you directly send your XML-View to the frontend and you're responsible for ensuring it's valid and executable:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/cbfdc72f-31f1-460b-afa3-d03179e9b173" />

XML-View created by the user and ready for the 'Wire'

Luckily, we can significantly simplify the creation process by creating utility classes. For instance, by offering with z2ui5_cl_xml_view a class-based approach to create views that provide access to the UI5 API via ADT code completion:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/b8aa5f41-d958-4181-bdc3-bc92a4a57b4b" />

z2ui5_cl_xml_view - UI5 API (frontend) used for Code Completion in ADT (backend)

This is in contrast to RAP, where you benefit of well-documented and organized extra layers, but sometimes they have limited functionality. Take side effects for example. In RAP, you are restricted to use the +, -, and * operators. In abap2UI5 you have to write JavaScript directly, which requires a lot more knowledge, but it provides the benefit of accessing the full expression binding functionality available at the frontend:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/c8be7e94-c4e1-445e-b1f4-f79d81d421ac" />

Expression Binding (Side Effects) in abap2UI5 - Mixture of ABAP and JavaScript

##### 21. Separated _bind and _event method
In the first approach of this framework the event and data binding were included in every method call:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/3bc268e0-e08f-40b3-b152-b3fa375c0faf" />

First approach - Data binding and events are not separated from the view

In the current approach, they are separated from the view and created using additional methods. Moreover, the entire process of data binding and transfer is automatically handled by the framework (9):

<img width="600" alt="image" src="https://github.com/user-attachments/assets/4708b4c1-a031-48d5-823c-5b8434a98c0c" />

Actual Approach - extra methods for the event and binding

This is a difference from many other UI rendering processes, where data and UI are usually imported together. Separating them here simplifies the view creation process, avoids data redundancies, and prevent the framework from becoming messy. The current approach has fewer lines of code than the first approach that only focused on selection screens, because the entire view creation process is clearly separated from the rest now and kept outside of the framework.

##### 22. "Over the Wire" sending JS, HTML & CSS 

Furthermore, we can add extra functionality (JS, HTML, CSS) without extending the framework itself or changing the abap2UI5 frontend app. For instance, take the Upload Files App – it has its own custom control that isn't part of the framework and is sent "Over the Wire" after calling the app:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/5960c1c9-1675-440f-80f9-a3e52db31c1c" />

App delivering its own JavaScript "Over the Wire"

With any request, there's the chance to send your own JavaScript or Custom Controls to the frontend. The abap2UI5 framework just sends it as-is to the frontend. All upcoming requests can now use this JavaScript – for example, to use Custom Controls in their UI5 Views:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/66345bd4-6208-4dfc-9870-c82e3a45f74a" />

abap2UI5 app sending custom Javascript to the client

##### 23. As simple as possible

So, we have seen in (22), apps can be made very complex, but the opposite is also possible - we can make them extremely simple. One beautifully minimalistic approach is the use of if_oo_adt_classrun. By implementing just one method, we can generate an output with a single click (F9). This is extremely efficient and was one of the role models for abap2UI5. Here's a comparison of both approaches:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/28a09830-ba3a-4608-aab9-5f4af8028a18" />

if_oo_adt_classrun vs. abap2UI5

To summarize what we have covered so far, abap2UI5 is built in a highly generic manner, placing most of the responsibility on the user's apps. As a result, we gain a lot of flexibility and freedom in the app implementation, but we also have full responsibility for the view creation and the program flow. Furthermore we have to keep the following downsides in mind.

##### 24. Downsides compared to UI5 & RAP

Most notably, compared to UI5, we can't implement offline capabilities – in such scenarios we can't continuously ask the server after every event to determine what happens next.

Furthermore, using HANA DB capabilities directly at the frontend leads to problems. By using the same generic HTTP-Service for every app, we have decoupled the UI from the rest. However, in a RAP scenario, they use a typed OData and can directly touch HANA capabilities via a CDS View (and skip the ABAP layer). With this approach, pagination or fuzzy searchs can be easily integrated in UI5 freestyle or RAP apps. The combination of OData-Service directly calling a CDS View of HANA is extremely effective here.

Of course, we can also select from CDS Views in an abap2UI5 app and send the result to the frontend. But implementing this manually requires more effort, and we can't render a fuzzy search help on the frontend – we're forced to replace the entire view after every request with this approach. As always, every advantage we gain with abap2UI5 (like flexibility in creating models) comes with a trade-off of lower functionality in other areas.

Additionally, Fiori Elements with all its floorplans & templates is very straightforward and will get lots of updates in the future. In the end, the wide range of UI5 use cases makes comparing the different approaches very difficult – and can't be fully discussed here. Now, let's continue to the last part of this blog post and take a closer look at the framework's code base.

##### 25. System Footprint

We keep the system footprint as small as possible – abap2UI5 is based only on ABAP classes without CDS and RAP artifacts. Most of the coding is delegated to the user outside of the framework (21) (22). In total, the framework consists of around 2,300 lines of code spread over one HTTP-Handler, two interfaces, and one database table:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/981ab684-d2cf-4f56-b25c-c333db3c6dcc" />

System footprint of abap2UI5

The entire framework logic is implemented in the HTTP-Handler class:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/9c54e6b9-18a0-4582-a8cf-345d41d61a00" />

This is all that abap2UI5 does

The functionality focuses solely on the communication between the backend and frontend, controlling the application flow, and creating the view model. We saw the initial GET request in (17). The POST request is handled by this implementation:


<img width="600" alt="image" src="https://github.com/user-attachments/assets/d8276aed-f339-4084-97aa-b769a55d73c8" />

AJAX Post Handler

In the end, we get a pure source code-based framework, which offers us the following possibility.

##### 26. Running Everywhere Apps

Essentially, abap2UI5 generates two strings – one containing an XML-View and the other containing the JSON-View-Model. These strings are transmitted to and from the frontend. As a result, you don't need a high ABAP release – this can be done even with very old releases. This approach lets us run on both the latest ABAP Cloud stack and on-premise systems, as well as very old releases, making it a release-independent solution. Additionally, we don't necessarily lose access to new UI5 features – we can bootstrap the UI5 framework on the frontend from a Content Delivery Network (CDN) and use the latest UI5 version even on very old releases:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/ff4e308f-4b34-4981-9d8a-ecda39039720" />

Local Bootstrapping - UI5 version depends on the SAP release


<img width="600" alt="image" src="https://github.com/user-attachments/assets/a8477539-45c4-4a43-a53b-c4078d5057aa" />

CDN Bootstrapping - UI5 version independent from the SAP release

As a result, abap2UI5 apps can also be developed to be portable across various SAP systems, releases, and environments. If an app is developed once on ABAP Cloud 2305, it can also be used on lower releases. Similarly, apps developed on older Netweaver releases can run on BTP ABAP Environment or S/4 Public Cloud ABAP Environment. However, for this compatibility to be possible, abap2UI5 and its apps need to be designed to work with both language versions, 'ABAP Cloud' and 'Standard ABAP'. To avoid redundancy, abap2UI5 tries to achieve this by using a single code line.

##### 27. One-Code-Line

With this approach, the use of dependencies is limited to cloud-released APIs and functions available in lower Netweaver releases simultaneously. To handle this, abap2UI5 only uses SAP dependencies when it is really needed -- for instance in the GUID creation:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/8039f152-1f39-4428-93a3-6cf80b38da5d" />

GUID creation compatible to ABAP Cloud and Standard ABAP

As you can see, creating methods that are compatible with both 'ABAP Cloud' and 'Standard ABAP' is considerably more complex. Fortunately, abapUI5 only requires GUIDs as a dependency. However, when developing apps, you must be aware of this (and I have no experience if this is feasable). But in the end, it does have a key advantage: abap2UI5 runs on ABAP 2305 and is still portable down to NetWeaver v702.

##### 28. Compatibility & Downporting

Downporting abap2UI5 code normally would result in a release that is difficult to maintain and debug. To avoid this, abap2UI5 is divided into two repositories: a main repository (compatible from NW 7.50 to ABAP 2305) and a downport repository (compatible down to NW 7.02).

The low-syntax branch is automatically generated using abaplint. The separate branch enables development with all new ABAP expressions available since ABAP v750 while still ensuring that all abap2UI5 features added in the future are automatically downported and available for ABAP v702.

The functionality of automated ABAP downporting is impressive and greatly improves efficiency. Check out the abaplint dashboard of this project and the tool abaplint. Besides abaplint this framework uses abapGit and benefits from the work of the people who built it:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/b0ae2acd-9446-48ca-9459-13d4bffa8f72" />

##### 30. Summary

Long blog post short: Inspired by "HTML Over the Wire" (1)(2)(3) we mixed UI and Data together (7) and created an "UI5 Over the Wire" approach by sending the XML-View from the server (6). Then we used a single generic HTTP-Service for all apps (13) independent from the View and Data Model (12). It provides us with great flexibility allowing us to dynamically create Data Models (10) and Views (11) at runtime, resulting in a significantly reduced number of backend artifacts.

Next, we explored various ideas on how the framework reduces its own complexity by avoiding frontend artifacts (17), eliminating extra customizing layers (19), and separating the view from the framework (21), as well as app-specific JS or HTML (22). Finally, we got a pure source code approach with only one database table, two interfaces, one class and just 2,300 lines of code (25). It is developed in a single code line (27), making it cloud and on-premises ready and downportable to old releases (28). Its apps in combination with abapGit can be developed that they are running on nearly every release (29).

All in all, with abap2UI5, you need to disregard some common rules: there is no separation between View and Model in the HTTP communication (12), HTML & JavaScript are stored directly in the source code (17) (22), we don't use OData or RAP (7) and there are other downsides to consider (24). However, if you accept all of this, you get a very minimalistic approach where you only need to implement a single method to develop standalone UI5 applications (15).

Happy ABAPing! ❤️🦖🦕🦣
