# REST: Where Did My Sessions Go?



##### 14. REST

This makes it compatible with all mobile use cases and devices, as well as with 'RESTful' Environments such as the BTP ABAP Environment and the new language version 'ABAP Cloud'. Similar to an OData-Implementation, where data changes are reflected in the app without requiring an app restart, it is now possible to develop the entire application and modify its view without restarting the frontend app. Take a look at this demo:

![gif_dev](https://github.com/user-attachments/assets/c2c1afce-7d72-46a2-b0a7-7725c70bf5f4)

Developing the ABAP class without restarting the frontend app

We get also the advantage shared by all over-the-wire approaches that there is no need for cache busting anymore, as the frontend app remains unchanged during the development process.

Up until now, we have observed that the abap2UI5 frontend app is unaware of the specific application, just like the generic HTTP-Service on the server, which has also no knowledge of the particular model and view it is transmitting. So, which layer ultimately defines what happens in this architecture?


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
