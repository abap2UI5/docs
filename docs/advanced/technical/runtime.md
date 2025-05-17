# Runtime


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