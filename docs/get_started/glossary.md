---
outline: [2, 2]
description: The words this manual uses for one thing each - roundtrip, app instance, app draft, view, model, binding, event, frontend action, sub-app, user exit - each in two sentences.
---
# Glossary

The manual uses one word for one thing. This page says what each word means,
in two sentences, and links the page that explains it in full.

## The Words

| Word | What it means |
| --- | --- |
| **App** | One ABAP class that implements `z2ui5_if_app`. It has one method, `main`. → [Hello World](/get_started/hello_world) |
| **`main`** | The one method of an app. The framework calls it once for every click. → [Life Cycle](/cookbook/event_navigation/life_cycle) |
| **`client`** | The object `main` receives. Every call an app makes to the framework goes through it: show a view, bind a field, read the event. → [Client API](/resources/api) |
| **Roundtrip** | One click, from start to end: the browser sends a request, `main` runs once, the answer comes back. One click is one roundtrip is one HTTP request. |
| **App instance** | The object of your app class while `main` runs. It lives for one roundtrip only. |
| **App draft** | The saved copy of the app instance. After every roundtrip the framework writes the instance to the table `z2ui5_t_01`, and reads it back before the next one, so the attributes keep their values. Often just "the draft". **It is not a RAP draft**: it holds no business data model, and it has nothing to do with `with draft` in a behavior definition. → [RAP draft handling](/cookbook/eml_cds_sql/draft_handling) is the other meaning |
| **Stateless** | The normal mode: no ABAP session waits between two clicks, the app draft carries the state. **Stateful** is the exception, where one session stays open for one user. → [Statefulness](/cookbook/expert_more/statefulness) |
| **View** | The screen, written as a UI5 XML view. The app builds it as a string and hands it to `client->view_display( )`. → [View Definition](/cookbook/view/definition) |
| **View builder** | The class `z2ui5_cl_ui5_view_builder` that writes the XML for you. Four methods: `ele( )` adds a control and moves into it, `tag( )` adds a control and stays, `a( )` sets one attribute (`n` = name, `v` = value), `end( )` moves back up. |
| **Model** | The data the view shows, sent to the browser as JSON. It is made of the attributes the app has bound. |
| **Binding** | The link between a public attribute of the class and a property of a control. `_bind( )` shows the value and, for an input control, brings back what the user typed. → [Data Binding](/cookbook/model/binding) |
| **Event** | What the user did, sent to `main`: a button press, a row click. `_event( )` attaches an event name to a control, `check_on_event( )` asks for it. → [Backend events](/cookbook/event_navigation/backend) |
| **Frontend action** | Something the browser does without calling `main`: set the focus, copy to the clipboard, open a URL. Called with `follow_up_action( )`, as a statement or as the value of an event attribute. → [Frontend actions](/cookbook/event_navigation/frontend) |
| **Sub-app** | An app started by another app with `nav_app_call( )`. When it ends with `nav_app_leave( )`, the calling app continues. → [Navigation](/cookbook/event_navigation/navigation/inner_app) |
| **Popup, popover** | A dialog over the view, and a small bubble next to one control. Each is a view of its own, shown with `popup_display( )` and `popover_display( )`. → [Popup](/cookbook/popup_popover/popup) |
| **Frontend** | The part that runs in the browser: one generic UI5 app that shows whatever view and model it receives. It is the same for every app and contains no app code. |
| **HTTP handler** | The ABAP class behind the endpoint. It passes every request to the framework. You create it once, in the [Quickstart](/get_started/quickstart). |
| **Endpoint** | The URL of the HTTP service that serves abap2UI5: an ICF node on an on-premise system, an HTTP service in ABAP Cloud. |
| **User exit** | A class of your own that implements `z2ui5_if_ui5_exit`. It configures the installation: UI5 source, theme, security settings. It is not a classic SAP user exit. → [User Exits](/advanced/extensibility/user_exits) |
| **Startup page** | The page the endpoint shows when no app is named. It checks a class name and starts the app. |

## Words You May Know from Elsewhere

| If you know | then in abap2UI5 |
| --- | --- |
| PBO and PAI of a dynpro | one call of `main`: it reacts to the event and builds the answer |
| a selection screen parameter | a public attribute bound to an input with `_bind( )` |
| `CALL SCREEN` | `nav_app_call( )` |
| a UI5 controller | the app class |
| an OData service per app | nothing: one endpoint serves every app |
| a RAP draft | still a RAP draft, used through EML. The *app draft* is something else, see above |
