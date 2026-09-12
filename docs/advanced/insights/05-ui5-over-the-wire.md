# #5 UI5 Over-the-Wire

A UI5 freestyle app is a single-page application. The view is deployed with the
app in the frontend, the backend delivers data through OData, and the browser
puts the two together and renders the HTML. Everybody knows this split; most
SAP frontends alive today are built on it.

But there is a detail inside it that is easy to walk past. UI5 does not render
from JavaScript objects assembled by hand — it renders from an **XML view**,
bound to its data. The view is a document. And documents, unlike compiled
applications, can travel.

So: what if the backend sent the view as well?

That is the entire idea. Really — that is the whole framework, and everything
else in this series is a consequence of it. abap2UI5 answers every request with
two strings. An XML view:

```xml
<mvc:View xmlns="sap.m" xmlns:mvc="sap.ui.core.mvc">
  <Page title="abap2UI5 - Hello World">
    <Input value="{/NAME}"/>
    <Button press=".eB(['BUTTON_POST'])" text="post"/>
  </Page>
</mvc:View>
```

and the model that fills it:

```json
{ "MODEL": { "NAME": "test" } }
```

![The browser sends an event, the backend answers with a view and its model, the browser renders both.](/insights/05-two-strings.svg)

*The browser sends an event, the backend answers with a view and its model, the browser renders both.*

Nothing there is a protocol abap2UI5 invented. The XML is UI5's own view
format, the JSON is an ordinary UI5 JSON model, and the frontend does what it
has always done — build HTML from a view and its data. What changes is who owns
the view. It is no longer an artifact deployed beside the app; it is a string an
ABAP class produced for this request, and the next request may produce a
different one.

The pattern has a name outside SAP, and a fan club. **HTML Over-the-Wire** —
the server renders, the browser inserts, and JSON never becomes the in-between
format. htmx, Hotwire, Phoenix LiveView, Livewire and Unpoly are all built on
it, and the arguments for it read remarkably like an ABAP developer describing
a dialog step.

It is not a return to full-page reloads either — the first objection anybody
raises. ITS Mobile and SAP GUI for HTML answered every interaction with
a whole document; Over-the-Wire replaces fragments and leaves the page
standing. UI5 cannot take HTML off the wire — it renders in the browser by
design — so what travels is the layer directly above it. A view, not a page.

The frontend stopped being an application and became a renderer, and most of
what follows in this series comes out of that one move.
