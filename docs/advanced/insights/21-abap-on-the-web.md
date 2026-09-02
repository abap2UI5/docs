# Twenty-Five Years of ABAP on the Web

Worth laying out end to end, because the shape of it is not what most people
remember.

**ITS, 2000.** Dynpro screens rendered as HTML by the server. Every interaction
a full page from the server. Still in warehouses today, on scanners.

**BSP, 2001.** HTML and JavaScript written inside the ABAP stack, with ABAP
embedded for logic. Server-side rendering again, with real control over the
markup.

**Web Dynpro, 2003.** Views, context and controllers defined in ABAP. A
component model rather than a page model — and still the server building the
HTML.

**UI5 Freestyle, 2010.** The rendering moves. JavaScript, XML views and UI5
controls run in the browser; the backend delivers data through OData and stops
having an opinion about the screen.

**RAP and Fiori Elements, 2019.** The UI definition returns to ABAP as CDS
annotations, and a runtime in the browser turns them into the app.

**abap2UI5, 2023.** The backend sends the XML view itself, and a static UI5 app
renders it.

![Twenty-three years, and the one move that did not reverse.](/insights/21-timeline.svg)

*Twenty-three years, and the one move that did not reverse.*

Read as a line, it is not a march away from the server. For the first ten years
ABAP built the HTML. Then rendering left for the browser, where it stays —
because that is where a modern UI belongs, and nobody is proposing otherwise.
What has been moving back ever since is not the rendering but the **definition**
of the screen: first as annotations, then as the view itself.

Which makes Over-the-Wire less of a new idea than a returning one, arriving with
the part the old server-rendered world never had — a real control library in the
browser.

**The screen went to the browser and stayed. The decision about the screen came
home.**

Happy ABAPing! 🦖🦕🦣
