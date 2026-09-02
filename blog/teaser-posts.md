# LinkedIn teaser posts

The posts that introduce each article. Plain text — LinkedIn renders no
markdown. Kept here rather than on the published pages: they are publishing
material, not documentation.

## Somewhere on the Way to UI5, We Lost RTTS

Plain text — LinkedIn renders no markdown. The article link comes from the
attached article preview, so the post carries no inline URL. 744 characters.

> In the past, screens were routinely built for tables nobody had seen.
> cl_salv_table=>factory( ) took any internal table and drew it — RTTS read the
> structure at runtime, the DDIC supplied the labels, and a whole category of
> ABAP tooling was built that way.
>
> With Fiori Elements, RAP or freestyle UI5, that path goes through a typed
> OData service: the entity type has to exist first, at design time. Exactly
> right when the client does not know your backend — but a dead end for those
> tools.
>
> New article 🎉 abap2UI5 can give a runtime-typed model a UI5 face, and be a
> perfect complement to the UI5 and RAP solutions you already run.
>
> Where do you still use RTTS in UIs today? And how do you get around the
> design-time bottleneck?
>
> #ABAP #SAP #UI5

## abap2UI5 is not a Programming Model

Plain text — LinkedIn renders no markdown.

> The useful question about a UI framework is not what it can do. It is what it
> wants from you: a structure to follow, a lifecycle to fit into, layers to fill
> in.
>
> abap2UI5 fits its answer on a page — one interface, one method. No data model,
> no service, no binding, no annotations, no BSP per app, no frontend artefact
> to transport.
>
> Which is why it composes instead of competing. The new article shows one app
> and three save handlers: EML against a business object, MODIFY against a
> table, and a BAPI call. The framework never learns which — it could just as
> well be the EWM classes, or whatever SAP releases next.
>
> New article 🎉
>
> What does your UI framework ask of your architecture?
>
> #ABAP #SAP #UI5

## The Cost of a Screen

Plain text — LinkedIn renders no markdown.

> A maintenance view for a customising table. A cockpit showing what last
> night's job did. An approval step for one department.
>
> The logic behind each is about thirty lines. The cost of putting a UI in front
> of thirty lines is not thirty lines, and it does not scale down: a data model,
> a service, a binding, a frontend artefact, a deployment — and an object that
> has to be transported, survive upgrades, and one day be deprecated by someone
> who never met the department that asked for it.
>
> So the screen never gets built. Every system has a Z package full of the ones
> that became a selection screen and an ALV grid instead.
>
> New article 🎉 with a job monitor as a complete abap2UI5 app — and an honest
> note on where this is the wrong trade.
>
> Which screen in your system stayed an ALV grid because a proper UI was never
> worth the effort?
>
> #ABAP #SAP #UI5

## No Annotation in Between

Plain text — LinkedIn renders no markdown.

> A metadata-driven screen is described, not written — and the description has a
> vocabulary. What the vocabulary covers arrives in an afternoon. What it does
> not becomes an extension point.
>
> abap2UI5 has no vocabulary of its own to run out of. It sends a UI5 XML view,
> so the vocabulary is the UI5 control library itself, all of it, at whatever
> version the launchpad already serves. The new article builds a three-step
> sap.m.Wizard in one ABAP class to show what that means in practice.
>
> The price is on the same page: nothing writes the layout for you.
>
> New article 🎉
>
> Which control did you last want, and not have a way to ask for?
>
> #ABAP #SAP #UI5

## UI5 Over-the-Wire

Plain text — LinkedIn renders no markdown.

> UI5 does not render from objects you assemble by hand. It renders from an XML
> view, bound to its data. The view is a document — and documents can travel.
>
> So what if the backend sent the view too? That is the whole of abap2UI5: every
> request is answered with two strings, a UI5 XML view and a UI5 JSON model.
> Neither is a protocol the framework invented, and the frontend does what it
> always did.
>
> What changes is who owns the view. Not an artefact deployed beside the app —
> a string an ABAP class produced for this request.
>
> New article 🎉
>
> Where would you draw the line between a frontend and a renderer?
>
> #ABAP #SAP #UI5

## The Frontend That Does Not Know What It Shows

Plain text — LinkedIn renders no markdown.

> The UI5 app that abap2UI5 sends to the browser does not contain the screen.
> Not the fields, not the table, not the buttons. It is a shell: enough UI5 to
> render a view and a model that arrive from the server, and to send events
> back.
>
> It cannot tell whether it is showing a table or a wizard, and it does not know
> which action follows the next click. PBO builds, PAI decides — the dialog step
> just became an AJAX call.
>
> Which means there is one shell, shared by every app in the system. Not one
> deployed frontend per app, each pinned to whichever UI5 version someone last
> had time for.
>
> New article 🎉
>
> How many separately deployed frontends does your system carry right now?
>
> #ABAP #SAP #UI5

## One Service for Every App

Plain text — LinkedIn renders no markdown.

> Count what one screen usually costs in backend artefacts: a CDS view or two, a
> service definition, a service binding, a behavior definition and its
> implementation — each named, transported, reviewed, and each belonging to
> exactly one app.
>
> An abap2UI5 app adds none of them. Every app is served by the same generic
> handler, which does not know the app, the view or the model — it moves two
> strings. Set a breakpoint and look at the call stack: one frame.
>
> It can be generic because nothing about the data is agreed in advance. The
> model travels with every response, so every response may carry a different one.
>
> New article 🎉
>
> How many of the objects in your last transport existed only to get one screen
> onto a display?
>
> #ABAP #SAP #UI5

## Only the Changed Part

Plain text — LinkedIn renders no markdown.

> If the backend sends the view on every request, does the screen rebuild itself
> on every click?
>
> It would — and the user would feel it. A rebuilt view is a new set of
> controls: focus lost, half-typed input gone, table scrolled back to the top.
>
> So the view is not sent every time. It is one IF in the app. When it is
> skipped, the response carries only the model, and UI5 data binding updates
> exactly the controls bound to what changed. No diffing, no virtual DOM, no
> reconciler — a mechanism UI5 has had all along.
>
> New article 🎉
>
> Where has a full re-render cost you a user's input?
>
> #ABAP #SAP #UI5

## PUBLIC Means Persisted

Plain text — LinkedIn renders no markdown.

> Every abap2UI5 request lands on a fresh instance of the app class. No session
> holds the previous one, and the next request may not even reach the same app
> server.
>
> So how does what the user typed three clicks ago still exist? The app class is
> serializable, and after each request the framework serializes it into a draft
> table. What survives is exactly the PUBLIC SECTION.
>
> Which makes one keyword a performance decision. A catalogue parked in a public
> attribute is not cached — it is written to the database and shipped to the
> browser after every click. State public, working data protected.
>
> New article 🎉
>
> Where in your code does a keyword quietly decide what travels?
>
> #ABAP #SAP #UI5

## Swapping the View at Runtime

Plain text — LinkedIn renders no markdown.

> In abap2UI5 the view is a string the app produced for this request. So what
> stops the next request from producing a different one?
>
> Nothing does. One IF in the app class, and the table is a list — not a table
> with hidden columns, not a second app behind a navigation step. A different
> control, chosen in ABAP.
>
> A screen assembled at design time varies only where somebody anticipated
> variation and left a switch. A screen assembled per request varies wherever
> the code branches.
>
> New article 🎉
>
> Which screen in your system would you rebuild differently per user, if it cost
> you an IF?
>
> #ABAP #SAP #UI5

## index.html Lives in a String

Plain text — LinkedIn renders no markdown.

> Somebody has to deliver the first HTML page. In the SAP world that normally
> means a BSP: a frontend artefact, built somewhere, deployed, transported on
> its own path, invalidated from its own caches.
>
> abap2UI5 does not have one. The initial GET is answered from ABAP source —
> the page is a string in the handler.
>
> So the project is nothing but ABAP: abapGit and activate, no npm install, no
> bundler, no dist folder. No build output to go stale, so no cache to
> invalidate. And no dependency tree that somebody has to audit, pin and answer
> for.
>
> New article 🎉
>
> How much of your last frontend incident was the pipeline rather than the code?
>
> #ABAP #SAP #UI5

## Where Your Own JavaScript Goes

Plain text — LinkedIn renders no markdown.

> Sooner or later an app needs what an XML view cannot express: a chart library,
> an unwrapped control, or a UI5 method that exists only as a method —
> sap.m.Carousel moves through setActivePage( ) and nothing else.
>
> abap2UI5 has no plugin system for that, on purpose. It has three declared
> seams: an imperative method reached by control id from ABAP, a custom control
> living in its own BSP behind a reserved resource root, and extra JavaScript
> for the initial page set once through the framework exit.
>
> None lets an app change the framework. None makes the framework change for an
> app.
>
> New article 🎉
>
> When you last needed a custom control, what did it cost to get it in?
>
> #ABAP #SAP #UI5

## Four Verbs

Plain text — LinkedIn renders no markdown.

> The first view builder in abap2UI5 had one method per UI5 control — hundreds
> of them, properties as named parameters. Good reason: ADT code completion
> listed them, so the frontend API was imported into backend tooling.
>
> Hard edge: a control the class had no method for could not be written at all.
> Every new UI5 control was a pull request.
>
> Its successor has four verbs — ele, tag, a, end. None of them names a control,
> which is exactly why every control is reachable. The completion list is gone;
> the compiler and the linter still check the chain.
>
> New article 🎉
>
> Wrapper or raw API — which side do you land on, and why?
>
> #ABAP #SAP #UI5

## The Class That Runs

Plain text — LinkedIn renders no markdown.

> The smallest ABAP program that does something is a class with
> if_oo_adt_classrun: one interface, one method, F9. No transaction, no program,
> nothing beside it.
>
> abap2UI5 copied that silhouette exactly. Same shape, different destination —
> it renders in a browser, follows the Fiori guidelines, and goes to a colleague
> as a URL instead of "open ADT and press F9".
>
> Which leaves a property that has quietly become valuable: the whole app is one
> file. Anything that has to reason about it — a reviewer, a successor, a search,
> an agent — can hold all of it at once.
>
> New article 🎉
>
> What is the smallest complete app in your system, measured in files?
>
> #ABAP #SAP #UI5

## Where the Selection Screen Went

Plain text — LinkedIn renders no markdown.

> Before anyone called it full-stack, ABAP had PARAMETERS. Four lines and every
> layer is there: a typed, labelled input with a default and value help, plus
> validation and event handling. Nobody wrote a frontend, because declaring the
> variable was writing the frontend.
>
> That trade is what disappeared on the way to the browser: the variable and the
> field became two artefacts in two places, kept in step by a service in between.
>
> _bind puts them back together. The attribute is not a field name in a string —
> it is the ABAP variable.
>
> New article 🎉
>
> How many places does one input field touch in your current stack?
>
> #ABAP #SAP #UI5

## No Cache, No Deploy, Any IDE

Plain text — LinkedIn renders no markdown.

> The abap2UI5 loop is: change the class, activate, refresh the browser.
>
> What is missing from that sentence is the point. No deployment — activating is
> the deployment. No cache to invalidate — the UI is built per request, so
> nothing can be stale. No IDE agreement, because it is ABAP. No context switch
> to debug: a breakpoint in the method that built the view stops there.
>
> Individually, conveniences. Together, the reason a screen gets tried at all —
> when an experiment costs a class and a refresh, "could we just show this?"
> stops being a project.
>
> New article 🎉
>
> What is your current edit-to-see-it time on a Fiori change?
>
> #ABAP #SAP #UI5

## One Codebase, 7.02 to ABAP Cloud

Plain text — LinkedIn renders no markdown.

> Most SAP landscapes are two landscapes right now: something old running the
> business, something new beside it, and a migration measured in years. Anything
> written for one side usually gets written again for the other.
>
> abap2UI5 needs almost nothing from the release, because it only produces two
> strings and moves them over HTTP. The one real SAP dependency is GUID
> creation, written for both language versions in a single code line — and
> abaplint generates the 7.02 branch automatically, so the downport cannot drift.
>
> UI5 comes from a CDN, so the UI5 version is configuration, not a property of
> the release.
>
> New article 🎉
>
> How much of what you build today will survive your next release upgrade?
>
> #ABAP #SAP #UI5

## 2,300 Lines

Plain text — LinkedIn renders no markdown.

> The communication core of abap2UI5 is one HTTP handler, two interfaces and one
> database table — originally around 2,300 lines of ABAP.
>
> It is small because of what it does not do. It does not build views, apps do.
> It does not decide flow, apps do. It does not wrap UI5 controls, so it does
> not grow when UI5 does.
>
> Which matters past elegance: a framework in the request path sees every input,
> every response, every user. There is a difference between trusting a package
> because it is popular and being able to open it and find out.
>
> New article 🎉
>
> When did you last read a framework you depend on?
>
> #ABAP #SAP #UI5

## Where the Line Is

Plain text — LinkedIn renders no markdown.

> Eighteen articles is enough credit to say what abap2UI5 does not do.
>
> Offline is out — every event asks the server what happens next; take the
> server away and there is no app left. Pushdown to HANA is indirect, and a
> frontend fuzzy search help is not available. Real-time and collaborative UIs
> are the wrong shape for a request-per-event model. Separate frontend and
> backend teams lose the contract between them. And where a Fiori Elements
> floorplan fits, it fits — that is less work, not more.
>
> What is left is still most business software: forms, tables, dashboards,
> approvals, the small screens nobody funds a project for.
>
> New article 🎉
>
> Where would you put the line?
>
> #ABAP #SAP #UI5

## Cloud-Ready Is a Property of Your App

Plain text — LinkedIn renders no markdown.

> abap2UI5 is ABAP Cloud compliant: written in ABAP for Cloud, released APIs
> only, no modifications, installs on BTP and S/4HANA Public Cloud.
>
> None of that says anything about the app you build with it.
>
> SELECT FROM i_salesorder, or SELECT FROM vbak. Same screen, same framework —
> one survives the next upgrade and can move to a cloud system, the other
> cannot. Nothing about the UI is involved in the difference.
>
> Clean core is not a property a dependency grants your application. It is
> decided in the SELECT.
>
> New article 🎉
>
> Where does your team actually check this — review, ATC, or after the upgrade?
>
> #ABAP #SAP #UI5

## Twenty-Five Years of ABAP on the Web

Plain text — LinkedIn renders no markdown.

> ITS 2000. BSP 2001. Web Dynpro 2003. UI5 Freestyle 2010. RAP and Fiori
> Elements 2019. abap2UI5 2023.
>
> Read as a line, it is not a march away from the server. For the first ten
> years ABAP built the HTML. Then rendering left for the browser — and it stays
> there, because that is where a modern UI belongs.
>
> What has been moving back ever since is not the rendering but the definition
> of the screen: first as annotations, then as the view itself.
>
> New article 🎉
>
> Which of these six did you write your first web app in?
>
> #ABAP #SAP #UI5

## Where the View Lives

Plain text — LinkedIn renders no markdown.

> Three ways to get a UI5 screen in front of a user on an ABAP stack. All three
> render in the browser, same control library, same framework. What differs is
> where the view is defined — and therefore when it is fixed.
>
> UI5 Freestyle: a file in a frontend project, fixed at build time.
> RAP with Fiori Elements: UI annotations on CDS, fixed when they are activated.
> abap2UI5: an XML string an ABAP class produced for this request.
>
> Everything else follows from that axis — what gets deployed per app, what a
> screen costs in artefacts, what can still change at runtime. Not a ranking:
> fixed early standardises well, fixed late adapts well.
>
> New article 🎉
>
> Where does the view live in the app you are working on today?
>
> #ABAP #SAP #UI5

## RAP or abap2UI5 — When to Use Which

Plain text — LinkedIn renders no markdown.

> RAP or abap2UI5? Most systems end up with both, so the real question is never
> "which framework" but "which one for this screen".
>
> RAP when the behaviour matters more than the screen: a transactional object
> with validations, drafts and authorizations, and more than one consumer for it.
>
> abap2UI5 when the screen is the deliverable: one consumer, one purpose, often a
> short life — an ops tool, a correction screen, a dashboard for one team. Or when
> the shape is only known at runtime, or the release is too old for RAP.
>
> And they compose: an abap2UI5 app calls a RAP business object through EML like
> any other consumer.
>
> New article 🎉
>
> Which of your screens is really a behaviour, and which is really just a screen?
>
> #ABAP #SAP #UI5

## UI5 Freestyle or abap2UI5 — When to Use Which

Plain text — LinkedIn renders no markdown.

> UI5 Freestyle or abap2UI5? Both give you the whole UI5 control library, so the
> choice is not about what is possible — it is about where the work happens.
>
> Freestyle when the browser has to be smart: offline, client-side state between
> roundtrips, genuinely interactive UIs. Also when a frontend team with its own
> release cycle owns the app — there the contract between front and back is a
> feature.
>
> abap2UI5 when the backend already knows everything: the data, the rules and the
> decisions are in ABAP, and a separate frontend project would be a second place
> to maintain for a screen whose logic never left the server.
>
> New article 🎉
>
> Iteration speed or client richness — which one is actually scarce for you?
>
> #ABAP #SAP #UI5

## Low-Code or abap2UI5 — When to Use Which

Plain text — LinkedIn renders no markdown.

> A commercial low-code platform and abap2UI5 answer the same question: modern
> UIs for SAP systems without a frontend stack per app. A visual designer on a
> licensed platform, or plain ABAP in an open-source framework.
>
> Low-code when the requirement is outside code: offline-capable native mobile,
> a bundled workflow or portal suite, contractual SLAs, or app building by
> non-developers — a designer exists so that a non-developer can produce
> something, and no code-first framework replaces that.
>
> abap2UI5 when apps should be code in your own system: diffable, transportable,
> unit-testable, nothing recurring per seat. And code-first is what AI agents are
> actually good at — a visual designer needs a human in front of it.
>
> New article 🎉
>
> Is your bottleneck building the apps, or governing them afterwards?
>
> #ABAP #SAP #UI5
