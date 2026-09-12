# LinkedIn teaser posts

The posts that introduce each article. Plain text — LinkedIn renders no
markdown. Kept here rather than on the published pages: they are publishing
material, not documentation.

The sections stand in the order the articles are numbered in
`docs/advanced/insights/`, one per article, so this file is also the
publishing order. Move a post here when an article moves there. A post whose
article does not exist yet waits under *Not yet published* at the end, and
moves up the day its article lands.

The fifteen posts marked *draft* were written from the articles after the
fact, in the shape of the ones before them, and have not been posted - read
one before it goes out, the way the others were read.

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

## The Cost of a Screen

Plain text — LinkedIn renders no markdown. The article link comes from the
attached article preview, so the post carries no inline URL. 1141 characters,
and the first line is 111 of them: what stands above LinkedIn's fold, so it
says the whole thing on its own.

> Small applications in ABAP are still built the way they always were: a
> selection screen, a SELECT, an ALV grid.
>
> And honestly - why not.
>
> It just takes a few hours to build a cockpit showing what last night's job
> did. An approval step for one department, four people, twice a year. A
> correction screen somebody needs exactly once, during a go-live.
>
> Building any of those in UI5 is different: a data model, a service, a
> binding, an annotation model, sometimes also a frontend project and a
> deployment.
>
> So the ALV stays.
>
> New article 🎉 The same job monitor as a complete abap2UI5 app: one ABAP
> class, activate, call the endpoint. About as much work as the ALV, except
> this one follows the Fiori design guidelines and starts on your phone too.
>
> abap2UI5 is open source, MIT licensed, and it runs on the UI5 and the ABAP
> your system already has: one abapGit pull, nothing to deploy beside it. A
> perfect complement to the RAP and freestyle UI5 apps you already run — for
> the screens nobody would start a project for.
>
> Which of your ALV grids would you hand to a user as a Fiori app, if it cost
> you one class?
>
> #ABAP #SAP #UI5 #abapGit #abap2UI5

## abap2UI5 in Your Favorite Programming Model

Plain text — LinkedIn renders no markdown.

> With abap2UI5 you start by writing an ABAP class, the way you used to start a
> report with a selection screen. One interface, one method — that is the whole
> contract.
>
> Beyond that there are no rules. No service, no binding, no annotations,
> nothing to transport but the class — and no demands on how you design your
> model.
>
> The new article shows the same edit screen three times: through a RAP business
> object, straight to a database table, and against a BAPI from twenty years
> ago. Three programming models, one unchanged UI class.
>
> Where a strict programming model fits, use it. This is one more option next to
> it, for the screen that would otherwise not get built at all.
>
> New article 🎉
>
> Which programming model would sit behind your screen?
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
> What changes is who owns the view. Not an artifact deployed beside the app —
> a string an ABAP class produced for this request.
>
> New article 🎉
>
> Where would you draw the line between a frontend and a renderer?
>
> #ABAP #SAP #UI5

## The Frontend Knows Nothing

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

## One Handler for Every App

Plain text — LinkedIn renders no markdown.

> Count what one screen usually costs in backend artifacts: a CDS view or two, a
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

## A New Instance on Every Request

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

## No Build, No Deploy, No Cache

Plain text — LinkedIn renders no markdown.

> Somebody has to deliver the first HTML page. In the SAP world that normally
> means a BSP: a frontend artifact, built somewhere, deployed, transported on
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

## Four Verbs, Every Control

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

## A Classrun for the Browser

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
> layer is there: a typed, labeled input with a default and value help, plus
> validation and event handling. Nobody wrote a frontend, because declaring the
> variable was writing the frontend.
>
> That trade is what disappeared on the way to the browser: the variable and the
> field became two artifacts in two places, kept in step by a service in between.
>
> _bind puts them back together. The attribute is not a field name in a string —
> it is the ABAP variable.
>
> New article 🎉
>
> How many places does one input field touch in your current stack?
>
> #ABAP #SAP #UI5

## One Click, One Request

Plain text — LinkedIn renders no markdown. Draft, 803 characters.

> Every click is one HTTP request, and every request is one call to main( ) on a
> fresh instance.
>
> That is PBO and PAI with the names filed off. A request arrives with the event
> and what the user changed, main( ) runs, decides, returns. Nothing runs between
> two clicks, because nothing is there to run - no timer, no controller, no
> half-finished promise.
>
> So main( ) is a dispatcher over three questions: is this the first call, is the
> app coming back to the screen, did an event fire. Two of the three are the
> mistake everybody makes exactly once - the init branch has to stand first, and
> a return owes a view.
>
> New article 🎉 The mental model everything else in abap2UI5 rests on, in one
> sentence and one CASE statement.
>
> Which habit from dialog programming did you have to unlearn first?
>
> #ABAP #SAP #UI5

## What the Client Can Do

Plain text — LinkedIn renders no markdown. Draft, 778 characters.

> An app implements one interface with one method. Its single parameter is the
> other interface - and it is the whole of what an app can ask the framework for.
>
> Show a screen, put ABAP data on it, find out what the user did, talk to the
> user, move to another app and back, ask the browser for something. Eight rows
> in a table, and the two that carry the most weight are the smallest: _bind( )
> takes an ABAP variable and returns the binding path, _event( ) names what the
> frontend sends when a control fires. Both are string generators, and that is
> all they are.
>
> New article 🎉 The API surface an abap2UI5 app ever touches - small enough to
> hold in your head, by design rather than by accident.
>
> How many methods of your UI framework could you name without looking?
>
> #ABAP #SAP #UI5

## CALL SCREEN, LEAVE SCREEN

Plain text — LinkedIn renders no markdown. Draft, 714 characters.

> Module pools had a call stack of screens. CALL SCREEN pushed one, LEAVE TO
> SCREEN 0 popped it, and the screen underneath came back with its fields intact.
>
> abap2UI5 has the same stack. Its elements are app instances: nav_app_call( )
> pushes one, nav_app_leave( ) pops it, and the caller gets main( ) called again
> with its attributes still there. What it reads back is not a string in a
> parameter table - it is the sub-app instance itself, typed attributes and all.
> A luxury CALL SCREEN never had.
>
> New article 🎉 Screens within one class, screens between classes, popups as
> classes of their own - and the one navigation that is not on the stack.
>
> What did your CALL SCREEN chains hand back, and how?
>
> #ABAP #SAP #UI5

## Where F4 Went

Plain text — LinkedIn renders no markdown. Draft, 749 characters.

> PARAMETERS pa_arbgb TYPE t100-arbgb got a value help for free. The DDIC knew
> the search help, the screen knew the DDIC, and nobody wrote a line for it.
>
> An Input in a UI5 view is a box that holds text. It has no idea what type
> stands behind it.
>
> abap2UI5 does not put the automatic version back. What it gives you is the
> three shapes an F4 actually comes in: suggestions while typing (a bound table,
> no roundtrip), a selection popup (a SELECT, a list, an event with the picked
> row), and a reusable value help as a class of its own on the stack.
>
> New article 🎉 F4 is three lines and a SELECT now. No longer free - and no
> longer limited to the DDIC either.
>
> Which of your value helps would be better as a SELECT than as a search help?
>
> #ABAP #SAP #UI5

## MESSAGE Still Works

Plain text — LinkedIn renders no markdown. Draft, 918 characters.

> Thirty years of habits around messages, and most of them carry over unchanged.
> What changes is where the message ends up.
>
> A toast for what needs no acknowledgment, a box for what does. sy after
> MESSAGE ... INTO, a BAPIRET2 table, an exception object - the framework reads
> text, type and details out of each. Message classes, T100 texts and the
> translation tooling around them stay exactly as useful as they were: an app
> is an ABAP class, there is no i18n file beside it, nobody exports a properties
> file to anybody.
>
> What is different is the exception that gets away: one catch in the HTTP
> handler, HTTP 500, a restart overlay - and one setting every production
> system needs so the 500 body does not tell a browser more than it should.
>
> New article 🎉 The message classes, the texts and the translation stay. Only
> the dump looks different.
>
> Where do your messages land today - and who translates them?
>
> #ABAP #SAP #UI5

## The Lock Is Gone by the Next Click

Plain text — LinkedIn renders no markdown. Draft, 915 characters.

> VA02 holds its lock for as long as the dialog session lives. That works
> because the session lives.
>
> In abap2UI5 every click is a fresh session, so a lock set while the order
> opens is released before the user has finished reading the screen. The ABAP is
> still valid. The assumption underneath it is not - and this is the habit that
> most reliably surprises a developer coming from dialog programming.
>
> It is not a limitation to work around. It is the question every stateless web
> application has answered since roughly 1999: lock at save, an optimistic check
> on the timestamp, a soft lock for the "being edited by Müller" warning - and
> the GUI way, still there, for the few apps that should pin a work process.
>
> New article 🎉 A lock is a promise about a session. Where the session is one
> request long, the promise has to be, too.
>
> How does your app tell the second user that the first one was faster?
>
> #ABAP #SAP #UI5

## Who May Start Which App

Plain text — LinkedIn renders no markdown. Draft, 840 characters.

> One ICF node serves every abap2UI5 app, and the URL names the class. So
> anybody with the node can start any class?
>
> Only if nobody decided otherwise, and the deciding happens in two familiar
> places. Authentication is the ICF node's - logon, SSO, certificate, like any
> other UI5 app. Authorization is the app's, the way a report checks before it
> selects: an AUTHORITY-CHECK in the branch that shows the first view, one
> object, one field, the class as the value, roles in PFCG. The check sits in
> the class it protects, so a transport carries the app and its guard together.
>
> New article 🎉 One node, one class, one AUTHORITY-CHECK. The rest is PFCG -
> plus the CSRF token and the Content Security Policy a web app needs and a
> report never did.
>
> Where does the authorization check sit in your apps - on the node, or in the
> code?
>
> #ABAP #SAP #UI5

## 100 Rows, Four Hours, One Request

Plain text — LinkedIn renders no markdown. Draft, 847 characters.

> Three numbers decide whether an abap2UI5 app feels fast, and none of them is
> in your code.
>
> 100 rows: a UI5 JSON model shows a list binding at most 100 items by default.
> Bind a table with 300 entries and the rest are dropped - silently, no error
> anywhere. Raised per view from ABAP, and not the fix for fifty thousand rows.
>
> Four hours: the serialized instance lives in one table until it expires. Back
> after lunch, you continue; back tomorrow, you start fresh.
>
> One request: every click is one roundtrip plus one read and one write of the
> draft, so the cost of a click is the size of the instance plus the size of
> the model - which are the same attributes.
>
> New article 🎉 Small instance, bounded model, one request per click.
> Everything else the system already does well.
>
> Which of the three has cost you the most debugging time?
>
> #ABAP #SAP #UI5

## ABAP Unit for a Screen

Plain text — LinkedIn renders no markdown. Draft, 817 characters.

> Testing a screen is usually where enthusiasm goes to die. You drive a
> browser, or you mock a framework, or you decide - silently, in a meeting
> nobody minuted - that this part is tested by the users.
>
> An abap2UI5 app is a global class, and a global class has a Test Classes
> include. That is the whole story, and it stays that short because of one
> decision in the app: the logic does not touch the client. main( ) dispatches,
> the methods it dispatches to read data, decide and change attributes - and a
> test calls those methods directly and looks at the attributes afterwards.
> Nothing in the test is abap2UI5.
>
> New article 🎉 A screen under ABAP Unit, with no browser driven and no
> framework mocked - and the linter for the half a unit test cannot see.
>
> When did a screen of yours last have a unit test?
>
> #ABAP #SAP #UI5

## When One Class Is Not Enough

Plain text — LinkedIn renders no markdown. Draft, 819 characters.

> Every example in this series is one class, and for the screens it is about
> that is the right size. Real applications grow past it.
>
> A framework with no answer here produces one 4,000-line class and a
> maintainer who resigns. There are four seams, and each one is ordinary ABAP:
> one class per screen, connected by the stack. Popups and value helps as
> classes of their own. The logic in a class that has no screen - the service a
> second screen, a batch job and a test all call. Parts of one screen as nested
> views.
>
> What does not appear in that list is a single framework concept. No component,
> no manifest, no router, no controller hierarchy, no dependency injection
> container.
>
> New article 🎉 A larger app is more classes. The unit stayed the same size.
>
> Where do you cut a screen that has grown too big?
>
> #ABAP #SAP #UI5

## A Tile Like Any Other

Plain text — LinkedIn renders no markdown. Draft, 802 characters.

> An app that is reached by pasting a URL is a demo. It becomes an application
> when it has a tile.
>
> In the launchpad an abap2UI5 app is indistinguishable from the RAP and
> freestyle tiles beside it. One thing is installed once: the abap2UI5 shell in
> the UI5 repository, through abapGit, for the whole system. Everything else is
> the Fiori administration the system already does - a target mapping, a tile,
> a catalog, a role - with nothing abap2UI5-specific in it except one
> parameter: the app class.
>
> And when the tile is blank, because it will be, once: the app index after an
> abapGit import. One report, one cache, one hard reload.
>
> New article 🎉 One shell in the UI5 repository, one parameter per tile. No
> user can tell the difference.
>
> How long did your last "blank tile" take to find?
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

## When the API Is Not Released

Plain text — LinkedIn renders no markdown. Draft, 923 characters.

> SAP grades an extension on four clean core levels now: A for released APIs
> only, B for classic APIs in Standard ABAP, C for SAP-internal objects, D for
> modifications.
>
> abap2UI5 is Level A. Which says nothing about your app - the level an app
> reaches is decided by what the app calls.
>
> So the case that actually comes up on a Tuesday: the API you need is not
> released. The move is a wrapper - a class in Standard ABAP that calls the
> classic API and is itself released for ABAP for Cloud Development. Graded on
> its own, B, while the app calling it stays A. The value is not the grade. It
> is that the part of the system which is not upgrade-stable now has a name, a
> size and a boundary - one class to re-check when SAP changes the API
> underneath it.
>
> New article 🎉 A wrapper does not make the dependency clean. It makes it
> findable.
>
> How many classic APIs does your newest app reach - and could you list them?
>
> #ABAP #SAP #UI5

## On-Stack or Side-by-Side

Plain text — LinkedIn renders no markdown. Draft, 834 characters.

> Two places to run an abap2UI5 app, and the choice is about lifecycles, not
> about code. The class is the same class either way.
>
> On-stack: inside the SAP system. Direct reads, no remote call, the transport
> you already use - and that system's release, upgrade window and change freeze
> in December.
>
> Side-by-side: on the SAP BTP ABAP Environment, calling S/4 through released
> remote APIs. Released, upgraded and restarted on its own schedule, and the
> S/4 system does not have to agree. The cost is equally plain: a remote call is
> slower than a local read, the released APIs are narrower, and there is a
> second system to run.
>
> New article 🎉 Neither choice touches the app class. The same code renders in
> both places - only what it reads changes.
>
> Which constraint decides it for you - the data path, or the release cycle?
>
> #ABAP #SAP #UI5

## One App, Many Systems

Plain text — LinkedIn renders no markdown. Draft, 931 characters.

> A side-by-side app already runs outside the system it serves. So: how many
> systems can it serve?
>
> More than one. The app lives on the SAP BTP ABAP Environment and reaches each
> S/4 system through its released APIs, so the connection is configuration
> rather than code. One codebase, deployed once, serving several tenants, each
> with its own system, its own data and its own release - nobody transports
> into a customer system, and a fix reaches every tenant at once.
>
> Nothing was added to make this possible. The frontend renders whatever
> arrives, the app is one class with nothing beside it, and the state travels
> with the request. The limits are the ones the shape implies - and the
> tenant's data boundary is now the app's to enforce.
>
> New article 🎉 A framework with nothing to install per system can serve
> systems it was never installed on.
>
> Would you run one app for several S/4 systems - and what would stop you?
>
> #ABAP #SAP #UI5

## From ITS to abap2UI5

Plain text — LinkedIn renders no markdown.

> ITS 1996. BSP 2001. Web Dynpro ABAP 2006. UI5 Freestyle 2012. RAP and Fiori
> Elements 2019. abap2UI5 2023.
>
> Read as a line, it is not a march away from the server. For the first fifteen
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

## RAP or abap2UI5?

Plain text — LinkedIn renders no markdown.

> RAP or abap2UI5? Most systems end up with both, so the real question is never
> "which framework" but "which one for this screen".
>
> RAP when the behavior matters more than the screen: a transactional object
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
> Which of your screens is really a behavior, and which is really just a screen?
>
> #ABAP #SAP #UI5

## UI5 Freestyle or abap2UI5?

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

## Low-Code or abap2UI5?

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

## Written for Agents

Plain text — LinkedIn renders no markdown. Draft, 848 characters.

> An app is one class, and the class is text. That property has a consequence
> nobody was designing for in 2023: it is the shape an AI coding agent is best
> at.
>
> An agent writing a Fiori Elements app keeps a CDS view, annotations, a
> behavior definition, a service binding and a generated frontend in step, and
> can verify none of it without a system. An agent writing abap2UI5 writes one
> file, in one language, and the thing it writes is the thing that runs.
>
> Three things turn that into a working setup: a map for the reader with no
> checkout, a linter that checks the view without a system, and several
> hundred worked examples - many of them ported by agents exactly that way.
>
> New article 🎉 Code-first was the design. Agent-friendly was the consequence.
>
> What does your assistant get wrong about your framework, and what would fix
> it?
>
> #ABAP #SAP #UI5

## Not yet published

Posts whose article is not in `docs/advanced/insights/` yet. Each moves up
into the numbered order the day its article lands.

### No Cache, No Deploy, Any IDE

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

### 2,300 Lines

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

### What It Does Not Do

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

### Where the View Lives

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
> screen costs in artifacts, what can still change at runtime. Not a ranking:
> fixed early standardises well, fixed late adapts well.
>
> New article 🎉
>
> Where does the view live in the app you are working on today?
>
> #ABAP #SAP #UI5
