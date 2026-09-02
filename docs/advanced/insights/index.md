# Technical Insights

The knowledge behind abap2UI5, cut into pieces that each fit one coffee. Every
article makes one claim, shows it in code where code can show it, and can be
read on its own — but read in order, the four blocks add up to the whole
picture: why the framework exists, how it works under the app, what a working
day with it looks like, and where it belongs next to what SAP ships.

The axis the whole series turns on is this one:

![One axis: when the definition of the screen stops being changeable.](/insights/00-where-the-view-lives.svg)

*One axis: when the definition of the screen stops being changeable.*

Three ways to put a UI5 screen in front of a user on an ABAP stack, all three
rendering in the browser with the same control library. **UI5 Freestyle** fixes
the view at build time, as a file in a frontend project. **RAP with Fiori
Elements** fixes it at activation time, as annotations on CDS. **abap2UI5**
fixes it when the request is answered, as a string an ABAP class produced for
this request — and the next request may answer differently. Everything else
follows from where the view sits: what is deployed per app, what a screen
costs in artefacts, what can still change at runtime, and which language it is
all written in.

## Why

What the framework is for, before how it works.

| | |
|---|---|
| [#1 Somewhere on the Way to UI5, We Lost RTTS](/advanced/insights/01-somewhere-on-the-way-to-ui5) | a table whose structure is only known at runtime, drawn in UI5 |
| [#2 abap2UI5 Is Not a Programming Model](/advanced/insights/02-not-a-programming-model) | one interface, one method, and no opinion about what is behind the screen |
| [#3 The Cost of a Screen](/advanced/insights/03-the-cost-of-a-screen) | why the thirty-line screen never gets built, and a whole one as a class |
| [#4 No Annotation in Between](/advanced/insights/04-no-annotation-in-between) | the vocabulary is the UI5 control library, all of it |

## How It Works

The mechanism under the app, one piece per article.

| | |
|---|---|
| [#5 UI5 Over-the-Wire](/advanced/insights/05-ui5-over-the-wire) | every request answered with two strings |
| [#6 The Frontend Knows Nothing](/advanced/insights/06-the-frontend-knows-nothing) | one shell for every app, and why it never needs a redeploy |
| [#7 One Handler for Every App](/advanced/insights/07-one-handler-for-every-app) | no service per screen, and how small the handler is |
| [#8 Only the Changed Part](/advanced/insights/08-only-the-changed-part) | the view stays, the model updates |
| [#9 A New Instance on Every Request](/advanced/insights/09-a-new-instance-on-every-request) | serialized state, and what that asks of the class |
| [#10 Swapping the View at Runtime](/advanced/insights/10-swapping-the-view-at-runtime) | a different control, chosen in an IF |
| [#11 No Build, No Deploy, No Cache](/advanced/insights/11-no-build-no-deploy-no-cache) | change, activate, refresh |
| [#12 Where Your Own JavaScript Goes](/advanced/insights/12-where-your-own-javascript-goes) | three declared seams, no plugin system |
| [#13 Four Verbs, Every Control](/advanced/insights/13-four-verbs-every-control) | why the view builder knows no controls |
| [#14 A Classrun for the Browser](/advanced/insights/14-a-classrun-for-the-browser) | the shape it copied |
| [#15 Where the Selection Screen Went](/advanced/insights/15-where-the-selection-screen-went) | the variable and the field are one declaration again |

## A Working Day

What an ABAP developer meets in the first week, and how each habit carries over.

| | |
|---|---|
| [#16 One Click, One Request](/advanced/insights/16-one-click-one-request) | PBO, PAI, and the dispatch in `main( )` |
| [#17 What the Client Can Do](/advanced/insights/17-what-the-client-can-do) | the whole API surface, by shape |
| [#18 CALL SCREEN, LEAVE SCREEN](/advanced/insights/18-call-screen-leave-screen) | the app stack, sub-apps and popups |
| [#19 Where F4 Went](/advanced/insights/19-where-f4-went) | value help in three sizes |
| [#20 MESSAGE Still Works](/advanced/insights/20-message-still-works) | messages, exceptions, translation |
| [#21 The Lock Is Gone by the Next Click](/advanced/insights/21-the-lock-is-gone-by-the-next-click) | enqueue in a stateless world |
| [#22 Who May Start Which App](/advanced/insights/22-who-may-start-which-app) | one ICF node, authorization, CSRF |
| [#23 100 Rows, Four Hours, One Request](/advanced/insights/23-100-rows-four-hours-one-request) | the three numbers behind performance |
| [#24 ABAP Unit for a Screen](/advanced/insights/24-abap-unit-for-a-screen) | testing the class, linting the view |
| [#25 When One Class Is Not Enough](/advanced/insights/25-when-one-class-is-not-enough) | the seams of a larger app |
| [#26 A Tile Like Any Other](/advanced/insights/26-a-tile-like-any-other) | into the Fiori Launchpad |

## Where It Belongs

Releases, clean core, deployment shapes, and the choice against the alternatives.

| | |
|---|---|
| [#27 One Codebase, 7.02 to ABAP Cloud](/advanced/insights/27-one-codebase-702-to-abap-cloud) | why the release barely matters |
| [#28 Cloud-Ready Is a Property of Your App](/advanced/insights/28-cloud-ready-is-a-property-of-your-app) | decided in the SELECT |
| [#29 When the API Is Not Released](/advanced/insights/29-when-the-api-is-not-released) | the clean core levels, and the wrapper |
| [#30 On-Stack or Side-by-Side](/advanced/insights/30-on-stack-or-side-by-side) | two places to run, one class |
| [#31 One App, Many Systems](/advanced/insights/31-one-app-many-systems) | a SaaS shape in ABAP |
| [#32 From ITS to abap2UI5](/advanced/insights/32-from-its-to-abap2ui5) | twenty-seven years in one line |
| [#33 RAP or abap2UI5?](/advanced/insights/33-rap-or-abap2ui5) | behaviour against screen |
| [#34 UI5 Freestyle or abap2UI5?](/advanced/insights/34-freestyle-or-abap2ui5) | where the browser has to be smart |
| [#35 Low-Code or abap2UI5?](/advanced/insights/35-low-code-or-abap2ui5) | designer against code |
| [#36 Written for Agents](/advanced/insights/36-written-for-agents) | the consequence nobody designed |

*The articles on how it works grew out of
[Under the Hood of abap2UI5](https://community.sap.com/t5/technology-blog-posts-by-members/abap2ui5-7-technical-background-under-the-hood-of-abap2ui5/ba-p/13566459)
on the SAP Community, the long version in one piece.*
