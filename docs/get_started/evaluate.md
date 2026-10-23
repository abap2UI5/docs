---
outline: [2, 4]
description: abap2UI5 for the people who approve a tool rather than build with it - what problem it answers, when it is the right choice and when RAP is, what your organization keeps, where it runs, what it costs, the objections a review board raises, and how to test it on one real screen in a week.
---
# Evaluate abap2UI5

This page is for the people who decide whether abap2UI5 may be used: IT
management, architecture boards, product owners, SAP Basis, internal audit.
It answers the questions such a review asks, in the order it asks them, and
links the page that carries the detail behind each answer. A developer who
wants to see it run is better served by the [Quickstart](/get_started/quickstart);
send them there and read on.

## The problem it answers

Every SAP system has screens nobody shipped: the cockpit for one team, the
monitor for one process, the selection screen with a result list that used to
be a report with an ALV. They are not standard floorplans, so Fiori Elements
does not cover them; they are not worth a frontend project, an OData service
and a deployment pipeline, so they stay in SAP GUI, in a spreadsheet, or on
paper. And on a system older than S/4HANA, RAP is not there to build them
with at all.

abap2UI5 is the shorter path for exactly these screens. An app is one ABAP
class, written in ADT or SE80, transported like any other class, and it
renders as a UI5 app in the browser, in the Fiori launchpad, or on a mobile
device. There is no service, no frontend project and nothing to deploy. The
rest of this page is what that costs and what it does not.

→ *[In a Nutshell](/get_started/about) is the technical picture of how the
framework works, for the architect who wants to see the roundtrip*

## When to use it, and when not

abap2UI5 is one way to put a UI5 interface on ABAP. RAP with Fiori Elements
is another, freestyle UI5 with OData a third, and all three run side by side
in the same launchpad. Written so that a team can disagree with a specific
line rather than with a feeling:

**Use abap2UI5 when**

- **The team is ABAP.** No JavaScript, no UI5 tooling, no Business
  Application Studio to set up. An app is one class, written, debugged,
  transported and unit-tested like every other class.
- **The system is older than RAP.** NetWeaver 7.02 to 7.4x have no RAP and
  a Fiori stack that is expensive to add. abap2UI5 runs there from the same
  source it runs on ABAP Cloud, so an app written on 7.31 today moves to
  S/4HANA unchanged.
- **The screen is yours, not SAP's.** Tools, cockpits, monitors, admin
  consoles, anything that used to be a report or a module pool. The moment
  a screen is a dashboard with a chart and a custom form, a Fiori Elements
  template becomes the thing you fight.
- **You do not want a deployment pipeline.** No BSP upload, no approuter, no
  MTA. The ABAP system serves the frontend; an abapGit pull is the
  installation.
- **You replace SAP GUI screens one at a time.** Selection screens, lists and
  transactions become browser apps without rewriting the logic behind them.

**Use RAP and Fiori Elements when**

- **The service is the product.** Other consumers, a mobile app, an
  integration, SAP's own apps, need the OData service anyway. RAP gives you
  the service and the UI from one model; abap2UI5 gives you a UI and no
  service.
- **The screen is a standard floorplan and should stay one.** List report,
  object page, worklist, with SAP's draft handling and flexible column layout
  as SAP ships and maintains them. Fiori Elements tracks every UI5 release
  for you; a hand-built screen is yours to keep current.
- **The data is large and lives behind paging.** Fiori Elements pages, sorts
  and filters through OData on the server. In abap2UI5 the table a view
  binds travels with the model; thousands of rows are routine, millions are
  the app's job to page.
- **Offline, or a native mobile SDK, is a requirement.** abap2UI5 is a
  roundtrip to the ABAP system on every interaction. It has a mobile shell
  for camera, barcode and location; it has no offline store.
- **The organization has standardized on it.** A clean-core guideline that
  names RAP as the extensibility model is a decision already taken.
  abap2UI5 passes the ABAP Cloud checks, but it is not SAP's programming
  model, and a review board will say so.

The common answer is not either. Standard transactional screens as Fiori
Elements, the tools and cockpits around them as abap2UI5, both as tiles in
the same launchpad with navigation between them. What it costs either way:

| | abap2UI5 | RAP + Fiori Elements |
|---|---|---|
| Skills | ABAP, and a reading knowledge of the UI5 control API | ABAP, CDS, RAP, annotations, and someone who can debug the Fiori Elements template |
| First screen | One class, minutes | Data model, behavior definition, service, Fiori project, deploy: a day when everything is set up |
| Custom UI | Any UI5 control, 1:1, from ABAP | Extension points; past them, a freestyle UI5 project |
| Upgrade | The framework is a transport; the public API is held by a snapshot and deprecations are tracked | SAP's; the template follows the UI5 release |
| Reuse by other consumers | None, there is no service | The OData service, by anything |
| Runs on | NetWeaver 7.02 to ABAP Cloud, UI5 1.71 to 2.x | 7.5x and later for RAP, ABAP Cloud for the full model |
| License | MIT, no per-user cost | Part of the platform license |

→ *[On-Stack or Side-by-Side](/advanced/insights/30-on-stack-or-side-by-side)
and [One App, Many Systems](/advanced/insights/31-one-app-many-systems) are
the deployment scenarios in detail*

## What you keep

The decision everything else follows from: an abap2UI5 app is code, not
configuration. One ABAP class per app, no designer, no generated artifacts,
no tables that describe a screen. That is what makes it govern itself:

| | abap2UI5 | A tool that stores apps as configuration |
|---|---|---|
| What an app is | An ABAP class | Entries in the tool's tables, read by its runtime |
| Where a change is made | In the development system, in ADT or SE80 | Wherever the tool's designer is open, possibly production |
| How a change travels | A transport, like any class | The tool's own export, or a customizing transport |
| How a change is reviewed | As a code diff: in abapGit, a pull request or the transport | In the tool's designer, if it shows a history at all |
| What a debugger shows | Your code | The tool's runtime interpreting your configuration |
| What happens if the tool goes away | The classes stay; they are plain ABAP | The apps go with it |

So abap2UI5 needs no rule of its own. Every rule that already applies to
ABAP, naming conventions, code review, ATC, the four-eyes principle on
transports, applies to an app without any change. A system closed for
repository changes is closed for abap2UI5 apps.

- **Authentication is SAP's.** No user store, no token, no session concept
  of its own. A request that reaches an app has passed SAP logon.
- **Authorization is two gates.** The authorization object on the ICF node,
  as for any service, and the app's own authority checks in its code, the
  same way a transaction or a report does them. The framework adds no
  central check on purpose.
- **Data changes are your code's.** An app posts a document by calling the
  same BAPIs, function modules or RAP actions any other ABAP program calls,
  so change documents, application logs and the authority checks behind
  them apply unchanged.
- **Every roundtrip can be logged.** A monitor interface receives, for each
  request, the user, the app, the event, the timings and any error. It is
  off until an installation switches it on.
- **Clean core.** abap2UI5 uses released APIs only, passes the ABAP Cloud
  checks and runs in ABAP Cloud, on S/4HANA and on BTP, from the same source
  that runs on NetWeaver 7.02. It modifies nothing in the SAP standard.

The price is that someone has to write ABAP. There is no click-together
designer for business users, on purpose: a designer would put a second kind
of app definition next to the code, and that second kind is exactly what the
table above has to govern separately.

→ *More on [Security](/configuration/security) and
[Authorization](/configuration/authorization), and the
[security model](https://github.com/abap2UI5/abap2UI5/blob/main/docs/security.md)
a security review starts from: what the framework guarantees, and which test
holds the code to each guarantee*

## Where it runs

```
Browser                          SAP system (the only server)
───────────────────────────      ────────────────────────────────────────────
generic UI5 shell, loaded   ───▶ ICF node → abap2UI5 handler → your app class
once; no app code of its         SAP logon, authorization objects, sy-uname
own                              app state in one table, per user
```

The browser runs a generic UI5 shell, the same for every app. App code never
leaves the system; the browser receives the view and the data the app chose
to bind. There is one HTTP endpoint, an ICF node, and nothing else listens.
No middleware, no extra server, no BTP subscription, no frontend deployment.

- **Systems:** S/4HANA Public Cloud and BTP ABAP Environment (ABAP for
  Cloud), S/4HANA Private Cloud and on-premise, and NetWeaver 7.02 or later.
- **UI5:** the frontend is UI5 itself, 1.71 to 2.x. By default it loads from
  the public CDN; a productive installation points it at the UI5 the system
  already ships, which is what a system without internet access uses.
- **Where an app appears:** a browser tab, a Fiori launchpad tile, SAP Build
  Work Zone, Microsoft Teams, Microsoft Excel, a native app on iOS and
  Android, or embedded in a UI5 app of your own.

What Basis does, all of it: pull the package with abapGit, create the ICF
node and put an authorization object on it, change the four production
defaults in the user exit (UI5 source, error details, the Content Security
Policy, the node authorization), and optionally switch on the monitor. There
is no job to schedule and no schema to migrate on upgrade.

→ *More on [Productive Usage](/configuration/productive_usage),
[UI5 Versions](/configuration/ui5_versions),
[Performance](/configuration/performance) and
[Integration](/integration/)*

## What it costs

abap2UI5 is MIT licensed, commercial use included. No license key, no
subscription, no per-user, per-system or per-app fee, now or later. The
apps built with it are standard UI5 freestyle apps, licensed the way your
organization licenses every other UI5 app. It runs on the ABAP stack and
the UI5 version you already have.

What is not free is the work: an ABAP developer's time to write the screen,
and the evenings of the volunteers who maintain the framework. Support is
the community's, on GitHub issues and Slack, on a best-effort basis; there
is no vendor SLA to buy from the project. Companies and freelancers offer
paid implementation, training and support agreements around it.

→ *More on the [Cost Calculator](/resources/cost_calculator),
the [License](/resources/license) and [Support](/resources/support)*

## The objections

The questions a review board raises, with the honest answer to each.

**It is an open-source project. What if the maintainers stop?**
The installed version keeps running, the source is in your system in full,
readable and changeable, and the apps stay ordinary ABAP classes. Nothing is
called at runtime that is not in your system, apart from the UI5 library,
which you can host yourself. A build renamed into your own namespace is one
workflow run ([Namespaces, Renaming](/advanced/renaming)).

**Is this another UI5 wrapper that will not survive the next UI5 release?**
The frontend is UI5 itself, not a copy of it, and every merge is run in the
browser on the oldest supported release and on UI5 2.x. Releases are tagged
and listed ([Release Notes](/resources/changelog)); what is on its way out is
announced and tracked until removal ([Deprecations](/resources/deprecations)),
and the public API apps use is held by a snapshot, so a change to it fails
the framework's own pull request.

**Every click is a roundtrip. Is that fast enough?**
A roundtrip is one HTTP POST carrying only what changed, served by the ABAP
stack that already serves everything else. For the screens this page is
about, worklists, forms, cockpits, it is the same round trip a SAP GUI
screen makes, in a browser. What it is not built for is a real-time
collaborative editor or an app that holds its state offline
([Performance](/configuration/performance)).

**Our clean-core guideline names RAP. Where does this leave us?**
Side by side. abap2UI5 passes the ABAP Cloud checks and modifies nothing in
the standard, so it is clean core in the technical sense, but it is not
SAP's programming model. The usual outcome is a scope: standard
transactional screens in RAP, the tools around them in abap2UI5, both in one
launchpad.

**Who is accountable when something breaks?**
Your developer, for the app, the same as for any class in the system; the
community, for the framework, on a best-effort basis; a commercial partner,
if you sign one. Security reports are acknowledged within three business
days ([Security](/configuration/security)).

**Is anybody actually running this in production?**
Yes, see below. And every release is tested against Standard ABAP and ABAP
Cloud, downported and linted against NetWeaver 7.02, and run in the browser
on the oldest and the newest supported UI5 release
([verified on](https://github.com/abap2UI5/abap2UI5/blob/main/docs/verified-on.md)).

## Who runs it

Companies run abap2UI5 apps for EWM and PP on desktop and mobile devices,
consultancies offer workshops and certifications for it, and it has been
featured in SAP Developer News and at SAP TechEd. [Who Uses abap2UI5?](/resources/who_uses)
lists the companies with their system release and use case;
[References](/resources/references) lists the talks, articles and videos by
year. Both are open for your own row.

## How to evaluate it in a week

Take one real screen your team has to build this quarter. Build it with
abap2UI5 in a development system, with the people who will maintain it, and
review it the way your organization reviews ABAP: read the class, transport
it, run ATC on it, check the authorizations it asks for. Count the files,
the systems and the tools involved, and the time until a user can click it.
Then hold this page against the security model and the verified-on page
with the skepticism you would apply to a vendor's slide, and decide from
the screen, not from the slide.

Where to start, in order of effort:

1. **Ten minutes, nothing installed.** Open the
   [playground](https://abap2ui5.github.io/playground/) and change a running
   app, or browse the
   [sample catalog](https://abap2ui5.github.io/playground/samples/) for the
   screen closest to yours.
2. **One afternoon, one developer.** Send them the
   [Quickstart](/get_started/quickstart): abapGit pull, ICF node, first app.
   The [Tutorial](/tutorials/) takes the same app to transport and unit
   tests.
3. **One question.** Ask it in the
   [Slack channel](https://abapgit.slack.com/join/shared_invite/zt-46tqufaht-QlrxTzlDqlx85CWbeUnOqg#/shared-invite/email)
   or as a [GitHub issue](https://github.com/abap2UI5/abap2UI5/issues), where
   the people who run it in production answer.
4. **A partner.** The companies on [Who Uses abap2UI5?](/resources/who_uses)
   offer implementation, workshops and support agreements.
