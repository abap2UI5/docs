---
outline: [2, 4]
description: Release tags and cadence of abap2UI5, what counts as stable API, the frozen package, how deprecations are announced, and how to upgrade, pin, roll back and re-test.
---
# Versioning, Upgrade

What a release of abap2UI5 is, what you may build on, and how an installation
moves from one release to the next. The facts here come from the framework's
release process and its changelog; the [Deprecations](/resources/deprecations)
page is the list of what has a successor, the
[Changelog](/resources/changelog) the record of what shipped when.

## Release Tags

A release is two tags pushed together: `X.Y.Z` on `main` and `X.Y.Z-702` on
the downported branch. The tag is the only thing in the repository that does
not move - `main` is pulled by abapGit as it is on the day, the `702` branch is
rebuilt from `main` on every push. An installation reads its own release from
the constant `z2ui5_if_app=>version`, which abapGit shows and the delivered
frontend is stamped with.

There is no fixed cadence. In 2026 the changelog lists eight releases, from
1.142.0 on 2026-07-20 to 1.148.0 on 2026-10-14, after a gap of seven months
before that; two of them shipped on the same day. Read the dates on the
[changelog](/resources/changelog) rather than planning on an interval. The
framework's security policy supports the latest release only.

## What Is Stable API

The released API is the package `src/02`, nine objects:

| Object | What it is |
|---|---|
| `z2ui5_if_app` | the interface every app implements - `main( )`, `id_draft`, `id_app`, the `version` constant |
| `z2ui5_if_client` | the client API: binding, events, views, popups, navigation - the [API reference](/resources/api) is generated from it |
| `z2ui5_if_ui5_exit` | the user exit: the page configuration and the per-roundtrip settings |
| `z2ui5_if_ui5_monitor` | the roundtrip monitor - [Monitoring](/configuration/monitoring) |
| `z2ui5_cl_ui5_http_handler` | the handler your ICF handler class calls |
| `z2ui5_cl_ui5_view_builder` | the view builder |
| `z2ui5_if_ui5_llm`, `z2ui5_cx_ui5_llm`, `z2ui5_cl_ui5_llm` | the language model seam |

A normalized snapshot of this package is compared on every pull request, so a
change to it is a deliberate decision rather than a side effect, and the
changelog marks such a change `BREAKING`. Everything in `src/00` and `src/01`
is internal: it may change between any two releases without a note. The
[linter](/advanced/linter) reports a call into it as `non-released-api`, which
is the check to run over your apps before an upgrade.

## The Frozen Package

`src/99` is the compatibility package: the predecessor view builder, the
superseded exit and types interfaces, the retired utility classes and the
built-in popups whose successor is the
[popups add-on](https://github.com/abap2UI5-addons/popups). Frozen means:

- it ships with every release, unchanged, so an app that uses it keeps
  compiling and running after an upgrade;
- nothing in the framework calls it, and its production code is not changed -
  a gate in the framework's CI refuses the change;
- it is not API. What it holds is listed on
  [Deprecations](/resources/deprecations) with the replacement for each.

An object leaves the package only when it is removed, and the framework's
removal plan orders that: documentation first, then the samples, then the
code, and a removal is a `BREAKING` entry in the changelog with the release
it happened in.

## How Deprecations Are Announced

Three places, in this order:

1. The **changelog** of the release - the framework's `changelog.txt`, which
   becomes the GitHub release notes - with a `!` for a change and a `-` for a
   removal, and `BREAKING` written out where a call stops compiling.
2. The [Deprecations](/resources/deprecations) page: one row per call, the
   replacement, and a status column that names the release the change shipped
   in or says *removed* with that release. Rows marked *next release* are on
   `main` and not in a tag yet - what is coming if you pin, what you have if
   you track `main`.
3. The interface itself: an obsolete method of `z2ui5_if_client` carries
   *obsolete* in its documentation comment and stays callable; the
   [API reference](/resources/api) leaves it out.

There is no fixed period between a deprecation and its removal. What the
history shows: the bulk of the removals of 2026 shipped in 1.144.1 and
1.145.0 (September 2026), each with its replacement on the deprecations page,
and the frozen package keeps the larger pieces - the predecessor builder, the
popups, the utility classes - compiling across them. Run the linter with
`--fix` before an upgrade; it rewrites the mechanical part and reports the
rest.

## How to Upgrade

1. **Read the changelog** of every release between yours and the target, and
   the deprecations page for the rows whose status falls in that range.
2. **Run the linter** over your app packages:
   `npx @abap2ui5/linter src --fix-dry-run` shows what it would change.
3. **Pull the tag** on the development system: in abapGit, switch the
   repository to the new tag and pull. Pull a tag, not `main` - `main` may
   carry an API no release has yet.
4. **The frontend, if a BSP is installed.** An installation that only uses
   the ICF node has nothing else to pull: the frontend is embedded in the
   package (`src/01/03`). An installation with the frontend as a BSP in the
   UI5 ABAP repository pulls the matching branch of the
   [frontend repository](https://github.com/abap2UI5/frontend):

   | Branch | System | Frontend |
   |---|---|---|
   | `standard` | S/4 Private Cloud, S/4 On-Premise, R/3 NetWeaver 7.50 and later | classic |
   | `standard_v2` | S/4 Private Cloud, S/4 On-Premise | legacy-free (UI5 2.x) |
   | `standard_new` | S/4 Private Cloud, S/4 On-Premise, R/3 NetWeaver 7.50 and later | the embed control - loads the frontend from the backend |
   | `cloud` | S/4 Public Cloud, BTP ABAP Environment | classic |
   | `cloud_v2` | S/4 Public Cloud, BTP ABAP Environment | legacy-free (UI5 2.x) |
   | `cloud_new` | S/4 Public Cloud, BTP ABAP Environment | the embed control |

   The `_new` branches carry no copy of the frontend: their app is one view
   with the embed control, which fetches the frontend from the backend it
   runs against, so an upgrade of abap2UI5 needs no frontend pull. They need
   an abap2UI5 newer than 1.146.0. Every branch keeps the same objects, names
   and endpoints, so a system switches between them by pulling another one.
5. **Transport** framework and frontend together to the next system, then
   your apps - [Transport, Landscape](/configuration/transport).

The embedded frontend and the backend are one package, so browser and server
run the same release by construction; a BSP is the one piece that can fall
behind, which is what the `_new` branches remove.

## The 702 Branch

A system below 7.50 installs the `702` branch, generated from `main` by
abaplint's downport on every push - [Downporting](/advanced/downporting). It
is force-pushed on every rebuild, so pin the `X.Y.Z-702` tag rather than the
branch: the tag is the downport of exactly the commit the release was cut
from, checked by the release job. The `-702` release carries the sources and
no built assets.

## Pinning

In abapGit, link the repository to a tag and leave it there; `main` is for
contributors. Record the release in the operations notes together with the
UI5 version the bootstrap source names - two numbers, since UI5 is loaded
from wherever `cs_config-src` points and is not part of the pull. A fixed UI5
version belongs in the exit as well: the default bootstrap URL is a
cache-buster that follows the newest OpenUI5 -
[Bootstrapping](/configuration/setup/ui5_bootstrapping).

## Rollback

Switch the abapGit repository back to the previous tag and pull; the objects
of the newer release are overwritten with the older ones, and an object the
older release does not have is deleted by the pull. The frontend BSP, if one
is installed, goes back the same way.

What a rollback does not undo is the draft table: rows written by the newer
release stay in `Z2UI5_T_01` until they expire. A draft is the serialized app
instance, and a serialization written by one release is not guaranteed to
deserialize on another; the framework's changelog records such a case. The cost
is one error page for a user whose draft does not load, and the next start is a
fresh app; the rows are gone after `draft_exp_time_in_hours`, four hours by
default. Re-test after a rollback the same way as after an upgrade.

## What to Re-Test

The framework's CI proves the framework; what it cannot see is your system,
your apps and your exit. After every pull, on the development system and
again on the quality system:

- **The start page.** Open the node, press *check*, start the Hello World
  app. That is abapGit, handler, node, exit and table in one roundtrip.
- **Your ABAP Unit tests** over the app packages, and the linter.
- **One roundtrip per app**: start, one event, one navigation, one popup
  where the app has one. A view that references a control or property the
  newer UI5 does not have fails here and nowhere earlier.
- **The exit's effect**: the bootstrap source, the hidden error details, the
  CSP and the headers - the cockpit's security traffic light reads them off
  the running system in one screen ([Monitoring](/configuration/monitoring#admin-cockpit)).
- **The launchpad tiles**, if any: the app index of the UI5 repository has
  to know the new content of the BSP - [Fiori Launchpad](/configuration/launchpad#troubleshooting).

Something the newer release does differently is not a bug in your app; it
is a changelog entry you have not read yet. Read the entries between the two
releases first, then open an issue with a sample.
