---
outline: [2, 4]
description: Release tags and cadence of abap2UI5, what counts as stable API, the legacy objects in abap2UI5-ext, how deprecations are announced, and how to upgrade, stay on a release, roll back and re-test.
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
rebuilt from `main` whenever the ABAP sources change. An installation reads
its own release from the constant `z2ui5_if_app=>version`, which abapGit shows
and the delivered frontend is stamped with. The branch `cloud`, abap2UI5 for
ABAP Cloud, is generated from `main` on every push and carries no tags. What
each branch carries is on
[Deployment Scenarios](/configuration/installation#frontend-branches).

There is no fixed cadence. In 2026 the changelog lists eleven releases, from
1.142.0 on 2026-07-20 to 1.151.0 on 2026-11-25, after a gap of seven months
before that; two of them shipped on the same day. Read the dates on the
[changelog](/resources/changelog) rather than planning on an interval. The
framework's security policy supports the latest release only.

## What Is Stable API

The released API is the package `src/02`, ten objects:

| Object | What it is |
|---|---|
| `z2ui5_if_app` | the interface every app implements - `main( )`, `id_draft`, `id_app`, the `version` constant |
| `z2ui5_if_client` | the client API: binding, events, views, popups, navigation - the [API reference](/resources/api) is generated from it |
| `z2ui5_if_ui5_exit` | the user exit: the page configuration and the per-roundtrip settings |
| `z2ui5_if_ui5_monitor` | the roundtrip monitor - [Monitoring](/configuration/monitoring) |
| `z2ui5_cl_ui5_http_handler` | the handler your ICF handler class calls |
| `z2ui5_cl_ui5_view_builder` | the view builder |
| `z2ui5_cl_ui5_test` | the test harness that drives an app from ABAP Unit, one roundtrip per call |

A normalized snapshot of this package is compared on every pull request, so a
change to it is a deliberate decision rather than a side effect, and the
changelog marks such a change `BREAKING`. Everything in `src/00` and `src/01`
is internal: it may change between any two releases without a note. The
[linter](/advanced/linter) reports a call into it as `non-released-api`, which
is the check to run over your apps before an upgrade.

## The Legacy Objects

The predecessor view builder, the superseded types interface, the retired
utility classes and the built-in popups whose successor is the
[popups add-on](https://github.com/abap2UI5-addons/popups) are no longer part
of the framework. They ship, frozen, from
[abap2UI5-ext](https://github.com/abap2UI5/abap2UI5-ext), a repository a system
installs next to abap2UI5 when its code still names one of them. Frozen means:

- the objects keep their names and are not changed, so an app that uses them
  keeps compiling and running after an upgrade of abap2UI5;
- nothing in the framework calls them;
- they are not API. What they hold is listed on
  [Deprecations](/resources/deprecations) with the replacement for each.

A removal is a `BREAKING` entry in the changelog with the release it happened
in. The framework's removal plan sets the order: documentation first, then the
samples, then the code.

## How Deprecations Are Announced

Three places, in this order:

1. The **changelog** of the release - the framework's `changelog.txt`, which
   becomes the GitHub release notes - with a `!` for a change and a `-` for a
   removal, and `BREAKING` written out where a call stops compiling.
2. The [Deprecations](/resources/deprecations) page: one row per call, the
   replacement, and a status column that names the release the change shipped in
   or says *removed* with that release. Rows marked *next release* are on `main`
   and not in a tag yet - what is coming if you stay on a release tag, what you
   have if you track `main`.
3. The interface itself: an obsolete method of `z2ui5_if_client` carries
   *obsolete* in its documentation comment and stays callable; the
   [API reference](/resources/api) leaves it out.

There is no fixed period between a deprecation and its removal. The history
shows this: most of the removals of 2026 shipped in 1.144.1 and 1.145.0
(September 2026), each with its replacement on the deprecations page.
[abap2UI5-ext](https://github.com/abap2UI5/abap2UI5-ext) keeps the larger
pieces compiling across them: the predecessor builder, the popups, the utility
classes. Run the linter with `--fix` before an upgrade;
it rewrites the mechanical part and reports the rest.

## How to Upgrade

1. **Read the changelog** of every release between yours and the target, and
   the deprecations page for the rows whose status falls in that range.
2. **Run the linter** over your app packages:
   `npx @abap2ui5/linter src --fix-dry-run` shows what it would change.
3. **Pull the tag** on the development system: in abapGit, switch the
   repository to the new tag and pull. Pull a tag, not `main` - `main` may
   carry an API no release has yet. Which tag belongs to which system
   (`X.Y.Z`, or `X.Y.Z-702` below 7.50) is the table in the
   [Quickstart](/get_started/quickstart#_1-install-the-framework-via-abapgit).
   On ABAP Cloud pull the branch `cloud` again instead.
   A system that uses the legacy objects pulls
   [abap2UI5-ext](https://github.com/abap2UI5/abap2UI5-ext) in the same step.
4. **Nothing else to pull for the frontend.** It is embedded in the package
   (`src/01/03`), and the BSP `Z2UI5` for the launchpad is part of the same
   pull (`src/03`). The BSP carries no copy of the frontend - it loads it from
   the node - so it needs nothing after an upgrade. An installation whose BSP
   still comes from the retired abap2UI5/frontend repository switches once:
   [Coming from the frontend repository](/configuration/installation#frontend-branches).
5. **Transport** the framework, node and BSP included, to the next system,
   then your apps - [Transport, Landscape](/configuration/transport).

The embedded frontend and the backend are one package, so browser and server
always run the same release, and the BSP cannot fall behind either: it loads
the frontend from the node.

## The 702 Branch

A system below 7.50 installs the `702` branch, generated from `main` by
abaplint's downport whenever the ABAP sources or the downport tooling
change - [Downporting](/advanced/downporting). It
is force-pushed on every rebuild, so pull the `X.Y.Z-702` tag rather than the
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

Switch the abapGit repository back to the previous tag and pull. The objects of
the newer release are overwritten with the older ones. An object the older
release does not have is deleted by the pull. The node and the BSP go back the
same way; a rollback to 1.150.0 or older, from before they were part of the
package, deletes them.

What a rollback does not undo is the draft table: rows written by the newer
release stay in `Z2UI5_T_01` until they expire. A draft is the serialized app
instance, and a serialization written by one release is not guaranteed to
deserialize on another; the framework's changelog records such a case. The cost
is one error page for a user whose draft does not load, and the next start is a
fresh app; the rows are gone after `draft_exp_time_in_hours`, four hours by
default ([Sizing](/configuration/sizing#draft-table-growth)). Re-test after a rollback the same way as after an upgrade.

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

When an app behaves differently after the upgrade, read the changelog entries
between the two releases first: the change is usually described there. If it
is not, open an issue with a sample.
