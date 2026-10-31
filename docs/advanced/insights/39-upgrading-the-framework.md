---
description: How abap2UI5 is versioned and upgraded - release tags, what is a stable contract (the public package), what the frozen package means, the deprecations page, pulling a tag with abapGit, the frontend branches, pinning, rolling back, what to re-test.
---
# #39 Upgrading the Framework

An installation that runs abap2UI5 runs a version of it, and sooner or later
pulls the next one. This is what a version is, what it promises, and how the
move is made.

**What a release is.** A release is a git tag on the framework repository,
`X.Y.Z` on `main` and `X.Y.Z-702` on the generated downport branch
([#27](/advanced/insights/27-one-codebase-702-to-abap-cloud)). `main` moves
with every merge; a tag does not. The [release notes](/resources/changelog)
list every tag with its date — eight of them in 2026 so far, from 1.142.0 in
July to 1.148.0 in October — and the version an installation runs is the
constant `z2ui5_if_app=>version`.

**What is stable.** The public API is the package `src/02`:
`z2ui5_if_app`, `z2ui5_if_client`, `z2ui5_if_ui5_exit`,
`z2ui5_if_ui5_monitor`, `z2ui5_cl_ui5_http_handler`,
`z2ui5_cl_ui5_view_builder` and the language-model seam. An app that uses
only those compiles on the next release or finds its change on the
deprecations page; the framework records the package symbol for symbol and
checks every change against it. Everything else — `src/00` and `src/01`, the
handler's internals, the draft service, the generated frontend classes — is
the framework's own and may change between two tags without notice. The
[linter](/advanced/linter) reports an app that reaches outside the public
package as `non-released-api`, which is the cheapest way to find out before
an upgrade does.

**What frozen means.** The package `src/99` holds what the framework
replaced and still ships: the predecessor view builder, the old handler
shim, the superseded exit and types interfaces, the retired utility classes,
the built-in popups. Nothing in the framework calls them, a gate refuses a
change to them, and an app that still uses one keeps compiling release after
release. Frozen is not stable: it means the code will not be fixed either.
Every object in it is named, with its successor, on
[Deprecations](/resources/deprecations) — the one page that names them.

**The deprecations page is the upgrade plan.** Its version-status table
names what has a successor, what the successor is, and the release that made
the change — and entries marked *next release* are already on `main` and not
in a tag yet, which tells an installation on the newest tag what the next pull
brings. `npx @abap2ui5/linter src --fix` rewrites the mechanical part of the
table over an app package and reports the rest.

**How the pull is done.** The framework is an abapGit repository, so an
upgrade is a pull of the next tag: switch the repository's branch to the tag
in abapGit and pull, which activates the changed objects. The standalone ICF
installation needs nothing else — the frontend is embedded in the backend
classes ([#11](/advanced/insights/11-no-build-no-deploy-no-cache)), so the
page and the modules the browser loads are the new ones with the same pull.
A launchpad installation has one more repository, the shell BSP from
[abap2UI5/frontend](https://github.com/abap2UI5/frontend)
([#26](/advanced/insights/26-a-tile-like-any-other)): the branch `standard`
carries the frontend itself and is pulled along, `standard_v2` is the same
for the legacy-free frontend, and `standard_new` carries only a container
that loads the frontend from the backend, so an installation on that branch
upgrades the launchpad shell with the backend pull alone
([Fiori Launchpad](/configuration/launchpad)). Add-ons are repositories of
their own with tags of their own, pulled the same way.

**Pinning.** A productive system installs a tag, not `main`
([Productive Usage](/configuration/productive_usage)): `main` may carry an API
no release has, and a pull of `main` on two days is two different frameworks.
The release number in the manual's version dropdown and on the deprecations
page is the one the manual is written against.

**Rolling back.** The previous tag is still there, and a pull of it is the
same operation as the upgrade. The framework writes one table, `z2ui5_t_01`,
and its rows are serialized instances that expire within hours
([#38](/advanced/insights/38-how-many-users)), so there is no data to migrate
in either direction; a user whose draft was written by the other version
restarts the app. Apps that already use an API the previous tag lacks do not
compile on it — the deprecations table says which release brought what.

**What to re-test.** The things a tag can change for an installed app, in the
order they fail:

- the **user exit**: `z2ui5_if_ui5_exit` is public API, but a field of its
  configuration structures may gain a default or a sibling — read the exit's
  own methods against the changelog, the CSP in particular
  ([Security](/configuration/security));
- the **app classes** against the deprecations table, with the linter over
  the package;
- **custom controls** in `z2ui5_ccc` and the popups and other add-ons, each
  against its own release;
- the **launchpad tile**: after the frontend BSP changes, the UI5 app index
  (`/UI5/APP_INDEX_CALCULATE`) and the HTTP cache in `SMICM`;
- one **roundtrip per app** in a browser, the one check that needs a system.
