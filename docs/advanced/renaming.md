---
outline: [2, 4]
description: The Z2UI5_ prefix and the naming scheme of every object abap2UI5 and its add-ons install, and the package layout. How the build-rename workflow installs the framework a second time under a prefix or registered namespace of your own. The 7.02 variant the same run builds, the manual way for ABAP Cloud, and two installations in one launchpad.
---
# Namespaces, Renaming

Everything abap2UI5 installs lives under a single prefix: `Z2UI5_`. The first
half of this page describes what that prefix covers and how the names inside it
are structured. The second half is about installing abap2UI5 under a *different*
prefix. That is what makes several installations in one system possible.

## The `z2ui5` Namespace

### Why a Prefix and Not a Package
ABAP has no package manager. Every object abapGit pulls is created **globally**
in the system, so two installations cannot hide behind their package the way two
npm modules hide behind `node_modules`. The only thing that keeps objects apart
is the name itself, which is why the whole framework — classes, interfaces,
exception classes and DDIC tables — carries the same prefix.

`Z2UI5_` is reserved for abap2UI5 and the projects of its ecosystem. Name your
own apps in your own customer namespace (`Z…` / `Y…`), and never add an object
to `Z2UI5_` yourself: the next upgrade may ship an object of that name, and
abapGit will overwrite yours.

| Object type | Pattern | Example |
|---|---|---|
| Class | `z2ui5_cl_*` | `z2ui5_cl_ui5_http_handler` |
| Interface | `z2ui5_if_*` | `z2ui5_if_app` |
| Exception class | `z2ui5_cx_*` | `z2ui5_cx_ui5_util_error` |
| DDIC table | `z2ui5_t_*` | `z2ui5_t_01` |

### The Second Segment
The scheme the abap2UI5 repositories share is
`z2ui5_<type>_<segment>_<object>` — a name has one more part than the prefix
suggests:

```
z2ui5 _ cl _ ui5 _ http_handler
  │      │     │       │
  │      │     │       └── what it is
  │      │     └────────── which project and layer it comes from
  │      └──────────────── object type: cl, if, cx, t
  └─────────────────────── the namespace
```

That middle segment is what does the real work. `Z2UI5_` is shared by the
framework *and* every ecosystem project — samples, add-ons, control libraries
all install under it, into the same system, from separate repositories. The
prefix alone would let them collide; the segment is what keeps them apart.

**Inside the framework installation:**

| Segment | Meaning |
|---|---|
| *(none)* — `z2ui5_if_app`, `z2ui5_if_client` | **The public API.** Both interfaces carry no segment on purpose: they are the contract, and a contract does not move between layers |
| `ui5` | The framework itself — the engine and the shipped apps (`z2ui5_cl_ui5_handler`, `z2ui5_cl_ui5_srv_draft`, `z2ui5_cl_ui5_app_start`), plus the released classes and interfaces of `src/02` that carry it (`z2ui5_cl_ui5_http_handler`, `z2ui5_cl_ui5_view_builder`, `z2ui5_cl_ui5_test`, `z2ui5_if_ui5_exit`, `z2ui5_if_ui5_monitor`, the `z2ui5_*_ui5_llm` objects) |
| `ui5f` | The UI5 **f**rontend, embedded as ABAP string constants and **generated** — never edit one by hand, the next build overwrites it |
| `ajson`, `srt` | [ajson](/technical/tools/ajson) and [S-RTTI](/technical/tools/srtti), mirrored from their upstream projects under this namespace |
| `util`, `pop`, `xml_view` | Frozen, shipped from [abap2UI5-ext](https://github.com/abap2UI5/abap2UI5-ext) — see [Deprecations](/resources/deprecations) |

**Everything else that installs under `Z2UI5_`:**

| Segment | Project |
|---|---|
| `smp` | [samples](https://github.com/abap2UI5/samples) — the sample catalog, `z2ui5_cl_smp_app_*` |
| `smps` | [samples-stack](https://github.com/abap2UI5/samples-stack) — full-stack samples |
| `smpc` | [samples-controls](https://github.com/abap2UI5/samples-controls) — control samples |
| `popup` | [popups](https://github.com/abap2UI5-addons/popups) — the popups [add-on](/resources/addons) |
| `cci` | [custom-controls](https://github.com/abap2UI5-addons/custom-controls) — custom controls shared with the community |
| `ccc` | [custom-controls-customer](https://github.com/abap2UI5-addons/custom-controls-customer) — the template for **your own** frontend artifacts |

The two tables share one rule and one exception. The rule: an object's segment
tells you which repository it was pulled from. The exception: frozen code keeps
the segment it started with. So a repository's `src/99` can hold a segment that
is no longer issued. For example, the popups add-on ships as `z2ui5_cl_popup_*`
today, and its `src/99` still carries the `pop` and `demo` names it started
with. abap2UI5-ext does the same with the objects that left the framework.

::: tip The segment can reach the frontend too
For the two custom-control repositories the segment also names the
resourceRoot of their controls. `cci` loads under the root `z2ui5_cci`, served
by the abap2UI5 endpoint out of the add-on's classes. `ccc` ships the BSP
`Z2UI5_CCC` under `z2ui5_ccc`. abap2UI5 reserves both roots in its
`manifest.json`, which is what makes those controls loadable without patching
any framework file. The two differ in the last letter only. A typo in a
view's `xmlns:` shows only as a failed module request in the browser console. So
when a custom control does not render, check that letter first.
:::

Only the public API is a contract. Every other segment may be restructured in
any release.

::: tip Enforced, not just documented
The framework has a CI check for this: `.github/scripts/object-naming-gate.mjs`
(`npm run check:naming`). It fails the build when a new object lacks the `ui5` /
`ui5f` segment. The public API and the two mirrored libraries are exempt.
abaplint's own naming rule only checks the `Z2UI5_` prefix, so without that
check a new segment would drift in unnoticed.
:::

### Package Layout
The prefix is flat, the packages are not. abapGit is configured with
`FOLDER_LOGIC=PREFIX`, so the folders of a repository become the package
hierarchy in your system. For the framework repository that is:

| Package | Contents |
|---|---|
| `src/00/` | External libraries — ajson (`src/00/01`), S-RTTI (`src/00/02`) and the framework's own utility classes (`src/00/03`) |
| `src/01/` | Internal use only — draft persistence, request handling, event routing, binding, and the generated frontend |
| `src/02/` | The released API — `z2ui5_if_app`, `z2ui5_if_client`, `z2ui5_cl_ui5_http_handler`, `z2ui5_cl_ui5_view_builder`, `z2ui5_if_ui5_exit`, `z2ui5_if_ui5_monitor`, `z2ui5_cl_ui5_test`, and `z2ui5_if_ui5_llm` with `z2ui5_cl_ui5_llm` and `z2ui5_cx_ui5_llm` |
| `src/03/` | The service — the ICF node `/sap/bc/z2ui5`, its handler `z2ui5_cl_ui5_icf_handler`, and the BSP `Z2UI5` for the launchpad. The branch `cloud` carries another package in its place ([Deployment Scenarios](/configuration/installation#frontend-branches)) |

The package a class sits in is the honest answer to "may I use this?": `src/02`
yes, `src/01` and `src/03` no.

### Name Length
ABAP object names are limited to 30 characters, and abap2UI5 does not spend all
of them: every object name in `src/` stays at **25 characters or less**. The
remaining five are headroom for renaming — a namespace of up to 10 characters
replaces the 5-character `z2ui5` and still fits into 30.

This is why the generated frontend classes carry short names like
`z2ui5_cl_ui5f_bundle` and `z2ui5_cl_ui5f_preload`: the generator caps them and
fails rather than truncating silently.

### The Frontend Namespace
`z2ui5` exists a second time — in the browser, as the UI5 module namespace. It
is a **different namespace that happens to share the name**, and it is not an
ABAP object at all:

| Form | Example |
|---|---|
| Module IDs | `z2ui5/core/Server`, `z2ui5/model/formatter` |
| Custom control XML namespace | `xmlns:z2ui5="z2ui5.cc"` → `z2ui5/cc/<Name>` |
| Reserved sibling roots | `z2ui5_cci` (served by the endpoint) and `z2ui5_ccc` (served by its BSP), see above |

Renaming the ABAP side does not touch any of these — they stay `z2ui5` in every
installation, renamed or not. A renamed BSP keeps the UI5 identity as well: the
component `z2ui5.Component`, the app id `z2ui5` and the inbound `z2ui5-display`.
On the standalone URL of the BSP or the ICF node that is safe, because that page
loads one installation and nothing else.

The Fiori launchpad is different: it is one page for the whole session. When a
user opens an app of one renamed installation and then an app of another one of
a different version, the second may run on the frontend modules the first has
already loaded, and its frontend and backend no longer match. That has not been
tested yet. Until it is, give each launchpad one abap2UI5 installation, or
isolate the second one, for example by starting its apps in their own iframe.

### Your Own Objects
Nothing about the framework's naming applies to your apps. An app is a class in
your own namespace that implements `z2ui5_if_app`:

```abap
CLASS zcl_my_app DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
```

The only place the framework's namespace reaches into your code is the interface
name — which is exactly the part that changes when you rename the installation
your app runs on.

## Renaming

You may need abap2UI5 under a different prefix to fit customer-specific
requirements, like:
- Different modules, e.g., `z2ui5_sd`, `z2ui5_mm`
- Release-specific naming, e.g., `z2411`, `z2502`
- One installation per app, e.g., `z2ui5app1`, `z2ui5app2`

abap2UI5 works with the abaplint renaming feature and supports namespaces up to
10 characters, e.g., `zabap2ui5`.

### Why Rename?
Because objects are global, a system can hold only **one** version of abap2UI5 —
and every app in the system is forced to use it. Pulling the latest version can
then break existing apps. Public API changes are kept to a minimum, but a
constantly evolving project cannot avoid them entirely.

Renaming closes this gap. It is not a true package management system, but it
lets you install abap2UI5 multiple times under different namespaces in the same
system and upgrade each installation at its own pace. At the app level, you
simply implement the renamed app interface — e.g., `z2ui5_sd_if_app` or
`z2411_if_app` instead of `z2ui5_if_app`.

### How It Works
[abaplint](https://abaplint.org) can rename ABAP artifacts across a whole
repository. You define rename patterns (old name → new name, including regular
expressions) in an abaplint configuration. `abaplint --rename` then rewrites
every class, interface and reference consistently and writes the result to an
output folder:

```jsonc [abaplint.json]
"rename": {
  "output": "output",
  "patterns": [
    { "type": "CLAS|INTF", "oldName": "z2ui5(.*)", "newName": "zmyns$1" }
  ]
}
```

The renamed copy is a complete, installable abapGit project under your own
namespace — install it side by side with the original, pin it to a release, or
ship it inside your product. The abap2UI5 CI runs this transformation on every
pull request (`npm run rename`, in the workflow `abaplint.yaml`) to guarantee
the codebase stays renameable. The workflow `build-rename.yaml` below is the
one you start yourself.

### Step-by-Step Guide
Everything is already set up in the main repository: the on-demand GitHub Action
`build-rename` renames all artifacts to a namespace of your choice and pushes
the result as a ready-to-install branch. Renaming abap2UI5 takes just two steps:

1. **Fork** the [abap2UI5 repository](https://github.com/abap2UI5/abap2UI5)
2. **Run the Action** — in your fork, open the *Actions* tab (enable workflows
   when asked) and select the **build-rename** workflow. Start it with your new
   namespace, e.g., `ZMYUI5`: a letter followed by letters, digits or
   underscores, max. 10 characters. The second input, `with_702`, is on by
   default and also builds the 7.02 variant; switch it off if you do not need
   one

The workflow runs `abaplint --rename` with the checked-in configuration
`.github/abaplint/rename.jsonc` and pushes two branches:

| Branch | For | Contents |
|---|---|---|
| `rename_<name>` (e.g., `rename_zmyui5`) | Standard ABAP from 7.50 | the complete renamed `src` tree with a matching `.abapgit.xml`. The ICF node, its handler and the BSP of `src/03` are renamed with the rest, so the branch is a whole installation |
| `rename_<name>_702` | NetWeaver 7.02 to 7.4x | the same renamed tree, run through the downport that builds the branch `702` and linted with the 7.02 rules before it is pushed. No BSP, as on `702` ([Downporting](/advanced/downporting)) |

**Pull the one that fits your system with abapGit** for a parallel installation
next to the original. For ABAP Cloud see [ABAP Cloud](#abap-cloud-and-7-02)
below.

**Rename the add-ons with the same name.** An add-on with a `build-rename`
workflow of its own, such as custom-controls, renames every reference to
abap2UI5 along, so its branch `rename_<name>` runs on the abap2UI5 renamed with
that name. The renamed endpoint asks for the community controls under its own
prefix as well: `zmyui5_cl_cci_bundle` in `ZMYUI5`, `/ca2ui5/cl_cci_bundle` in
`/CA2UI5/`.

To upgrade an installation later, sync your fork with upstream and re-run the
workflow with the same name. The branches are updated to the current state;
nothing is pushed when there are no content changes. Then pull again with
abapGit.

### ABAP Cloud {#abap-cloud-and-7-02}
There is no renamed build of the branch `cloud`: its service package holds an
HTTP service, a communication scenario and IAM objects, and the rename does not
cover these types. The packages `src/00` to `src/02` are the same on `cloud` as
on `main`, and the rename handles them. The service objects are created in ADT
anyway, so you create them yourself:

1. Run `build-rename` as described above.
2. Take `src/00`, `src/01` and `src/02` of the branch `rename_<name>`, without
   `src/03`: that is the Standard ABAP service and does not activate on ABAP
   Cloud. Push them to a branch of your fork and pull that branch with abapGit
   for Eclipse, as on [ABAP Cloud](/configuration/abap_cloud#installation).
3. In ADT, create the objects of `src/03` of the branch `cloud` under your name,
   in a package of your own:

| On the branch `cloud` | Object type | Create as |
|---|---|---|
| `z2ui5_cl_ui5_icf_handler` | Class implementing `if_http_service_extension` | `<name>_cl_ui5_icf_handler`, calling `<name>_cl_ui5_http_handler=>run( )` ([The Handler](/configuration/abap_cloud#the-handler)) |
| `Z2UI5` | HTTP service, with its authorization default values (SUSH) | `<NAME>`, handler `<name>_cl_ui5_icf_handler` |
| `Z2UI5_HTTP` | Communication scenario (SCO2) with its inbound service `Z2UI5_HTTP_IBS` (SIA6) | `<NAME>_HTTP` |
| `Z2UI5_IAM_EXT` | IAM app (SIA6) for the HTTP service | `<NAME>_IAM_EXT` |
| `Z2UI5_BC` | Business catalog (SIA1) | `<NAME>_BC` |
| `Z2UI5_BC_0001` | Business catalog app assignment (SIA7) of the IAM app | `<NAME>_BC_0001` |

The IAM app on `cloud` also names the deployed launchpad app. If business users
reach the renamed installation through a tile, deploy the folder `app` under a
name of your own, with its data source pointing at the renamed HTTP service, and
read [The Frontend Namespace](#the-frontend-namespace) about two installations
in one launchpad first.

### What Renaming Does Not Cover
abaplint rewrites object names and every reference the compiler can see: a
`TYPE REF TO`, a static call, an `INTERFACES`. It leaves a string literal as it
is. No production code of the framework names one of its own objects in a
literal; a dynamic lookup takes the name from a typed reference instead. The
check `.github/scripts/rename-literal-gate.mjs` (`npm run check:rename`) keeps
it that way. It fails a pull request that adds such a literal, and the
`build-rename` workflow runs it once more over the renamed tree. Two things stay
outside the rename on purpose.

**The frontend namespace.** Module IDs and the `z2ui5.cc` XML namespace
stay `z2ui5` in a renamed installation, and so does the UI5 identity of the
BSP. Two renamed installations in one launchpad session are therefore not
tested yet — see [The Frontend Namespace](#the-frontend-namespace) above.

**Real SAP namespaces.** The `build-rename` workflow takes a plain name
(`ZMYUI5`) or a registered SAP namespace with both slashes (`/CA2UI5/`), at
most 10 characters either way: `z2ui5_cl_ui5_client` becomes
`/ca2ui5/cl_ui5_client`, abapGit stores it as `#ca2ui5#cl_ui5_client`, and the
branch is `rename_ca2ui5`. The namespace has to be set up in the target system
(transaction SE03) and a package of that namespace has to exist before abapGit
can pull into it.

### Renaming in Practice: ajson
abap2UI5 itself relies on this feature. Its JSON handling comes from the
open-source project [ajson](https://github.com/sbcgua/ajson), which is
integrated under the `z2ui5` namespace via renaming. So there are no collisions
if you pull both abap2UI5 and ajson separately into the same system. The
[mirror-ajson](https://github.com/abap-util/mirror-ajson) repository holds ajson
renamed to the `zabaputil_` of `abap-util`, and once a month a workflow of the
framework renames what it needs to `z2ui5_cl_ui5_ajs` and opens a pull request
with what changed. abapGit bundles ajson under its own namespace the same way.
So renaming with abaplint also makes it possible to integrate open-source
projects into each other.

### Background
The steps above are complete on their own. Two articles describe how the
rename came about:
- [Automagic standalone renaming of ABAP objects](https://community.sap.com/t5/application-development-blog-posts/automagic-standalone-renaming-of-abap-objects/ba-p/13499851)
- [Renaming of ABAP Artifacts — The Power of abaplint and abapGit in ABAP Development](https://www.linkedin.com/pulse/renaming-abap-artifacts-power-abaplint-github-actions-development-kqede/)
