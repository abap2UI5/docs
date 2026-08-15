---
outline: [2, 4]
---
# Namespaces, Renaming

Everything abap2UI5 installs lives under a single prefix: `Z2UI5_`. The first
half of this page describes what that prefix covers and how the names inside it
are structured; the second half is about installing abap2UI5 under a *different*
prefix, which is what makes several installations in one system possible.

## The `z2ui5` Namespace

#### Why a Prefix and Not a Package
ABAP has no package manager. Every object abapGit pulls is created **globally**
in the system, so two installations cannot hide behind their package the way two
npm modules hide behind `node_modules`. The only thing that keeps objects apart
is the name itself, which is why the whole framework — classes, interfaces,
exception classes and DDIC tables — carries the same prefix.

`Z2UI5_` is reserved for abap2UI5 and the projects of its ecosystem. Name your
own apps in your own customer namespace (`Z…` / `Y…`), and never add an object to
`Z2UI5_` yourself: the next upgrade may ship an object of that name, and abapGit
will overwrite yours.

| Object type | Pattern | Example |
|---|---|---|
| Class | `z2ui5_cl_*` | `z2ui5_cl_ui5_http_handler` |
| Interface | `z2ui5_if_*` | `z2ui5_if_app` |
| Exception class | `z2ui5_cx_*` | `z2ui5_cx_ui5_util_error` |
| DDIC table | `z2ui5_t_*` | `z2ui5_t_01` |

#### The Second Segment
A name has one more part than the prefix suggests:

```
z2ui5 _ cl _ ui5 _ http_handler
  │      │     │       │
  │      │     │       └── what it is
  │      │     └────────── which project and layer it comes from
  │      └──────────────── object type: cl, if, cx, t
  └─────────────────────── the namespace
```

That middle segment is what does the real work. `Z2UI5_` is shared by the
framework *and* every ecosystem project — samples, add-ons, control libraries all
install under it, into the same system, from separate repositories. The prefix
alone would let them collide; the segment is what keeps them apart.

**Inside the framework installation:**

| Segment | Meaning |
|---|---|
| *(none)* — `z2ui5_if_app`, `z2ui5_if_client`, `z2ui5_if_types`, `z2ui5_if_exit` | **The public API.** The four interfaces carry no segment on purpose: they are the contract, and a contract does not move between layers |
| `ui5` | The framework itself — the engine and the shipped apps (`z2ui5_cl_ui5_handler`, `z2ui5_cl_ui5_srv_draft`, `z2ui5_cl_ui5_app_start`), plus the two public classes `z2ui5_cl_ui5_http_handler` and `z2ui5_cl_ui5_view_builder` |
| `ui5f` | The UI5 **f**rontend, embedded as ABAP string constants and **generated** — never edit one by hand, the next build overwrites it |
| `ajson`, `srt` | [ajson](/technical/tools/ajson) and [S-RTTI](/technical/tools/srtti), mirrored from their upstream projects under this namespace |
| `util`, `pop`, `xml_view` | Frozen legacy code — still ships, still works, no longer developed. See [Deprecations](/resources/deprecations) |

**Everything else that installs under `Z2UI5_`:**

| Segment | Project |
|---|---|
| `smp` | [samples](https://github.com/abap2UI5/samples) — the sample catalog, `z2ui5_cl_smp_app_*` |
| `smps` | [samples-stack](https://github.com/abap2UI5/samples-stack) — full-stack samples |
| `smpc` | [samples-controls](https://github.com/abap2UI5/samples-controls) — control samples |
| `popup` | [popups](https://github.com/abap2UI5-addons/popups) — the popups [add-on](/advanced/addons) |
| `cci` | [custom-controls](https://github.com/abap2UI5-addons/custom-controls) — the custom control library |

The two tables share one rule and one exception. The rule: an object's segment
tells you which repository it was pulled from. The exception: frozen code keeps
the segment it was born with, so a repository's `src/99` can hold a segment that
is no longer issued — the popups add-on ships as `z2ui5_cl_popup_*` today, and
its `src/99` still carries the `pop` and `demo` names it started with, just as
the framework's own `src/99` does.

Only the public API is a contract. Every other segment may be restructured in any
release — the engine classes carried a `z2ui5_cl_core_*` segment not long ago,
and the generated frontend artifacts a `z2ui5_cl_app_*` one.

::: tip Enforced, not just documented
A CI gate (`check_object_naming`) fails the framework build when an object
outside the public API and the frozen package is added without the `ui5` / `ui5f`
segment. abaplint's own naming rule only checks the `Z2UI5_` prefix, so without
that gate a new segment would drift in unnoticed.
:::

#### Package Layout
The prefix is flat, the packages are not. abapGit is configured with
`FOLDER_LOGIC=PREFIX`, so the folders of a repository become the package
hierarchy in your system. For the framework repository that is:

| Package | Contents |
|---|---|
| `src/00/` | External libraries — ajson, S-RTTI and the vendored context/HTTP helpers |
| `src/01/` | Internal use only — draft persistence, request handling, event routing, binding, and the generated frontend |
| `src/02/` | The released API — the six objects above |
| `src/99/` | Frozen — the legacy view builder, the utility classes and the built-in popups |

The package a class sits in is the honest answer to "may I use this?": `src/02`
yes, `src/01` no, `src/99` only if your app already does.

#### Name Length
ABAP object names are limited to 30 characters, and abap2UI5 does not spend all
of them: every object name in `src/` stays at **25 characters or less**. The
remaining five are headroom for renaming — a namespace of up to 10 characters
replaces the 5-character `z2ui5` and still fits into 30.

This is why the generated frontend classes carry compressed names like
`z2ui5_cl_ui5f_scrfocus_js` rather than spelled-out ones: the generator caps them
and fails rather than truncating silently.

#### The Frontend Namespace
`z2ui5` exists a second time — in the browser, as the UI5 module namespace. It is
a **different namespace that happens to share the name**, and it is not an ABAP
object at all:

| Form | Example |
|---|---|
| Module IDs | `z2ui5/core/Server`, `z2ui5/model/formatter` |
| Globals | `z2ui5.Formatter`, `z2ui5.Util` |
| Custom control XML namespace | `xmlns:z2ui5="z2ui5.cc"` → `z2ui5/cc/<Name>` |
| The event constant | `cs_event-z2ui5` for [Custom JS](/cookbook/expert_more/custom_js) |

Renaming the ABAP side does not touch any of these — they stay `z2ui5` in every
installation, renamed or not. That is deliberate: the frontend namespace lives
inside one browser page, where only one abap2UI5 installation is ever loaded, so
there is nothing for it to collide with.

#### Your Own Objects
Nothing about the framework's naming applies to your apps. An app is a class in
your own namespace that implements `z2ui5_if_app`:

```abap
CLASS zcl_my_app DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
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

#### Why Rename?
Because objects are global, a system can hold only **one** version of abap2UI5 —
and every app in the system is forced to use it. Pulling the latest version can
then break existing apps. Public API changes are kept to a minimum, but a
constantly evolving project cannot avoid them entirely.

Renaming closes this gap. It is not a true package management system, but it lets
you install abap2UI5 multiple times under different namespaces in the same system
and upgrade each installation at its own pace. At the app level, you simply
implement the renamed app interface — e.g., `z2ui5_sd_if_app` or `z2411_if_app`
instead of `z2ui5_if_app`.

#### How It Works
[abaplint](https://abaplint.org) can rename ABAP artifacts across a whole repository: you define rename patterns (old name → new name, including regular expressions) in an abaplint configuration, and `abaplint --rename` rewrites every class, interface, and reference consistently, writing the result to an output folder:

```jsonc
"rename": {
  "output": "output",
  "patterns": [
    { "type": "CLAS|INTF", "oldName": "z2ui5(.*)", "newName": "zmyns$1" }
  ]
}
```

The renamed copy is a complete, installable abapGit project under your own namespace — install it side by side with the original, pin it to a release, or ship it inside your product. The abap2UI5 CI runs this transformation on every change (`npm run rename`, workflow `test_rename.yaml`) to guarantee the codebase stays renameable.

#### Step-by-Step Guide
Everything is already set up in the main repository: the on-demand GitHub Action `build_rename` renames all artifacts to a namespace of your choice and pushes the result as a ready-to-install branch. Renaming abap2UI5 takes just two steps:

1. **Fork** the [abap2UI5 repository](https://github.com/abap2UI5/abap2UI5)
2. **Run the Action** — in your fork, open the *Actions* tab (enable workflows when asked), select the **build_rename** workflow, and start it with your new namespace (a letter followed by letters, digits or underscores, max. 10 characters, e.g., `ZMYUI5`)

The workflow runs `abaplint --rename` with the checked-in configuration `.github/abaplint/rename.jsonc` and pushes the renamed sources to the branch `rename_<name>` (e.g., `rename_zmyui5`). The branch contains the complete renamed `src` tree together with a matching `.abapgit.xml` — **pull it with abapGit** into your ABAP system for a parallel installation next to the original.

To upgrade an installation later, sync your fork with upstream and re-run the workflow with the same name: the branch is updated to the current state (nothing is pushed when there are no content changes), and you simply pull again with abapGit.

#### What Renaming Does Not Cover
abaplint rewrites object names and every reference the compiler can see. Three
things fall outside that, and it is worth knowing them before you ship a renamed
installation.

**Class names written as strings.** A dynamic lookup passes its class or
interface name as text, and text is not a reference. One production lookup in
`src/` is affected: the user exit. `<ns>_cl_ui5_user_exit=>get_user_exit_class( )`
searches for classes implementing the literal `` `Z2UI5_IF_EXIT` ``, which in a
renamed installation is not the interface your exit implements — that one is
`<NS>_IF_EXIT`. The lookup therefore comes back empty (or, next to an original
installation, with the *other* installation's exit class, which then cannot be
instantiated into the renamed reference and is discarded). The search is wrapped
in `CATCH cx_root`, so nothing is reported: the installation just runs with the
default configuration and your [user exit](/advanced/extensibility/user_exits) is
never called — no custom theme, no bootstrap configuration, no CSP override.

::: warning Patch the two literals after renaming
In your renamed branch, change the literals in `get_user_exit_class( )` to your
own namespace (`` `ZMYUI5_IF_EXIT` `` and `` `ZMYUI5_CL_UI5_USER_EXIT` ``). Only
apps that use a user exit are affected — everything else in the renamed
installation works without a change.
:::

**The frontend namespace.** Module IDs, globals and the `z2ui5.cc` XML namespace
stay `z2ui5` in a renamed installation — see
[The Frontend Namespace](#the-frontend-namespace) above.

**Real SAP namespaces.** The `build_rename` workflow takes a plain prefix — a
letter followed by letters, digits or underscores. A registered SAP namespace
such as `/ZZZ/` is a regular expression away in the abaplint configuration, but
it is not what the workflow builds, and the namespace has to exist in the target
system before you can import objects into it.

#### Renaming in Practice: ajson
abap2UI5 itself relies on this feature: its JSON handling comes from the open-source project [ajson](https://github.com/sbcgua/ajson), which is integrated under the `z2ui5` namespace via renaming — so there are no collisions if you pull both abap2UI5 and ajson separately into the same system. A GitHub Action in the [mirror-ajson](https://github.com/abap2UI5/mirror-ajson) repository checks weekly for upstream changes and automatically creates a pull request with the latest ajson version renamed to `z2ui5`. abapGit bundles ajson under its own namespace the same way — renaming with abaplint also makes it possible to integrate open-source projects into each other.

#### Further Reading
- [Automagic standalone renaming of ABAP objects](https://community.sap.com/t5/application-development-blog-posts/automagic-standalone-renaming-of-abap-objects/ba-p/13499851)
- [Renaming of ABAP Artifacts — The Power of abaplint and abapGit in ABAP Development](https://www.linkedin.com/pulse/renaming-abap-artifacts-power-abaplint-github-actions-development-kqede/)
