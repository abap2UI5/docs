---
outline: [2, 4]
---
# Namespaces

Everything abap2UI5 installs lives under a single prefix: `Z2UI5_`. This page
describes what that prefix covers, how the names inside it are structured, which
of them are a contract you can rely on — and where the ABAP namespace ends and
the UI5 one begins.

If you need abap2UI5 under a *different* prefix, that is
[Renaming](/advanced/renaming).

#### Why a Prefix and Not a Package
ABAP has no package manager. Every object abapGit pulls is created **globally**
in the system, so two installations cannot hide behind their package the way two
npm modules hide behind `node_modules`. The only thing that keeps objects apart
is the name itself, which is why the whole framework — classes, interfaces,
exception classes and DDIC tables — carries the same prefix.

`Z2UI5_` is reserved for the framework and the objects it ships. Name your own
apps in your own customer namespace (`Z…` / `Y…`), and never add an object to
`Z2UI5_` yourself: the next upgrade may ship an object of that name, and abapGit
will overwrite yours.

| Object type | Pattern | Example |
|---|---|---|
| Class | `z2ui5_cl_*` | `z2ui5_cl_ui5_http_handler` |
| Interface | `z2ui5_if_*` | `z2ui5_if_app` |
| Exception class | `z2ui5_cx_*` | `z2ui5_cx_ui5_util_error` |
| DDIC table | `z2ui5_t_*` | `z2ui5_t_01` |

#### Segments Inside the Namespace
The part after `z2ui5_cl_` / `z2ui5_if_` says which layer an object belongs to.
It is worth reading, because it tells you whether you are looking at something
you may call, something internal, or something that only still ships for
compatibility:

| Segment | Meaning |
|---|---|
| `z2ui5_if_app`, `z2ui5_if_client`, `z2ui5_if_types`, `z2ui5_if_exit`, `z2ui5_cl_ui5_http_handler`, `z2ui5_cl_ui5_view_builder` | **The public API.** Six objects, the stable contract for app developers |
| `z2ui5_cl_ui5_*` | The framework engine and the shipped apps — internal, e.g. `z2ui5_cl_ui5_handler`, `z2ui5_cl_ui5_srv_draft`, `z2ui5_cl_ui5_app_start` |
| `z2ui5_cl_ui5f_*` | The UI5 **f**rontend, embedded as ABAP string constants and **generated** — never edit one by hand, the next build overwrites it |
| `z2ui5_cl_ajson*`, `z2ui5_if_ajson*` | [ajson](/technical/tools/ajson), mirrored from the upstream project under this namespace |
| `z2ui5_cl_srt_*` | [S-RTTI](/technical/tools/srtti), mirrored the same way |
| `z2ui5_cl_util*`, `z2ui5_cl_pop_*`, `z2ui5_cl_xml_view*`, `z2ui5_cl_http_handler` | Frozen legacy code — still ships, still works, no longer developed. See [Deprecations](/resources/deprecations) |

Only the first row is a contract. Everything else may be renamed or restructured
in any release — the engine classes carried a `z2ui5_cl_core_*` segment not long
ago, and the generated frontend artifacts a `z2ui5_cl_app_*` one.

::: tip Enforced, not just documented
A CI gate (`check_object_naming`) fails the build when an object outside the
public API and the frozen package is added without the `ui5` / `ui5f` segment.
abaplint's own naming rule only checks the `Z2UI5_` prefix, so without that gate
a new segment would drift in unnoticed.
:::

#### Package Layout
The prefix is flat, the packages are not. abapGit is configured with
`FOLDER_LOGIC=PREFIX`, so the folders in the repository become the package
hierarchy in your system:

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
remaining five are headroom for [renaming](/advanced/renaming) — a namespace of
up to 10 characters replaces the 5-character `z2ui5` and still fits into 30.

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
name — which is exactly the part that changes when you
[rename](/advanced/renaming) the installation your app runs on.
