# Security policy

## Reporting a vulnerability

Please use the GitHub Security Advisory
["Report a Vulnerability"](https://github.com/abap2UI5/docs/security/advisories/new)
tab. Do not open a public issue for a security report.

Expect an acknowledgement within a few days. This project is developed
alongside other work, so a fix is agreed rather than promised by a date — the
advisory is where that conversation happens.

## Supported versions

Only what is **currently published** at
[abap2ui5.github.io/docs](https://abap2ui5.github.io/docs/) is supported.
There are no released versions to patch: `main` is the site, and a fix is
deployed by merging it.

## What this site is, from a security point of view

- **There is no server.** The site is static files, written by
  `scripts/build-site.mjs` and served from GitHub Pages. No backend, no SAP system, no account, no session,
  and nothing a reader types is sent anywhere.
- **The one thing that executes is the Run button.** A fenced example that
  carries one links into
  [abap2UI5/playground](https://github.com/abap2UI5/playground), which
  transpiles the ABAP and runs it **in the reader's own browser**. The code
  travels in the URL fragment, which a browser never sends to a server. The
  playground's own [security policy](https://github.com/abap2UI5/playground/blob/main/SECURITY.md)
  covers what happens once the reader is there.
- **The pages are not a source of truth about the framework.** The API
  reference, the version numbers and the sample links are generated from, or
  checked against, `abap2UI5` and its sibling repositories at a pinned release.
  A wrong statement here is usually wrong there first — but report it here if
  that is where you found it.
- **Dependencies are pinned.** What the build fetches beyond them is the
  organisation's own: the framework's interface and the sample catalogues from
  `raw.githubusercontent.com`, and the bar, two stylesheets, the search box and
  the ABAP highlighter from the playground's published site
  (`abap2ui5.github.io/playground`), which the build runs to colour the
  examples - so a compromise of that deployment would reach this build.

## Out of scope

- A page that is out of date, incomplete, or wrong about the framework. That is
  a documentation bug and belongs in
  [an issue](https://github.com/abap2UI5/docs/issues) — it is what the fifteen
  gates in [AGENTS.md](AGENTS.md) exist to catch, so a report that names one
  they missed is genuinely useful.
- Anything a reader does to their own browser tab with code they wrote
  themselves.
