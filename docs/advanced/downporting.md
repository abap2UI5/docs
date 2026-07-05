---
outline: [2, 4]
---
# Downporting

abap2UI5 works right away on ABAP 7.50 and later. On an earlier release, install the downported version, which supports R/3 NetWeaver 7.02 and later.

#### Branch
To install on an older system, use the `702` branch:
| Branch    | System                |
|-----------| ----------------------------|
| main     | S/4 Public Cloud, BTP ABAP Environment, S/4 Private Cloud, S/4 On-Premise, R/3 NetWeaver ≥7.50 |
| 702  |  R/3 NetWeaver ≥7.02   |

Some sample projects and other repositories also ship a downported version. Check whether a `702` branch is available.

#### Installation
The installation on an old release follows the normal [installation](/configuration/installation) flow with two adjustments:
1. Pull the `702` branch with abapGit — abapGit itself is 7.02-compatible, so deployment to old ECC machines works the same way as everywhere else
2. Create the HTTP handler (SICF service) with the 7.02-compatible handler syntax instead of the modern snippet

After that, call your HTTP handler from the browser and abap2UI5 is ready for use — even on your old machine.

If you later want to reach these apps from the cloud, use the [RFC Connector](/advanced/rfc): it calls abap2UI5 apps on legacy systems via BTP destination and RFC, integrating them into Steampunk, S/4 Public Cloud, Fiori Launchpads, or SAP Mobile Start. The [HTTP Connector](/advanced/http) offers the same over HTTP.

#### How It Works
The `702` branch is not maintained by hand — it is **generated** from `main` by an automated GitHub Actions workflow on every change. The pipeline runs [abaplint](https://abaplint.org)'s downport rule (`abaplint --fix` with a 7.02 target configuration), which rewrites modern syntax into 7.02-compatible equivalents, for example:

- inline declarations `DATA(x) = ...` → separate `DATA` statements
- constructor expressions (`VALUE #( )`, `NEW #( )`, `CONV #( )`) → classic statements
- `xsdbool( )` → `boolc( )`
- string templates → concatenation where needed

A few small compatibility fix-ups follow (e.g. replacing exception types that don't exist on old releases), and the result is committed to the `702` branch. Because the transformation is fully automatic, the downported version stays feature-identical with `main` — you never wait for a manual backport. And since the main code line stays on 7.50 syntax, contributors write and debug modern ABAP instead of 7.02 syntax.

The same mechanism runs in this project's CI (`npm run auto_downport`) to guarantee every change on `main` stays downportable.

#### Why abap2UI5 Is Downportable
Automatic syntax rewriting is only the last step. The codebase is compatible with everything from ABAP 7.02 up to the newest ABAP Cloud because of four design principles:

**1. Only basic technology.** No OData services, CDS views, or new EML syntax — none of these exist on older ECCs. Conversely, no SAP GUI or other statements forbidden in ABAP Cloud. The only universally available technology is a plain HTTP service (defined via SICF on-premise, via Eclipse in the cloud), and that is all abap2UI5 needs for communication.

**2. Few SAP dependencies.** SAP APIs like `/UI2/CL_JSON` or function modules may be missing or behave differently on certain releases, and their availability per release is poorly documented. abap2UI5 therefore replaces SAP dependencies with open-source projects where possible — most importantly [ajson](/technical/tools/ajson) for JSON handling, which is compatible from 7.02 to ABAP Cloud and ships bundled under the `z2ui5` namespace. Browse [dotabap.org](https://dotabap.org) for more open-source projects that can reduce your own dependencies.

**3. Version-independent code.** Where a dependency is unavoidable and SAP offers different APIs per language version (e.g., generating UUIDs), abap2UI5 uses a hybrid approach: dynamic ABAP calls the new API in ABAP for Cloud and falls back to the classic API in Standard ABAP — one code line, no syntax errors on either side. Not always elegant, but very pragmatic.

**4. Downported syntax.** The main code line is written in 7.50 syntax and the `702` branch is generated automatically, as described above.

Keep these principles in mind if you want to set up your next abapGit project with compatibility for older releases.

#### Further Reading
Background article: [Running abap2UI5 on older R/3 Releases](https://www.linkedin.com/pulse/running-abap2ui5-older-r3-releases-downport-compatibility-abaplint-mjkle).
