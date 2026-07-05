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

#### How It Works
The `702` branch is not maintained by hand — it is **generated** from `main` by an automated GitHub Actions workflow on every change. The pipeline runs [abaplint](https://abaplint.org)'s downport rule (`abaplint --fix` with a 7.02 target configuration), which rewrites modern syntax into 7.02-compatible equivalents, for example:

- inline declarations `DATA(x) = ...` → separate `DATA` statements
- constructor expressions (`VALUE #( )`, `NEW #( )`, `CONV #( )`) → classic statements
- `xsdbool( )` → `boolc( )`
- string templates → concatenation where needed

A few small compatibility fix-ups follow (e.g. replacing exception types that don't exist on old releases), and the result is committed to the `702` branch. Because the transformation is fully automatic, the downported version stays feature-identical with `main` — you never wait for a manual backport.

The same mechanism runs in this project's CI (`npm run auto_downport`) to guarantee every change on `main` stays downportable.

#### Further Reading
Background article: [Running abap2UI5 on older R/3 Releases](https://www.linkedin.com/pulse/running-abap2ui5-older-r3-releases-downport-compatibility-abaplint-mjkle).
