---
outline: [2, 4]
description: Guides for the tasks that come up in real projects - each one a whole job from the first click to the saved data, built from the cookbook's pieces and backed by a sample you can install.
---
# Everyday Work

The [Tutorial](/tutorials/) teaches the framework one concept at a time, and
the [Cookbook](/cookbook/) answers one question per chapter: how to bind a
table, how to open a popup, how to read a file. Real work is rarely one of
those on its own. An import, a mass change or an approval screen needs half a
dozen of them, in the right order, against a business object that has its own
rules.

The guides here take one such job and walk through it from start to finish:
what the user sees, which roundtrip does what, where the business logic stays,
and what goes wrong in practice. Each guide links the cookbook chapters it is
built from and ends with a sample you can pull with abapGit and run.

## Guides

| Guide | The job | Needs |
|---|---|---|
| [Excel Import into RAP](/everyday/excel_import_rap) | Upload an `.xlsx` file, check every row, create the rows in a RAP business object and show each refusal on its row | XCO XLSX (ABAP Cloud, or on-premise 2023 and later), a RAP business object |

## What makes a good guide

A guide belongs here when the job is common, takes more than one cookbook
chapter, and has a decision in it that a reader would otherwise get wrong the
first time - all or nothing versus row by row, a draft versus an active
instance, where a check belongs. Suggestions are welcome in the
[discussions](https://github.com/abap2UI5/abap2UI5/discussions).
