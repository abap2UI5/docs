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
| [Mass Change](/everyday/mass_change) | Select many rows, describe the change once, preview old and new values, save all with one `UPDATE` | a RAP business object (1909 and later) |
| [Approval Worklist](/everyday/approval_worklist) | Show what waits for a decision, accept or reject several items at once through the business object's actions | a RAP business object with actions |
| [Report and Export](/everyday/report_export) | A selection as range tables, one `SELECT`, a total, and the rows on the screen as an `.xlsx` download | XCO XLSX for the export |
| [Concurrent Editing](/everyday/concurrent_editing) | Let two users edit the same instance without a lock while typing, and refuse a save over a change the user has not seen | a RAP business object with an ETag field |
| [File Attachments](/everyday/file_attachments) | Store files in a business object, list them without their content, download and delete by key | a deep action parameter (ABAP Platform 2025 or ABAP Cloud) |
| [Send by Mail](/everyday/send_by_mail) | A mail form next to the data, the attachment written when sending, a clear answer whether the mail left | `cl_bcs_mail_message` and outbound mail configured |
| [Print Forms as PDF](/everyday/print_forms) | Render an existing Adobe Form into a PDF instead of a printer, one file or a ZIP of several | Adobe Document Services (on-premise) |
| [React to Changes](/everyday/react_to_changes) | Log, notify or follow up whenever a business object changes, whoever changed it | RAP business events (ABAP Platform 2021 or ABAP Cloud) |

## What makes a good guide

A guide belongs here when the job is common, takes more than one cookbook
chapter, and has a decision in it that a reader would otherwise get wrong the
first time - all or nothing versus row by row, a draft versus an active
instance, where a check belongs. Suggestions are welcome in the
[discussions](https://github.com/abap2UI5/abap2UI5/discussions).
