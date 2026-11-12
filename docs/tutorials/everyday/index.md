---
outline: [2, 4]
description: Guides for the tasks that come up in real projects. Each one is a whole job from the first click to the saved data, built from cookbook chapters and backed by a sample you can install.
---
# Everyday Work

The [steps of the Tutorial](/tutorials/) teach the framework one concept at a
time. The [Cookbook](/cookbook/) answers one question per chapter: how to bind a
table, how to open a popup, how to read a file. Real work is rarely one of those
on its own. An import, a mass change or an approval screen needs half a dozen of
them, in the right order, against a business object that has its own rules.

The guides here take one such job and walk through it from start to finish:
what the user sees, which roundtrip does what, where the business logic stays,
and what goes wrong in practice. Each guide links the cookbook chapters it is
built from and ends with a sample you can pull with abapGit and run.

## Guides

### Without RAP

These three work on any system that has the named library or service. Start
here if you come from classic reports.

| Guide | The job | Needs |
|---|---|---|
| [Report and Export](/tutorials/everyday/report_export) | A selection as range tables, one `SELECT`, a total, and the rows on the screen as an `.xlsx` download | XCO XLSX for the export |
| [Send by Mail](/tutorials/everyday/send_by_mail) | A mail form next to the data, the attachment written when sending, a clear answer whether the mail left | `cl_bcs_mail_message` and outbound mail configured |
| [Print Forms as PDF](/tutorials/everyday/print_forms) | Render an existing Adobe Form into a PDF instead of a printer, one file or a ZIP of several | Adobe Document Services (on-premise) |

### With a RAP Business Object

These six need RAP. Each guide names the release it starts from.

| Guide | The job | Needs |
|---|---|---|
| [Excel Import into RAP](/tutorials/everyday/excel_import_rap) | Upload an `.xlsx` file, check every row, create the rows in a RAP business object and show each refusal on its row | XCO XLSX (ABAP Cloud, or on-premise 2023 and later), a RAP business object |
| [Mass Change](/tutorials/everyday/mass_change) | Select many rows, describe the change once, preview old and new values, save all with one `UPDATE` | a RAP business object (1909 and later) |
| [Approval Worklist](/tutorials/everyday/approval_worklist) | Show what waits for a decision, accept or reject several items at once through the business object's actions | a RAP business object with actions |
| [Concurrent Editing](/tutorials/everyday/concurrent_editing) | Let two users edit the same instance without a lock while typing, and refuse a save over a change the user has not seen | a RAP business object with an ETag field |
| [File Attachments](/tutorials/everyday/file_attachments) | Store files in a business object, list them without their content, download and delete by key | a deep action parameter (ABAP Platform 2025 or ABAP Cloud) |
| [React to Changes](/tutorials/everyday/react_to_changes) | Log, notify or follow up whenever a business object changes, whoever changed it | RAP business events (ABAP Platform 2021 or ABAP Cloud) |

## Getting the Sample

Every guide is built on a sample in
[abap2UI5/samples-stack](https://github.com/abap2UI5/samples-stack), and names
the package it is in, for example package `03`. You do not have to install the
whole repository. Each package has a branch of its own, and the branch name
starts with the package number (`03-rap`, `05-business-events`, …):

1. In abapGit, create a *New Online* repository with the URL
   `https://github.com/abap2UI5/samples-stack`.
2. Choose the branch of the package the guide names, then pull and activate.
3. Start the sample class with `?app_start=<class>`, as on the
   [Hello World](/get_started/hello_world#starting-an-app-by-url) page.

The README of the repository lists every branch and the release each one needs.

## Suggest a Guide

A guide belongs here when the job is common, takes more than one cookbook
chapter, and contains a decision that is easy to get wrong the first time.
Examples of such decisions: all or nothing versus row by row, a draft versus an
active instance, where a check belongs. Suggestions are welcome in the
[discussions](https://github.com/abap2UI5/abap2UI5/discussions).
