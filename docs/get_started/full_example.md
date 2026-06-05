---
outline: [2, 4]
---
# Full Example

This tutorial walks through a complete app that follows a typical ABAP flow: a small selection screen, reading data from the database, showing the result in a table, opening a popup to edit a row, and posting the changes back. It ties together everything from the [Hello World](/get_started/hello_world) page and shows how the pieces fit into a real screen.

The example uses sales-order-like data but keeps the SELECTs and updates as plain ABAP so you can adapt them to your own tables. Drop the class into your system and launch it the same way as the Hello World app.

## What You Will Build

1. **Selection screen** — date range and a customer filter.
2. **Read data** — fetch matching orders into an internal table on button press.
3. **Result table** — show the orders, with one row editable via popup.
4. **Popup** — open a dialog that lets the user change the delivery date.
5. **Post** — confirm in the popup, write the change back, refresh the table.

## Tutorial

🚧 UNDER CONSTRUCTION 🚧

## What to Take Away

- One controller class, one `main` method, all state in public attributes — that is the whole app
- The view is rebuilt only when the structure changes. Edits, saves, and popup open/close do not need a fresh `view_display( )`
- Popups are just a different factory (`factory_popup`) on the same `z2ui5_cl_xml_view`, displayed via `popup_display` / `popup_destroy` while the main view stays in place
- Reading and writing the database is plain ABAP — abap2UI5 does not abstract that layer, which is what makes it easy to plug into existing code

From here, look at the [Cookbook](/cookbook/event_navigation/life_cycle) for value helps, navigation between apps, message handling, and other patterns you will need next.
