---
outline: [2, 4]
---
# XML Templating

abap2UI5 sends views to the browser as XML strings. XML Templating lets you construct those strings dynamically in ABAP — branching on conditions, looping over data, and composing fragments — so that the view the browser receives is already fully resolved without any client-side template engine.

#### How It Works

Every abap2UI5 view is ultimately an XML string. You can build that string however you like: literal assignment, string concatenation, helper method calls, or a dedicated XML builder. The browser never sees template syntax — it only receives the final, rendered XML.

```abap
METHOD z2ui5_if_app~main.
  IF client->check_on_init( ).
    DATA(xml) = z2ui5_cl_xml_view=>factory( ).
    client->view_display( xml->stringify( ) ).
  ENDIF.
ENDMETHOD.
```

#### Conditional Rendering

Use ABAP `IF` statements to include or exclude controls depending on runtime state:

```abap
DATA(xml) = z2ui5_cl_xml_view=>factory( ).
DATA(page) = xml->page( 'Conditional View' ).

IF is_admin = abap_true.
  page->button(
    text  = 'Admin Action'
    press = client->_event( 'ADMIN_ACTION' ) ).
ENDIF.

page->text( 'Hello, ' && username ).
client->view_display( xml->stringify( ) ).
```

Only the controls added to the builder are serialized — nothing from the skipped branch appears in the XML.

#### Repeating Controls

Loop over an internal table to generate repeated controls:

```abap
DATA(list) = page->list( ).

LOOP AT items INTO DATA(item).
  list->item(
    title       = item-title
    description = item-description ).
ENDLOOP.
```

Each iteration appends a child node. The resulting XML contains exactly as many `<Item>` elements as there were rows in `items`.

#### Composing Fragments

Break complex views into reusable helper methods that each return a subtree:

```abap
METHOD build_header.
  rv_header = io_page->toolbar( )->title( title ).
ENDMETHOD.

METHOD build_table.
  DATA(tbl) = io_page->table( ).
  " ... add columns and rows
  rv_table = tbl.
ENDMETHOD.
```

Call the helpers in sequence inside `main` to assemble the final view. Because the builder accumulates children as regular ABAP object references, standard ABAP patterns (loops, conditions, method calls, function modules) all apply without restriction.

#### Dynamic Column Sets

A common pattern is selecting which columns to show based on user settings or authorization:

```abap
DATA(cols) = xml->table( )->columns( ).

IF show_price = abap_true.
  cols->column( )->text( 'Price' ).
ENDIF.
IF show_stock = abap_true.
  cols->column( )->text( 'Stock' ).
ENDIF.
```

The same loop that drives the column headers can drive the cell binding paths, keeping them in sync.

#### Tips

- Build the full view on every roundtrip. abap2UI5 is stateless by default, so the view is always reconstructed from the current model state — there is no diff or patch step.
- Keep view-construction logic out of business-logic methods. A dedicated `build_view` method or a set of small fragment builders keeps `main` readable.
- Prefer the fluent builder API (`z2ui5_cl_xml_view`) over raw string concatenation. The builder escapes special characters automatically and produces well-formed XML.

See `Z2UI5_CL_DEMO_APP_032` and `Z2UI5_CL_DEMO_APP_033` for complete examples of conditional and dynamic views.
