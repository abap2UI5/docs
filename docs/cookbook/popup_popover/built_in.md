---
outline: [2, 4]
---
# Built-In

Pre-built popup classes cover the most common cases. Call them via [navigation](/cookbook/event_navigation/navigation) — `client->nav_app_call( ... )` — and read the result back with `client->get_app_prev( )` when your app regains control:

| Class | Purpose |
| --- | --- |
| `Z2UI5_CL_POP_TO_CONFIRM` | Confirmation dialog with confirm/cancel events |
| `Z2UI5_CL_POP_TO_INFORM` | Information message dialog |
| `Z2UI5_CL_POP_TO_SELECT` | Select one or more entries from an internal table |
| `Z2UI5_CL_POP_TABLE` | Display an internal table |
| `Z2UI5_CL_POP_DATA` | Display any data object (structure or table) |
| `Z2UI5_CL_POP_TEXTEDIT` | Multi-line text editor |
| `Z2UI5_CL_POP_INPUT_VAL` | Prompt the user for a single input value |
| `Z2UI5_CL_POP_GET_RANGE` | Maintain range / select-option criteria |
| `Z2UI5_CL_POP_GET_RANGE_M` | Maintain multiple ranges at once |
| `Z2UI5_CL_POP_FILE_UL` | File upload |
| `Z2UI5_CL_POP_FILE_DL` | File download |
| `Z2UI5_CL_POP_PDF` | PDF preview |
| `Z2UI5_CL_POP_HTML` | Display HTML content |
| `Z2UI5_CL_POP_IMAGE_EDITOR` | Edit an image (crop, resize, filter) |
| `Z2UI5_CL_POP_MESSAGES` | Display a message table |
| `Z2UI5_CL_POP_BAL` | Display a Business Application Log (BAL) |
| `Z2UI5_CL_POP_ERROR` | Error display |
| `Z2UI5_CL_POP_JS_LOADER` | Load custom JavaScript libraries |
| `Z2UI5_CL_POP_DEMO_OUTPUT` | Quick demo-style output of arbitrary data |

Help expand this collection to cover more cases — contributions are welcome.
