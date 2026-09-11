/*
 * The parser behind the generated client API reference, on the one thing it
 * got wrong in public: ABAP-Doc is XML, so the interface writes an apostrophe
 * as `&apos;` - and resources/api.md printed `sap.tnt&apos;s` for weeks,
 * because the decoder knew three entities and that was not one of them. The
 * model has to carry plain text; the renderer escapes for its own target.
 *
 *   npm test
 */
import test from 'node:test';
import assert from 'node:assert/strict';

import { parseInterface } from '../scripts/lib/client-interface.mjs';

const iface = `
INTERFACE z2ui5_if_client PUBLIC.

  CONSTANTS:
    BEGIN OF cs_event,
      "Control
      control_by_id TYPE string VALUE \`CONTROL_BY_ID\`,
    END OF cs_event.

  "! Registers sap.tnt&apos;s font in the Component&apos;s init; a &quot;raw&quot;
  "! call &lt;works&gt;, &amp;amp; stays the text the author escaped,
  "! and &#39;numeric&#39; &#x27;forms&#x27; are entities too.
  METHODS follow_up_action
    IMPORTING
      "! @parameter val | the action, e.g. cs_event-control_by_id
      val TYPE clike.

ENDINTERFACE.
`;

test('ABAP-Doc entities arrive in the model as the characters they stand for', () => {
  const { methods } = parseInterface(iface);
  assert.equal(methods.length, 1);
  assert.equal(
    methods[0].doc[0],
    `Registers sap.tnt's font in the Component's init; a "raw" call <works>, &amp; stays the text the author escaped, and 'numeric' 'forms' are entities too.`,
  );
});

test('a constant run keeps its label, and the doc line under it is not the label', () => {
  const { constants } = parseInterface(iface);
  assert.equal(constants[0].name, 'cs_event');
  assert.deepEqual(constants[0].members, [
    { name: 'control_by_id', type: 'string', value: 'CONTROL_BY_ID', label: 'Control' },
  ]);
});
