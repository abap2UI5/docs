/*
 * The bookkeeping half of the Run button: every complete app class on a page
 * either has a button from the rule engine, or carries a marker saying why it
 * cannot run — and a marker that stops being true is refused as loudly as a
 * missing one. One fixture per verdict `auditPage` can reach, so the gate in
 * scripts/check-playground.mjs cannot drift into passing what it exists to
 * catch.
 *
 *   node --test test/
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { auditPage } from '../scripts/lib/playground-coverage.mjs';

/** A complete app class, with `body` as the whole of `main`. */
const app = (name, body) => `CLASS ${name} DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS ${name} IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
${body}
  ENDMETHOD.
ENDCLASS.`;

const RUNS = app('zcl_app_hello', '    client->message_box_display( `Hello` ).');
const REFUSED = app('zcl_app_sql', `    SELECT FROM sflight FIELDS * INTO TABLE @DATA(rows).
    client->message_box_display( \`Hello\` ).`);
const MARKER = '<!-- playground: no Run button — SELECTs from SFLIGHT, which no browser database has -->';

const fence = (code) => '```abap\n' + code + '\n```';

test('an example with a button needs no marker', () => {
  const r = auditPage(`# Page\n\n${fence(RUNS)}\n`);
  assert.deepEqual(
    { apps: r.apps, buttons: r.buttons, undecided: r.undecided, stale: r.stale },
    { apps: 1, buttons: 1, undecided: [], stale: [] },
  );
});

test('a refused example with a marker is excluded on purpose, both reasons kept', () => {
  const r = auditPage(`# Page\n\n${MARKER}\n${fence(REFUSED)}\n`);
  assert.equal(r.undecided.length, 0);
  assert.equal(r.stale.length, 0);
  assert.equal(r.excluded.length, 1);
  assert.equal(r.excluded[0].name, 'zcl_app_sql');
  assert.match(r.excluded[0].why, /sflight/);
  assert.match(r.excluded[0].reason, /SFLIGHT/);
});

test('a blank line between marker and fence does not detach it', () => {
  const r = auditPage(`# Page\n\n${MARKER}\n\n${fence(REFUSED)}\n`);
  assert.equal(r.excluded.length, 1);
});

test('a refused example without a marker is the gap this gate exists for', () => {
  const r = auditPage(`# Page\n\n${fence(REFUSED)}\n`);
  assert.equal(r.undecided.length, 1);
  assert.equal(r.undecided[0].name, 'zcl_app_sql');
  assert.match(r.undecided[0].why, /sflight/);
});

test('a marker above an example that HAS a button is stale', () => {
  // The example was fixed, the marker stayed. Left standing, it would let the
  // next edit take the button away again without anybody deciding that.
  const r = auditPage(`# Page\n\n${MARKER}\n${fence(RUNS)}\n`);
  assert.equal(r.buttons, 0, 'the contradiction is reported, not counted over');
  assert.equal(r.stale.length, 1);
  assert.match(r.stale[0], /HAS a Run button/);
});

test('a marker attached to nothing is stale', () => {
  const r = auditPage(`# Page\n\n${MARKER}\n\nSome prose instead of a fence.\n`);
  assert.equal(r.stale.length, 1);
  assert.match(r.stale[0], /attached to no ABAP fence/);
});

test('a marker above a class that is not an app is stale', () => {
  // The engine never offers a helper class a button; a marker there records an
  // intent about a decision nobody makes.
  const helper = `CLASS zcl_helper DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS do.
ENDCLASS.
CLASS zcl_helper IMPLEMENTATION.
  METHOD do.
  ENDMETHOD.
ENDCLASS.`;
  const r = auditPage(`# Page\n\n${MARKER}\n${fence(helper)}\n`);
  assert.equal(r.stale.length, 1);
  assert.match(r.stale[0], /not an app/);
});

test('a fragment never needs a marker, and a marker on one is stale', () => {
  const fragment = 'view->ele( `Page` )->a( n = `title` v = `x` ).';
  assert.equal(auditPage(`# Page\n\n${fence(fragment)}\n`).undecided.length, 0);
  const r = auditPage(`# Page\n\n${MARKER}\n${fence(fragment)}\n`);
  assert.equal(r.stale.length, 1);
});

test('two identical markers are two markers - an orphan does not hide behind a used one', () => {
  const r = auditPage(`# Page\n\n${MARKER}\n${fence(REFUSED)}\n\n${MARKER}\n\nprose\n`);
  assert.equal(r.excluded.length, 1);
  assert.equal(r.stale.length, 1);
});
