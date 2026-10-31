/*
 * The lifecycle rule of check-conventions.mjs: a runnable app example
 * displays its view under check_on_init / check_on_navigated, or its page
 * says why not. One fixture per way the scanner could be fooled - the
 * CASE abap_true shape, a nested IF, an ELSE, a negated condition, a display
 * in a comment - and the marker bookkeeping.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { unconditionalDisplays, auditLifecycle } from '../scripts/lib/lifecycle.mjs';

/** A complete, runnable app class with `body` as the whole of main. */
const app = (name, body) => `CLASS ${name} DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA value TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ${name} IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
${body}
  ENDMETHOD.
ENDCLASS.`;

const VIEW = 'DATA(view) = z2ui5_cl_ui5_view_builder=>factory( )->ele( `Page` )->stringify( ).';

const PASSING = app('zcl_sample_ok', `    IF client->check_on_init( ).
      ${VIEW}
      client->view_display( view ).
    ELSEIF client->check_on_event( \`POST\` ).
      client->message_toast_display( value ).
    ENDIF.`);

const CASE_SHAPE = app('zcl_sample_case', `    CASE abap_true.
      WHEN client->check_on_init( ).
        ${VIEW}
        client->view_display( view ).
      WHEN client->check_on_navigated( ).
        client->view_display( view ).
      WHEN client->check_on_event( \`POST\` ).
        client->message_toast_display( value ).
    ENDCASE.`);

const NESTED = app('zcl_sample_nested', `    IF client->check_on_init( ).
      ${VIEW}
      IF value IS INITIAL.
        client->view_display( view ).
      ENDIF.
    ENDIF.`);

const FAILING = app('zcl_sample_every', `    IF client->check_on_init( ).
      value = \`x\`.
    ENDIF.
    " client->view_display( ) in a comment is not a display
    ${VIEW}
    client->view_display( view ).`);

const IN_ELSE = app('zcl_sample_else', `    IF client->check_on_init( ).
      value = \`x\`.
    ELSE.
      ${VIEW}
      client->view_display( view ).
    ENDIF.`);

const NEGATED = app('zcl_sample_not', `    IF client->check_on_init( ) = abap_false.
      RETURN.
    ENDIF.
    ${VIEW}
    client->view_display( view ).`);

const fence = (code) => '```abap\n' + code + '\n```';
const MARKER = '<!-- lifecycle: allow unconditional display — the page is about the roundtrip itself -->';

test('a display under check_on_init, in an IF or a WHEN, nested or not, is in a lifecycle branch', () => {
  for (const code of [PASSING, CASE_SHAPE, NESTED]) assert.deepEqual(unconditionalDisplays(code), []);
});

test('a display after the branch, in an ELSE, or behind a negated check is unconditional', () => {
  const texts = (code) => unconditionalDisplays(code).map((f) => f.text);
  assert.deepEqual(texts(FAILING), ['client->view_display( view ).']);
  assert.deepEqual(texts(IN_ELSE), ['client->view_display( view ).']);
  assert.deepEqual(texts(NEGATED), ['client->view_display( view ).']);
});

test('a class without main( ) is not judged', () => {
  assert.deepEqual(unconditionalDisplays('CLASS x DEFINITION PUBLIC.\nENDCLASS.\n'), []);
});

test('a page: the finding names the class and the file line of the statement', () => {
  const md = `# Page\n\nintro\n\n${fence(FAILING)}\n`;
  const r = auditLifecycle(md);
  assert.equal(r.apps, 1);
  assert.equal(r.findings.length, 1);
  assert.equal(r.findings[0].name, 'zcl_sample_every');
  assert.equal(md.split('\n')[r.findings[0].line - 1].trim(), 'client->view_display( view ).');
  assert.deepEqual(r.stale, []);
});

test('a marker above the fence allows it, and counts it', () => {
  const r = auditLifecycle(`# Page\n\n${MARKER}\n${fence(FAILING)}\n`);
  assert.deepEqual({ apps: r.apps, allowed: r.allowed, findings: r.findings, stale: r.stale }, { apps: 1, allowed: 1, findings: [], stale: [] });
});

test('a marker above a passing example, or above nothing, is stale', () => {
  const r = auditLifecycle(`# Page\n\n${MARKER}\n${fence(PASSING)}\n\n${MARKER}\n\nprose\n`);
  assert.equal(r.stale.length, 2);
  assert.match(r.stale[0], /one of the two is wrong/);
  assert.match(r.stale[1], /attached to no ABAP fence/);
});

test('an example the playground would refuse is not judged', () => {
  const refused = app('zcl_sample_sql', `    SELECT FROM sflight FIELDS * INTO TABLE @DATA(rows).\n    ${VIEW}\n    client->view_display( view ).`);
  const r = auditLifecycle(`# Page\n\n${fence(refused)}\n`);
  assert.deepEqual({ apps: r.apps, findings: r.findings }, { apps: 0, findings: [] });
});
