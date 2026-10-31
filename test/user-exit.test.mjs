/*
 * The user-exit block of advanced/extensibility/user_exits.md is generated
 * from two sources: the interface for the shape, the shipped exit class for
 * the defaults it seeds. This pins the parts that could silently thin out -
 * a default read as "initial" because the assignment moved under an IF, a
 * comment run attached to the wrong field, a REPLACE statement in a note
 * printed as prose.
 *
 *   npm test
 */
import test from 'node:test';
import assert from 'node:assert/strict';

import { parseInterface } from '../scripts/lib/client-interface.mjs';
import { parseExitDefaults, renderExitBlock, noteParagraph } from '../scripts/lib/user-exit.mjs';

const iface = `
"! The framework's extension point.
"!
"! Supersedes an older name, which stays out of the page.
INTERFACE z2ui5_if_ui5_exit
  PUBLIC.

  TYPES:
    "! Which request the exit answers for.
    BEGIN OF ty_s_http_context,
      path      TYPE string,
      app_start TYPE string,
    END OF ty_s_http_context.

  TYPES:
    "! The page.
    BEGIN OF ty_s_http_config,
      src                     TYPE string,
      " the CSP tag, pre-filled. An installation switches eval on here -
      "   REPLACE \`script-src 'self'\` IN cs_config-content_security_policy
      "           WITH \`script-src 'self' 'unsafe-eval'\`.
      " - or replaces the tag. The header is client-
      " suppliable, though.
      content_security_policy TYPE string,
      t_security_header       TYPE z2ui5_if_client=>ty_t_name_value,
    END OF ty_s_http_config.

  TYPES:
    BEGIN OF ty_s_http_config_post,
      draft_exp_time_in_hours TYPE i,
      " off by default
      check_hide_error_details TYPE abap_bool,
      check_csrf_active        TYPE abap_bool,
    END OF ty_s_http_config_post.

  METHODS set_config_http_get
    IMPORTING
      is_context TYPE ty_s_http_context OPTIONAL
    CHANGING
      cs_config  TYPE ty_s_http_config.

  METHODS set_config_http_post
    IMPORTING
      is_context TYPE ty_s_http_context OPTIONAL
    CHANGING
      cs_config  TYPE ty_s_http_config_post.

ENDINTERFACE.
`;

const clas = `
CLASS z2ui5_cl_ui5_user_exit DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_ui5_exit.
ENDCLASS.

CLASS z2ui5_cl_ui5_user_exit IMPLEMENTATION.

  METHOD z2ui5_if_ui5_exit~set_config_http_get.

    cs_config-src = \`https://sdk.openui5.org/resources/sap-ui-core.js\`.
    " assembled from a template, so the page can only name it
    cs_config-content_security_policy = gv_csp_default.
    cs_config-t_security_header = VALUE #(
        ( n = \`X-Frame-Options\` v = \`SAMEORIGIN\` )
        " a comment line inside the table
        ( n = \`Referrer-Policy\` v = \`strict-origin-when-cross-origin\` ) ).

  ENDMETHOD.

  METHOD z2ui5_if_ui5_exit~set_config_http_post.

    CONSTANTS lc_default_exp_time_in_hours TYPE i VALUE 4.

    cs_config-draft_exp_time_in_hours = lc_default_exp_time_in_hours.
    cs_config-check_csrf_active = abap_true.

    IF cs_config-draft_exp_time_in_hours <= 0.
      cs_config-draft_exp_time_in_hours = 99.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
`;

test('the interface header is the model\'s doc, not the first type\'s', () => {
  const model = parseInterface(iface, { memberNotes: true });
  assert.equal(model.name, 'z2ui5_if_ui5_exit');
  assert.deepEqual(model.doc, ["The framework's extension point.", 'Supersedes an older name, which stays out of the page.']);
  assert.deepEqual(model.types[0].doc, ['Which request the exit answers for.']);
});

test('a comment run is the note of the field right after it, and no other', () => {
  const { types } = parseInterface(iface, { memberNotes: true });
  const [src, csp, headers] = types[1].members;
  assert.equal(src.note, undefined);
  assert.equal(csp.note.length, 5);
  assert.equal(headers.note, undefined);
  const [, hide, csrf] = types[2].members;
  assert.deepEqual(hide.note, ['off by default']);
  assert.equal(csrf.note, undefined);
});

test('without the option the member model keeps its old shape', () => {
  const { types } = parseInterface(iface);
  assert.deepEqual(types[1].members[1], {
    name: 'content_security_policy', type: 'string',
    label: 'the CSP tag, pre-filled. An installation switches eval on here -',
  });
});

test('the defaults come out of the shipped exit: literal, constant, flag, table, expression', () => {
  const d = parseExitDefaults(clas);
  const get = d.get('set_config_http_get');
  const post = d.get('set_config_http_post');
  assert.deepEqual(get.get('src'), { kind: 'literal', value: 'https://sdk.openui5.org/resources/sap-ui-core.js' });
  assert.deepEqual(get.get('content_security_policy'), { kind: 'expression', text: 'gv_csp_default' });
  assert.deepEqual(get.get('t_security_header'), {
    kind: 'table',
    rows: [{ n: 'X-Frame-Options', v: 'SAMEORIGIN' }, { n: 'Referrer-Policy', v: 'strict-origin-when-cross-origin' }],
  });
  assert.deepEqual(post.get('draft_exp_time_in_hours'), { kind: 'literal', value: '4' });
  assert.deepEqual(post.get('check_csrf_active'), { kind: 'bool', value: 'abap_true' });
  /* the clamp inside the IF is not the default */
  assert.equal(post.has('check_hide_error_details'), false);
});

test('a note prints its ABAP as one code span and joins a hyphenated word back up', () => {
  const { types } = parseInterface(iface, { memberNotes: true });
  const text = noteParagraph(types[1].members[1].note);
  assert.equal(
    text,
    "the CSP tag, pre-filled. An installation switches eval on here - `` REPLACE `script-src 'self'` IN cs_config-content_security_policy WITH `script-src 'self' 'unsafe-eval'`. `` - or replaces the tag. The header is client-suppliable, though.",
  );
});

test('the block renders every field with its default, and an unassigned flag as abap_false', () => {
  const model = parseInterface(iface, { memberNotes: true });
  const block = renderExitBlock(model, parseExitDefaults(clas), {
    ref: 'main', interfacePath: 'src/02/x.intf.abap', classPath: 'src/01/04/y.clas.abap',
    start: '<!-- api:start -->', end: '<!-- api:end -->',
  });
  assert.ok(block.startsWith('<!-- api:start -->\n'));
  assert.ok(block.endsWith('\n<!-- api:end -->'));
  assert.match(block, /^\| `is_context` \| `ty_s_http_context` \| importing, optional \|$/m);
  assert.match(block, /^\| `cs_config` \| `ty_s_http_config_post` \| changing \|$/m);
  assert.match(block, /^\| `check_hide_error_details` \| `abap_bool` \| `abap_false` \|$/m);
  assert.match(block, /^\| `check_csrf_active` \| `abap_bool` \| `abap_true` \|$/m);
  assert.match(block, /^\| `draft_exp_time_in_hours` \| `i` \| `4` \|$/m);
  assert.match(block, /^\| `t_security_header` \| .* \| `X-Frame-Options: SAMEORIGIN`, `Referrer-Policy: strict-origin-when-cross-origin` \|$/m);
  assert.match(block, /^\| `content_security_policy` \| `string` \| built by \[the shipped exit\]\(https:\/\/github\.com\/abap2UI5\/abap2UI5\/blob\/main\/src\/01\/04\/y\.clas\.abap\) \(`gv_csp_default`\) \|$/m);
  /* the context has no defaults column: nothing seeds it */
  assert.match(block, /^\| `app_start` \| `string` \|$/m);
  /* the header doc stays out: the one line that names a superseded interface */
  assert.ok(!block.includes('Supersedes'));
});
