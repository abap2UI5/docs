/*
 * The link a reader copies for the words they selected — specifically the half
 * that decides WHAT to quote and how to spell it (theme/text-fragment.js).
 *
 * Why this is a unit test and not a browser one: what can go wrong here is not
 * visible on a page. A directive that quotes two words the page carries three
 * times looks exactly like one that quotes two words it carries once — both
 * highlight something, and only one of them highlights the right thing. The
 * same goes for the escaping: a hyphen left unescaped inside the quote is a
 * valid directive that means something else entirely (everything before it
 * becomes a prefix), so the link works, silently, on the wrong words.
 *
 * The browser half (theme/link-to-selection.js) is a button, a rectangle and
 * the clipboard, and is left to the browser.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';

const { fragmentFor, normalise, parseDirective } = await import('../docs/.vitepress/theme/text-fragment.js');

test('a short selection is quoted whole', () => {
  assert.equal(fragmentFor({ text: 'the app class', pageText: 'a page with the app class in it' }),
    'text=the%20app%20class');
});

test('whitespace is collapsed, because that is how the fragment is matched', () => {
  // A chain selected out of a fence arrives with its indentation and its line
  // breaks; the directive has to spell it the way the browser compares it.
  assert.equal(normalise('  page->input(\n     value = lv_x )  '), 'page->input( value = lv_x )');
  assert.equal(fragmentFor({ text: '  two   words  ' }), 'text=two%20words');
});

test('nothing to quote is no link at all', () => {
  assert.equal(fragmentFor({ text: '   \n  ' }), null);
  assert.equal(fragmentFor({}), null);
});

test('a long selection is quoted as its two ends', () => {
  // Twenty words in a URL is a URL nobody pastes into a chat window, so a
  // selection past the limit is quoted as where it starts and where it stops.
  const text = 'one two three four five six seven eight nine ten eleven twelve thirteen fourteen';
  assert.equal(fragmentFor({ text }), 'text=one%20two%20three%20four%20five,ten%20eleven%20twelve%20thirteen%20fourteen');
});

test('a selection that crosses lines is quoted as two ends however short it is', () => {
  // The reason it must be: an exact quote has to be found inside ONE block,
  // and every line of a highlighted code fence is an element of its own. Five
  // words over two lines are quoted as two words and two words - the halves
  // may not overlap, because textEnd is looked for after textStart, so the one
  // in the middle is spanned rather than spelled.
  assert.equal(fragmentFor({ text: 'METHOD z2ui5_if_app~main.\n  DATA lv_x TYPE' }),
    'text=METHOD%20z2ui5_if_app~main.,lv_x%20TYPE');
});

test('a quote the page repeats is given the words in front of it', () => {
  const page = 'the app calls client->view_display twice; the app calls client->view_display again';
  assert.equal(
    fragmentFor({ text: 'client->view_display', prefix: 'somewhere else the reader will', pageText: page }),
    'text=somewhere%20else%20the%20reader%20will-,client%2D%3Eview_display',
  );
});

test('...and the words after it when the page repeats those too', () => {
  const page = 'the app calls client->view_display twice; the app calls client->view_display again';
  assert.equal(
    fragmentFor({ text: 'client->view_display', prefix: 'the app calls', suffix: 'twice;', pageText: page }),
    'text=the%20app%20calls-,client%2D%3Eview_display,-twice%3B',
  );
});

test('a quote the page carries once is left alone', () => {
  // Context is not free - it is length in a URL and one more thing that can go
  // stale - so it is only added when the quote is actually ambiguous.
  const page = 'the app calls client->view_display once';
  assert.equal(fragmentFor({ text: 'client->view_display', prefix: 'the app calls', pageText: page }),
    'text=client%2D%3Eview_display');
});

test('a hyphen inside the quote is escaped, because a hyphen is punctuation here', () => {
  // "prefix-,start" and "end,-suffix": an unescaped hyphen in the words does
  // not break the link, it silently makes it mean something else.
  assert.equal(fragmentFor({ text: 'z2ui5_cl_ui5_view_builder=>factory( )' }),
    'text=z2ui5_cl_ui5_view_builder%3D%3Efactory(%20)');
  assert.equal(fragmentFor({ text: 'client->nav_app_leave( )' }), 'text=client%2D%3Enav_app_leave(%20)');
  assert.equal(fragmentFor({ text: 'one, two & three' }), 'text=one%2C%20two%20%26%20three');
});

test('a directive reads back as the words it was written from', () => {
  // The fallback for a browser too old for `:~:` has to find the words again.
  const written = fragmentFor({
    text: 'client->view_display',
    prefix: 'the app calls',
    suffix: 'twice;',
    pageText: 'the app calls client->view_display twice; the app calls client->view_display again',
  });
  assert.deepEqual(parseDirective(`https://abap2ui5.github.io/docs/x.html#:~:${written}`), {
    prefix: 'the app calls',
    start: 'client->view_display',
    end: '',
    suffix: 'twice;',
  });
});

test('a two-ended directive reads back as both ends', () => {
  assert.deepEqual(parseDirective('/docs/x.html#:~:text=CLASS%20zcl_x,ENDCLASS.'),
    { prefix: '', start: 'CLASS zcl_x', end: 'ENDCLASS.', suffix: '' });
});

test('a URL with no directive in it is not one', () => {
  assert.equal(parseDirective('https://abap2ui5.github.io/docs/x.html'), null);
  assert.equal(parseDirective('https://abap2ui5.github.io/docs/x.html#a-heading'), null);
  assert.equal(parseDirective(undefined), null);
  // Somebody else's fragment directive, for something that is not text.
  assert.equal(parseDirective('/docs/x.html#:~:unknown=1'), null);
});
