/* The worklist after a release (scripts/report-changes.mjs) is only as good as
 * its two readers: the changelog as releases and entries, and the names an
 * entry carries. Both are pinned on lines the framework's changelog really
 * has, because a reader that quietly finds nothing prints a short, reassuring
 * report. */
import assert from 'node:assert/strict';
import test from 'node:test';
import { linesNaming, namesIn, parseChangelog } from '../scripts/lib/changes.mjs';

const CHANGELOG = `abap2UI5 changelog
==================

Legend
------
* : fixed
! : changed

unreleased
----------
! ABAP Cloud: the branch cloud carries the whole repository
* 702: the test include of z2ui5_cl_ui5_srv_model activates on 7.02

2026-11-25 v1.151.0
-------------------
+ Timers: timer_set( ) / timer_stop( ) declare a timer
! BREAKING: hash_set( ) / hash_replace( ) write only for an app with a hash listener
- BREAKING: z2ui5_if_exit (use z2ui5_if_ui5_exit), _bind_edit( ) (use _bind( ))


2026-11-20 v1.150.0
-------------------
! Drafts: a row with a blank owner is no longer read
`;

test('the changelog: releases in file order, the legend is not one', () => {
  const releases = parseChangelog(CHANGELOG);
  assert.deepEqual(releases.map((r) => [r.version, r.date, r.entries.length]), [
    ['unreleased', null, 2],
    ['1.151.0', '2026-11-25', 3],
    ['1.150.0', '2026-11-20', 1],
  ]);
  assert.deepEqual(releases[1].entries.map((e) => e.kind), ['added', 'changed', 'removed']);
});

test('the names of an entry: underscored tokens, both halves of a component, bare calls', () => {
  assert.deepEqual(
    namesIn('BREAKING: hash_set( ) / hash_replace( ) write only for an app with a hash listener'),
    ['hash_set', 'hash_replace'],
  );
  assert.deepEqual(
    namesIn('Launchpad: cs_event-set_title titles the shell, get_start_param( ) reads it; START_TIMER repeats'),
    ['cs_event', 'set_title', 'get_start_param', 'start_timer'],
  );
  assert.deepEqual(
    namesIn('an own draft store implements prune_chain( ) and touch( ), exit field draft_keep_per_chain'),
    ['prune_chain', 'touch', 'draft_keep_per_chain'],
  );
  assert.deepEqual(namesIn('_bind_edit( ) (use _bind( ))'), ['_bind_edit', '_bind']);
});

test('a plain word is never a name', () => {
  assert.deepEqual(namesIn('abap2UI5-obsolet is abap2UI5-ext again (use it)'), []);
  assert.deepEqual(namesIn('Drafts: smaller and pruned per chain, timers repeat'), []);
});

test('a name is found whole, in any case', () => {
  const page = 'Use `cs_event-set_title`.\nNot `set_title_launchpad`.\nSET_TITLE again.\nuntouched';
  assert.deepEqual(linesNaming(page, 'set_title'), [1, 3]);
  assert.deepEqual(linesNaming(page, 'touch'), []);
});
