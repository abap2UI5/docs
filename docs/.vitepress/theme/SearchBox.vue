<script setup>
/*
 * The search in the middle of the bar — one box for the whole project.
 *
 * It replaces VitePress's own local search, which indexed the pages of this
 * site and nothing else. Half of what a reader is looking for is a SAMPLE, in
 * another repository and on another deployment, and finding it meant knowing
 * the catalogue existed and searching there instead. The index this reads
 * (scripts/lib/search-index.mjs, published as /docs/search-index.json) carries
 * both areas, and the results say which is which.
 *
 * The matching is search-engine.js, which is framework-free because the same
 * box is on the playground, the catalogue and the per-sample pages, and those
 * are static HTML. This file is the Vue half: the button in the bar, the
 * overlay, the keyboard.
 *
 * Nothing is fetched until the box is opened. The index is 400 kB (80 over the
 * wire), which is not a price to charge a reader who came to read one page.
 */
import { computed, nextTick, ref, shallowRef, watch, onMounted, onUnmounted } from 'vue';
import { search, grouped, loadIndex, highlight, rememberQuery, recallQuery } from './search-engine.js';

const open = ref(false);
const query = ref('');
const input = ref(null);
const active = ref(0);
const entries = shallowRef([]);
const failed = ref(false);

/* The index lives at an absolute URL on the shared origin, and this site is
 * also served from a dev server on localhost. Its PATH is what is used here,
 * so the box works on both without knowing which it is on - the same reason
 * the position memory resolves against `location` rather than trusting a
 * stored origin. */
const INDEX = '/docs/search-index.json';

async function fetchIndex() {
  if (entries.value.length) return;
  try {
    const index = await loadIndex(INDEX);
    entries.value = index.entries;
    failed.value = false;
  } catch {
    /* No index is a search box that says so, not one that reports "nothing
     * found" - the reader would take that for an answer about the project. */
    failed.value = true;
  }
}

function show() {
  open.value = true;
  fetchIndex();
  /* The last thing that was searched for, if a hit was opened recently
   * (search-engine.js). Selected, not merely filled in: the reader who wants
   * it presses Enter or arrows, and the reader who wants something else types
   * over it without reaching for Backspace. */
  if (!query.value) query.value = recallQuery();
  nextTick(() => {
    input.value?.focus();
    if (query.value) input.value?.select();
  });
}

function hide() {
  open.value = false;
  query.value = '';
  active.value = 0;
}

/* A high limit, and the grouping does the capping. `search` slices to thirty
 * by default, and a group's count would then be "eight of twenty-nine" for a
 * word with two hundred and thirty-one answers - a number that is worse than
 * no number. Scoring is over ~940 short entries; the cost of asking for all of
 * them is not measurable. */
const hits = computed(() => (entries.value.length ? search(entries.value, query.value, { limit: 500 }) : []));
const groups = computed(() => grouped(hits.value));
/* What answered, when it was not what was typed: a typo corrected or a word
 * set aside (search-engine.js). The list says so above the results, and the
 * marks in the rows are of the query that answered, not the one that did not. */
const relaxedTo = computed(() => hits.value.relaxedTo || '');

/* What is in the box, in the two numbers a reader recognises. Only once the
 * index has arrived - before that the box says what it is, not how much. */
const counts = computed(() => {
  if (!entries.value.length) return null;
  let docs = 0;
  for (const e of entries.value) if (e.area === 'docs') docs++;
  return { docs, samples: entries.value.length - docs };
});

/* SOMETHING TO PRESS WHEN YOU DO NOT KNOW WHAT TO ASK. An empty search box is
 * a question put to somebody who came to browse, and these are the answers
 * worth having: eight words that each open a shelf rather than a page. They
 * are checked against the real index - the smallest of them, `chart`, returns
 * sixteen hits across three of the four areas. */
const SUGGESTIONS = ['table', 'dialog', 'value help', 'upload', 'chart', 'navigation', 'binding', 'launchpad'];

/* ⌘ on an Apple keyboard and Ctrl on every other one. The key row said ⌘K to
 * everybody, which is not a shorter way of writing Ctrl - it is an instruction
 * that does not work, given to the readers who are not on a Mac. The handler
 * has always taken either (`metaKey || ctrlKey`); only the label was wrong. */
const APPLE = typeof navigator !== 'undefined'
  && /Mac|iPhone|iPad|iPod/.test(navigator.platform || navigator.userAgent || '');
/* ONE KEY CAP, NOT TWO PUSHED TOGETHER. This was `⌘`/`Ctrl` and `K` as two
 * adjacent <kbd>s with nothing between them, which renders as "CtrlK" - a key
 * nobody has. The catalogue's row has been one cap reading "Ctrl K" since it
 * was written (`hint(" from anywhere", ["/", apple ? "⌘K" : "Ctrl K"], true)`
 * in src/shell/search-box.mjs); this is that, and the " or " before it is
 * already in the markup. */
const META = APPLE ? '⌘K' : 'Ctrl K';
function suggest(word) {
  query.value = word;
  input.value?.focus();
}
/* The rows in the order the arrow keys walk them, which is the order they are
 * drawn in - grouped, not scored. */
const rows = computed(() => groups.value.flatMap((g) => g.hits));

watch(query, () => { active.value = 0; });

/** A documentation hit stays inside this deployment, so it is followed as a
 *  path: the published index names absolute URLs, and on a dev server those
 *  would send the reader to the live site. A sample hit really does leave for
 *  the catalogue, and carries the `target` that keeps VitePress's router off
 *  it (scripts/lib/cross-site.mjs). */
function hrefOf(hit) {
  const url = hit.entry.url + (hit.heading ? `#${hit.heading.anchor}` : '');
  if (hit.entry.area !== 'docs') return { href: url, target: '_self' };
  try {
    const u = new URL(url);
    return { href: u.pathname + u.search + u.hash, target: null };
  } catch {
    return { href: url, target: null };
  }
}

/* A hit was opened - by Enter, by a click, or by a middle click that opened it
 * in a tab of its own. Written down BEFORE hide(), which clears the field. */
function leave() {
  rememberQuery(query.value);
  hide();
}

function go(hit) {
  if (!hit) return;
  const { href } = hrefOf(hit);
  leave();
  location.assign(href);
}

/**
 * The arrow keys move the mark AND bring the row into view.
 *
 * They moved the mark alone, and the list did not follow: eight rows a group
 * over four groups is more than a panel holds, so walking down with the
 * keyboard marked rows nobody could see and the reader was pressing Enter on
 * something off the bottom of the box.
 *
 * `block: 'nearest'` rather than a centring scroll: it moves the list by the
 * one row that is needed and leaves it alone while the mark is already on
 * screen, which is what makes a long walk down read as a list scrolling rather
 * than as a list jumping. Only from HERE - the mouse sets `active` too, and a
 * list that scrolled under the pointer would move the row out from under it.
 */
function move(step) {
  active.value = Math.min(Math.max(active.value + step, 0), rows.value.length - 1);
  nextTick(() => {
    document.querySelector('.a2ui5-search-hit.active')?.scrollIntoView({ block: 'nearest' });
  });
}

function onKey(e) {
  if (!open.value) {
    /* Two ways in, both what a reader of technical documentation already has
     * in their fingers, and neither of them while they are typing in a field
     * of the page. */
    const typing = /^(INPUT|TEXTAREA|SELECT)$/.test(e.target?.tagName || '') || e.target?.isContentEditable;
    if (typing) return;
    if (e.key === '/' || ((e.metaKey || e.ctrlKey) && e.key === 'k')) { e.preventDefault(); show(); }
    return;
  }
  if (e.key === 'Escape') { e.preventDefault(); hide(); return; }
  if (e.key === 'ArrowDown') { e.preventDefault(); move(1); }
  else if (e.key === 'ArrowUp') { e.preventDefault(); move(-1); }
  else if (e.key === 'Enter') { e.preventDefault(); go(rows.value[active.value]); }
}

onMounted(() => document.addEventListener('keydown', onKey));
onUnmounted(() => document.removeEventListener('keydown', onKey));

const indexOf = (hit) => rows.value.indexOf(hit);
const parts = (text) => highlight(text, relaxedTo.value || query.value);
</script>

<template>
  <!-- The button is drawn as the field it opens, which is what every search in
       a documentation bar looks like now; the shortcut is printed in it so it
       is discoverable without a tooltip. -->
  <button class="a2ui5-search-button" type="button" @click="show" aria-label="Search the documentation and the samples">
    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true">
      <circle cx="11" cy="11" r="6.4" fill="none" stroke="currentColor" stroke-width="1.9"/>
      <path d="M15.8 15.8 20 20" fill="none" stroke="currentColor" stroke-width="1.9" stroke-linecap="round"/>
    </svg>
    <span class="a2ui5-search-label">Search</span>
    <kbd class="a2ui5-search-key">/</kbd>
  </button>

  <Teleport to="body">
    <div v-if="open" class="a2ui5-search-scrim" @click.self="hide">
      <div class="a2ui5-search-panel" role="dialog" aria-modal="true" aria-label="Search">
        <div class="a2ui5-search-field">
          <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true">
            <circle cx="11" cy="11" r="6.4" fill="none" stroke="currentColor" stroke-width="1.9"/>
            <path d="M15.8 15.8 20 20" fill="none" stroke="currentColor" stroke-width="1.9" stroke-linecap="round"/>
          </svg>
          <input
            ref="input"
            v-model="query"
            type="search"
            autocomplete="off"
            spellcheck="false"
            placeholder="Search the documentation and every sample"
          />
          <button class="a2ui5-search-close" type="button" @click="hide" aria-label="Close">Esc</button>
        </div>

        <div class="a2ui5-search-results">
          <p v-if="failed" class="a2ui5-search-note">
            The search index could not be loaded. The
            <a href="/docs/">documentation</a> and the
            <a href="https://abap2ui5.github.io/playground/samples/" target="_self">sample catalog</a>
            are both browsable without it.
          </p>
          <div v-else-if="!query" class="a2ui5-search-empty">
            <p class="a2ui5-search-note">
              <template v-if="counts">
                <strong>{{ counts.docs }}</strong> pages of the manual and
                <strong>{{ counts.samples }}</strong> working samples, in one box —
                search by control, by class name, or by what you are trying to do.
              </template>
              <template v-else>
                Every page of the documentation and every sample in the three catalogues.
              </template>
            </p>
            <div class="a2ui5-search-try">
              <span class="a2ui5-search-try-head">Have a look at</span>
              <button
                v-for="word in SUGGESTIONS"
                :key="word"
                class="a2ui5-search-chip"
                type="button"
                @click="suggest(word)"
              >{{ word }}</button>
            </div>
          </div>
          <p v-else-if="!rows.length" class="a2ui5-search-note">
            Nothing matches <strong>{{ query }}</strong>.
          </p>
          <p v-else-if="relaxedTo" class="a2ui5-search-note">
            Nothing matches <strong>{{ query }}</strong> — showing <strong>{{ relaxedTo }}</strong>.
          </p>

          <div v-for="group in groups" :key="group.label" class="a2ui5-search-group">
            <div class="a2ui5-search-group-head">
              {{ group.label }}
              <!-- Eight of two hundred and thirty-one is a different answer
                   from eight, and the difference is whether there is more to
                   look at. -->
              <span v-if="group.total > group.hits.length" class="a2ui5-search-count">
                &nbsp;{{ group.hits.length }} of {{ group.total }}
              </span>
              <span v-else class="a2ui5-search-count">&nbsp;{{ group.total }}</span>
            </div>
            <a
              v-for="hit in group.hits"
              :key="hit.entry.url"
              class="a2ui5-search-hit"
              :class="{ active: indexOf(hit) === active }"
              :href="hrefOf(hit).href"
              :target="hrefOf(hit).target"
              @mouseenter="active = indexOf(hit)"
              @click="leave"
            >
              <span class="a2ui5-search-hit-title">
                <span v-for="([text, on], i) in parts(hit.entry.title)" :key="i" :class="{ hl: on }">{{ text }}</span>
              </span>
              <span v-if="hit.heading" class="a2ui5-search-hit-where">›&nbsp;{{ hit.heading.text }}</span>
              <span v-if="hit.entry.code" class="a2ui5-search-hit-code">
                <span v-for="([text, on], i) in parts(hit.entry.code)" :key="i" :class="{ hl: on }">{{ text }}</span>
              </span>
              <span v-if="hit.entry.text" class="a2ui5-search-hit-text">
                <span v-for="([text, on], i) in parts(hit.entry.text)" :key="i" :class="{ hl: on }">{{ text }}</span>
              </span>
            </a>
          </div>
        </div>

        <!-- THE KEYS, ALWAYS. They used to be in the line the empty state
             showed, which is the one moment a reader is not using them: the
             first keystroke replaced that line with results and took the only
             mention of the arrows and Enter with it. And the shortcut that
             opens the box was printed on the button and nowhere inside, so
             once you were in you were told nothing at all. -->
        <div class="a2ui5-search-keys">
          <span><kbd>↑</kbd><kbd>↓</kbd> to move</span>
          <span><kbd>↵</kbd> to open</span>
          <span><kbd>esc</kbd> to close</span>
          <span class="a2ui5-search-keys-end"><kbd>/</kbd> or <kbd>{{ META }}</kbd> from anywhere</span>
        </div>
      </div>
    </div>
  </Teleport>
</template>
