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
import { search, grouped, loadIndex, highlight } from './search-engine.js';

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
  nextTick(() => input.value?.focus());
}

function hide() {
  open.value = false;
  query.value = '';
  active.value = 0;
}

const hits = computed(() => (entries.value.length ? search(entries.value, query.value) : []));
const groups = computed(() => grouped(hits.value));
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

function go(hit) {
  if (!hit) return;
  const { href } = hrefOf(hit);
  hide();
  location.assign(href);
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
  if (e.key === 'ArrowDown') { e.preventDefault(); active.value = Math.min(active.value + 1, rows.value.length - 1); }
  else if (e.key === 'ArrowUp') { e.preventDefault(); active.value = Math.max(active.value - 1, 0); }
  else if (e.key === 'Enter') { e.preventDefault(); go(rows.value[active.value]); }
}

onMounted(() => document.addEventListener('keydown', onKey));
onUnmounted(() => document.removeEventListener('keydown', onKey));

const indexOf = (hit) => rows.value.indexOf(hit);
const parts = (text) => highlight(text, query.value);
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
            <a href="https://abap2ui5.github.io/playground/samples/" target="_self">sample catalogue</a>
            are both browsable without it.
          </p>
          <p v-else-if="!query" class="a2ui5-search-note">
            Every page of the documentation and every sample in the three catalogues.
            <kbd>↑</kbd><kbd>↓</kbd> to move, <kbd>↵</kbd> to open.
          </p>
          <p v-else-if="!rows.length" class="a2ui5-search-note">
            Nothing matches <strong>{{ query }}</strong>.
          </p>

          <div v-for="group in groups" :key="group.label" class="a2ui5-search-group">
            <div class="a2ui5-search-group-head">{{ group.label }}</div>
            <a
              v-for="hit in group.hits"
              :key="hit.entry.url"
              class="a2ui5-search-hit"
              :class="{ active: indexOf(hit) === active }"
              :href="hrefOf(hit).href"
              :target="hrefOf(hit).target"
              @mouseenter="active = indexOf(hit)"
              @click="hide"
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
      </div>
    </div>
  </Teleport>
</template>
