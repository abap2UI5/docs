<script setup>
/*
 * The sections of the project: Home and Documentation are pages of this
 * site, Samples and Playground are other sites, reached by plain links. The
 * other abap2UI5 sites carry the same top line by convention (see
 * CONVENTION.md in abap2UI5/abap2UI5.github.io); nothing is shared at build
 * time or in the browser's storage.
 *
 * This component is used twice: in the bar (TheBar.vue) and in the menu a
 * phone opens instead of it.
 */
import { computed } from "vue";
import { useData } from "vitepress";

const { page } = useData();

/* EVERY LINK INTO A NEIGHBOURING SITE ON THIS ORIGIN CARRIES A `target`.
 * This site is a single page application: VitePress's router takes over any
 * same-origin link that looks like a page, and /playground/ is both - without
 * the target it drew this site's 404 at the playground's URL.
 * scripts/lib/cross-site.mjs is the whole story, and
 * scripts/check-cross-site.mjs holds the built site to it. */
const SAME_TAB = "_self";

const PLAYGROUND = "https://abap2ui5.github.io/playground/";
const SAMPLES = "https://abap2ui5.github.io/playground/samples/";
const HOME = "/docs/";
const DOCS = "/docs/get_started/about";

const onHome = computed(() => page.value.relativePath === "index.md");
</script>

<template>
  <nav class="bar-nav" aria-label="abap2UI5 sites">
    <!-- The four sections of the project, left to right in the order a reader
         meets them, and the one you are on marked. The tools - the AI Studio,
         the Layout Editor, the Report converter - stand on their own and are
         in the menu (SiteMenu.vue), not here. Home and Documentation are
         two sections of THIS deployment now - the section that used to be one
         non-clickable word ("Documentation") is two places a reader can move
         between, which is what the brand alone could not offer. -->
    <a :href="HOME" :class="{ here: onHome }" :aria-current="onHome ? 'page' : undefined" title="abap2UI5 in one page"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round"><path d="M3.6 10.9 12 4.2l8.4 6.7v8.3a1 1 0 0 1-1 1h-4.3v-6.1H8.9v6.1H4.6a1 1 0 0 1-1-1z"/></svg><span data-text="Home">Home</span></a>
    <a :href="DOCS" data-site="docs" :class="{ here: !onHome }" :aria-current="!onHome ? 'page' : undefined" title="The manual"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round"><path d="M12 7.2C10.5 5.9 8.5 5.2 6 5.2H3.3v11.9H6c2.5 0 4.5.7 6 1.9 1.5-1.2 3.5-1.9 6-1.9h2.7V5.2H18c-2.5 0-4.5.7-6 1.9z"/><path d="M12 7.2v11.8"/></svg><span data-text="Documentation">Documentation</span></a>
    <a :href="SAMPLES" :target="SAME_TAB" data-site="samples" title="Every abap2UI5 sample, searchable"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round"><rect x="3.2" y="4.8" width="17.6" height="14.4" rx="2"/><path d="M3.2 9.4h17.6M8.5 9.4v9.8"/></svg><span data-text="Samples">Samples</span></a>
    <a :href="PLAYGROUND" data-site="playground" :target="SAME_TAB" title="Write ABAP and run it in the browser"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round"><circle cx="12" cy="12" r="8.6"/><path d="M10.2 8.4v7.2a.5.5 0 0 0 .76.43l5.8-3.6a.5.5 0 0 0 0-.86l-5.8-3.6a.5.5 0 0 0-.76.43z" fill="currentColor" stroke="none"/></svg><span data-text="Playground">Playground</span></a>
  </nav>
</template>
