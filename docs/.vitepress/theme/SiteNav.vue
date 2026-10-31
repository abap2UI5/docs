<script setup>
/*
 * The family's top line (CONVENTION.md in abap2UI5/abap2UI5.github.io): the
 * five sites of abap2UI5 as tabs, this one - Docs - current on every page.
 * The other sites are reached by plain links; nothing is shared at build
 * time or in the browser's storage.
 *
 * This component is used twice: in the bar (TheBar.vue) and in the menu a
 * phone opens instead of it.
 */

/* EVERY LINK INTO A NEIGHBOURING SITE ON THIS ORIGIN CARRIES A `target`.
 * This site is a single page application: VitePress's router takes over any
 * same-origin link that looks like a page, and /playground/ is both - without
 * the target it drew this site's 404 at the playground's URL.
 * scripts/lib/cross-site.mjs is the whole story, and
 * scripts/check-cross-site.mjs holds the built site to it. */
const SAME_TAB = "_self";

const MANAGERS = "https://abap2ui5.github.io/why/";
const DOCS = "/docs/";
const SAMPLES = "https://abap2ui5.github.io/samples-site/";
const PLAYGROUND = "https://abap2ui5.github.io/playground/";
const ADDONS = "https://abap2ui5-addons.github.io/";
</script>

<template>
  <nav class="bar-nav" aria-label="abap2UI5 sites">
    <a :href="MANAGERS" :target="SAME_TAB" data-site="managers" title="abap2UI5 for the person who approves it"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round"><rect x="3.4" y="7.6" width="17.2" height="11.6" rx="1.8"/><path d="M9 7.6V6a1.4 1.4 0 0 1 1.4-1.4h3.2A1.4 1.4 0 0 1 15 6v1.6M3.4 12.4h17.2"/></svg><span data-text="For managers">For managers</span></a>
    <a :href="DOCS" data-site="docs" class="here" aria-current="page" title="The manual"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round"><path d="M12 7.2C10.5 5.9 8.5 5.2 6 5.2H3.3v11.9H6c2.5 0 4.5.7 6 1.9 1.5-1.2 3.5-1.9 6-1.9h2.7V5.2H18c-2.5 0-4.5.7-6 1.9z"/><path d="M12 7.2v11.8"/></svg><span data-text="Docs">Docs</span></a>
    <a :href="SAMPLES" :target="SAME_TAB" data-site="samples" title="Every abap2UI5 sample, searchable"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round"><rect x="3.2" y="4.8" width="17.6" height="14.4" rx="2"/><path d="M3.2 9.4h17.6M8.5 9.4v9.8"/></svg><span data-text="Samples">Samples</span></a>
    <a :href="PLAYGROUND" data-site="playground" :target="SAME_TAB" title="Write ABAP and run it in the browser"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round"><circle cx="12" cy="12" r="8.6"/><path d="M10.2 8.4v7.2a.5.5 0 0 0 .76.43l5.8-3.6a.5.5 0 0 0 0-.86l-5.8-3.6a.5.5 0 0 0-.76.43z" fill="currentColor" stroke="none"/></svg><span data-text="Playground">Playground</span></a>
    <a :href="ADDONS" data-site="addons" title="Addons for your system, tools for your editor and pipeline"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round"><rect x="3.6" y="3.6" width="7" height="7" rx="1.3"/><rect x="13.4" y="3.6" width="7" height="7" rx="1.3"/><rect x="3.6" y="13.4" width="7" height="7" rx="1.3"/><rect x="13.4" y="13.4" width="7" height="7" rx="1.3"/></svg><span data-text="Addons &amp; Tools">Addons &amp; Tools</span></a>
  </nav>
</template>
