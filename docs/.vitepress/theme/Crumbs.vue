<script setup>
/*
 * The line above the title that says where in the manual you are:
 *
 *     Documentation › Cookbook › Model
 *
 * The sample catalogue has had this on every one of its pages since they were
 * generated (`.crumbs` in tools/sample-pages.mjs, abap2UI5/playground), and a
 * reader crossing between the two documents met it on one side only. Same
 * shape, same separator, same size, same place on the page — see *where in the
 * manual* in style.css for the numbers and where they came from.
 *
 * The trail names the SECTIONS the page stands in and stops short of the page
 * itself, because the page's own name is the heading directly underneath: a
 * catalogue page reads "Sample catalogue › Learn › Basics" above an h1 that is
 * the sample's title, and repeating it in the line above would say it twice.
 *
 * WHERE THE TRAIL COMES FROM. The sidebar in config.mjs, walked against the
 * page being rendered — not a second list to keep in step with it. The bar
 * used to carry a Guide dropdown that was exactly such a copy and it drifted
 * twice; the comment above `nav: []` in config.mjs is that scar. Whatever the
 * sidebar says a page's ancestors are is what this line says they are, and a
 * section renamed there is renamed here in the same commit. The walk itself is
 * `crumbs.js`, so that it can be tested.
 */
import { computed } from 'vue'
import { useData, withBase } from 'vitepress'
import { trailFor } from './crumbs.js'

const { page, theme } = useData()

/* `relativePath` rather than the route, because it is what the SERVER knows as
 * well: the trail is then in the HTML a crawler and a first paint are given,
 * and not something that appears a tick after hydration. */
const trail = computed(() => trailFor(theme.value.sidebar, page.value.relativePath))
</script>

<template>
  <!-- The separator is `aria-hidden`: a screen reader announcing "Documentation
       single right pointing angle quotation mark Cookbook" is worse than one
       announcing two links and a word. -->
  <p class="a2ui5-crumbs">
    <template v-for="(crumb, i) in trail" :key="i">
      <span v-if="i" class="sep" aria-hidden="true">›</span>
      <a v-if="crumb.link" :href="withBase(crumb.link)">{{ crumb.text }}</a>
      <span v-else>{{ crumb.text }}</span>
    </template>
  </p>
</template>
