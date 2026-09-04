<script setup>
/*
 * The right-hand end of the bar, which is the playground's right-hand end and
 * the sample catalogue's (src/shell/index.html, src/catalogue/catalogue.css in
 * abap2UI5/playground) down to the numbers: where in this project you are, a
 * button for light and dark, one hairline, then the two marks. Four documents
 * carry that group now - this one, the playground, the catalogue and the
 * per-sample pages - and somebody moving between them reads ONE bar, so it is
 * kept identical by hand rather than left to four interpretations of a house
 * style.
 *
 * The two marks are not here: they are VitePress's own `socialLinks`, which
 * draws the same LinkedIn and GitHub inline. The hairline in front of them is
 * in style.css, where the ordering that puts this group before them also is.
 */
import { inject, onMounted, ref, watch } from "vue";
import { useData } from "vitepress";
import { lastVisited } from "./site-memory.js";

const { isDark } = useData();

/* The same three items serve the bar and the menu a phone opens instead of it
 * (`nav-screen-content-after`). Only the button differs: down there VitePress
 * draws its own appearance control, and two of them in one menu is one too
 * many. */
defineProps({ theme: { type: Boolean, default: true } });

const PLAYGROUND = "https://abap2ui5.github.io/playground/";
const SAMPLES = "https://abap2ui5.github.io/playground/samples/";

/* The catalogue's front page until the browser says otherwise. This is what
 * the server renders, what a crawler is given and what a first visit follows;
 * onMounted only ever replaces it with a deeper page of the same site. */
const samplesHref = ref(SAMPLES);
onMounted(() => {
  samplesHref.value = lastVisited("samples", SAMPLES);
});

/* VitePress's own toggle, so a site that provides one (the appearance
 * transition) keeps it; the fallback is what VPSwitchAppearance does. */
const toggleAppearance = inject("toggle-appearance", () => {
  isDark.value = !isDark.value;
});

/* And the choice, handed to the other two deployments. The head script in
 * config.mjs reads this key back before the first paint; the two halves are
 * one mechanism. Not on the server, and not on the first render either - only
 * when the reader actually changes it. */
watch(isDark, (dark) => {
  try {
    localStorage.setItem("abap2ui5-playground:theme", dark ? "dark" : "light");
  } catch {
    /* A refused or full storage. This tab is dark; the next site is not. */
  }
});
</script>

<template>
  <nav class="bar-nav" aria-label="abap2UI5 sites">
    <a :href="PLAYGROUND" title="Write ABAP and run it in the browser">Playground</a>
    <a :href="samplesHref" data-site="samples" title="Every abap2UI5 sample, searchable">Samples</a>
    <!-- The one you are on, which is why it is a span and not a link - the
         look aria-current gets on the other three bars. -->
    <span class="here" aria-current="page">Docs</span>
  </nav>
  <button
    v-if="theme"
    class="theme"
    type="button"
    role="switch"
    :aria-checked="isDark"
    :aria-label="isDark ? 'Switch to light theme' : 'Switch to dark theme'"
    :title="isDark ? 'Switch to light theme' : 'Switch to dark theme'"
    @click="toggleAppearance"
  >
    <span class="theme-sun" aria-hidden="true">☀</span>
    <span class="theme-moon" aria-hidden="true">☾</span>
  </button>
</template>
