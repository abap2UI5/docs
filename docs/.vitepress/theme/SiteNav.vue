<script setup>
/*
 * The four sections of the project — the same four, in the same order, as the
 * playground's bar, the catalogue's and every per-sample page's
 * (src/shell/index.html, src/catalogue/index.html, tools/sample-pages.mjs in
 * abap2UI5/playground). Somebody moving between the four documents reads ONE
 * bar, so the group is kept identical by hand rather than left to four
 * interpretations of a house style.
 *
 * This component is used twice: in the bar (TheBar.vue) and in the menu a
 * phone opens instead of it, where the same four items are a list rather than
 * a strip.
 */
import { computed, onMounted, ref } from "vue";
import { useData } from "vitepress";
import { lastVisited } from "./site-memory.js";

const { page } = useData();

/* EVERY LINK OUT OF THIS SITE AND INTO A NEIGHBOURING ONE CARRIES A `target`,
 * and it is not decoration: without it the link does not arrive.
 *
 * The three sites share an origin - the thing that makes the shared theme and
 * the shared position memory possible - and that is also what breaks a plain
 * link between them. This site is a single page application: VitePress's
 * router takes over any link that is same-origin and looks like a page, and
 * /playground/ is both. It then has no page of THIS site to render there, so
 * it drew this site's 404 at the playground's URL. `_self` is the value
 * because it is the behaviour these items already promised - one site, one
 * tab. A link to another host needs nothing; only abap2ui5.github.io outside
 * /docs/ is affected. scripts/lib/cross-site.mjs is the whole story, and
 * scripts/check-cross-site.mjs holds the built site to it. */
const SAME_TAB = "_self";

const PLAYGROUND = "https://abap2ui5.github.io/playground/";
const SAMPLES = "https://abap2ui5.github.io/playground/samples/";
/* The two sections of THIS deployment. Home is the page the brand has always
 * opened and nothing else ever named; Documentation opens the manual. Both are
 * pages of this site, so neither needs the `target` the two above carry. */
const HOME = "/docs/";
const DOCS = "/docs/get_started/about";

/* Which of the two is the one you are on. `relativePath` rather than the URL:
 * it is what the server rendered as well, so the bar is right in the HTML and
 * not only after hydration. */
const onHome = computed(() => page.value.relativePath === "index.md");

/* The two links the memory can move, as the SERVER renders them: the front
 * page of each section. That is what a crawler is given and what a first visit
 * follows; onMounted only ever replaces one with a deeper page of the same
 * section. */
const samplesHref = ref(SAMPLES);
const docsHref = ref(DOCS);

/* Documentation restores WHEREVER IN THE MANUAL you were, which is why it
 * passes a scope: the link is written at the manual's first page, and a stored
 * /docs/cookbook/... is not inside that page. Without the third argument every
 * restore falls back - which is how this item behaved on this site while the
 * other three bars restored it correctly. The scope is a value passed HERE,
 * never one out of storage.
 *
 * The Home page is deliberately not part of it: rememberHere() does not write
 * it (theme/index.js), so going Home and back does not overwrite the page you
 * were reading with the front door. */
const lift = () => {
  samplesHref.value = lastVisited("samples", SAMPLES);
  docsHref.value = lastVisited("docs", DOCS, HOME);
};
/* The lift that cannot be missed: on the click itself, on the element, because
 * a ref set in the handler reaches the DOM a tick too late. */
const liftNow = (e) => {
  const el = e.currentTarget;
  el.href = el.dataset.site === "docs"
    ? lastVisited("docs", DOCS, HOME)
    : lastVisited("samples", SAMPLES);
};

/* Lifted on mount, and again whenever the stored position can have moved while
 * this page stayed open: the catalogue narrowed in another tab, a Back that
 * brought this page out of the back-forward cache. */
onMounted(() => {
  lift();
  addEventListener("pageshow", lift);
  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "visible") lift();
  });
});
</script>

<template>
  <nav class="bar-nav" aria-label="abap2UI5 sites">
    <!-- The four sections of the project, left to right in the order a reader
         meets them, and the one you are on marked. Home and Documentation are
         two sections of THIS deployment now - the section that used to be one
         non-clickable word ("Documentation") is two places a reader can move
         between, which is what the brand alone could not offer. -->
    <a :href="HOME" data-text="Home" :class="{ here: onHome }" :aria-current="onHome ? 'page' : undefined" title="abap2UI5 in one page">Home</a>
    <a :href="docsHref" data-text="Documentation" data-site="docs" :class="{ here: !onHome }" :aria-current="!onHome ? 'page' : undefined" title="The manual, where you left it" @click="liftNow">Documentation</a>
    <a :href="samplesHref" data-text="Samples" :target="SAME_TAB" data-site="samples" title="Every abap2UI5 sample, searchable" @click="liftNow">Samples</a>
    <a :href="PLAYGROUND" data-text="Playground" :target="SAME_TAB" title="Write ABAP and run it in the browser">Playground</a>
  </nav>
</template>
