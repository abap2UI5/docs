<script setup>
/*
 * The menu behind the bar's last button, drawn as a third mark: the switch for
 * light and dark, the practical links, the project's tools and its
 * repositories. The same rows in the same order as the other three bars carry
 * (src/shell/index.html, src/catalogue/index.html, tools/sample-pages.mjs in
 * abap2UI5/playground), kept in step by hand.
 *
 * It is a <details>, so it opens and closes with no script; the two listeners
 * below close it the two ways a menu is expected to close and a <details> does
 * not - a click anywhere outside it, and Escape. The same lines as
 * setUpExtra() in the playground's catalogue.mjs and extra.mjs.
 */
import { inject, onMounted, ref, watch } from "vue";
import { useData, useRoute } from "vitepress";

const { isDark } = useData();

/* The released framework version - z2ui5_if_app=>version in
 * abap2UI5/src/02/z2ui5_if_app.intf.abap is where the number comes from, and
 * check:version (scripts/lib/release.mjs) holds this line against the newest
 * release tag, the deprecations page and the changelog. It stood in a nav
 * dropdown of its own until the bar was rebuilt around the four sections; the
 * four entries under it were already in this menu, so what was left of that
 * dropdown was the NUMBER, and this is where it went. Moving it means moving
 * the pattern in release.mjs with it. */
const VERSION = "1.148.0";

const extra = ref(null);

/* Closed by a route change too: an internal link in the menu (Release notes)
 * is an in-app navigation here, no click lands outside it, and the menu
 * stayed open over the new page. The static build reloads, so it never had
 * this. */
const route = useRoute();
watch(() => route.path, () => {
  if (extra.value) extra.value.open = false;
});

onMounted(() => {
  document.addEventListener("click", (e) => {
    const el = extra.value;
    if (el?.open && !el.contains(e.target)) el.open = false;
  });
  document.addEventListener("keydown", (e) => {
    const el = extra.value;
    if (e.key === "Escape" && el?.open) {
      el.open = false;
      el.querySelector("summary")?.focus();
    }
  });
});

/* VitePress's own toggle, so a site that provides one (the appearance
 * transition) keeps it; the fallback is what VPSwitchAppearance does. */
const toggleAppearance = inject("toggle-appearance", () => {
  isDark.value = !isDark.value;
});

</script>

<template>
  <!-- The rest of abap2UI5 behind one more button: light or dark, then the
       practical links (issues, release notes, install, support), the project's
       tools and its repositories - the same rows, in the same order,
       as the other three bars carry. The switch says what a press does and
       swaps its whole label with the theme (style.css). -->
  <details ref="extra" class="a2ui5-extra">
    <summary class="a2ui5-extra-button" title="More: light or dark, and the rest of abap2UI5" aria-label="More: light or dark, and the rest of abap2UI5">
      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><circle cx="5" cy="12" r="2.2"/><circle cx="12" cy="12" r="2.2"/><circle cx="19" cy="12" r="2.2"/></svg>
    </summary>
    <div class="a2ui5-menu">
      <button class="theme" type="button" role="switch" :aria-checked="isDark" aria-label="Dark theme" @click="toggleAppearance">
        <span class="when-light"><span class="glyph" aria-hidden="true">☾</span>Switch to dark</span>
        <span class="when-dark"><span class="glyph" aria-hidden="true">☀</span>Switch to light</span>
      </button>
      <span class="a2ui5-menu-head">Version {{ VERSION }}</span>
      <a href="https://github.com/abap2UI5/abap2UI5/issues" target="_blank" rel="noopener">Issues</a>
      <a href="/docs/resources/changelog">Release notes</a>
      <a href="/docs/get_started/quickstart">Install with abapGit</a>
      <a href="/docs/resources/support">Support</a>
      <a href="/docs/resources/contribution">Contribute</a>
      <a href="/docs/resources/sponsor">Sponsor</a>
      <!-- The other abap2UI5 sites, named here and nowhere else on this
           site: the bar carries no row of them (CONVENTION.md in
           abap2UI5/abap2UI5.github.io). Same origin, other deployments:
           target="_self", as the Linter rules row below explains. -->
      <span class="a2ui5-menu-head">Sites</span>
      <a href="https://abap2ui5.github.io/" target="_self">Home</a>
      <a href="https://abap2ui5.github.io/why/" target="_self">For managers</a>
      <a href="https://abap2ui5.github.io/samples-site/" target="_self">Samples</a>
      <a href="https://abap2ui5.github.io/playground/" target="_self">Playground</a>
      <a href="https://abap2ui5-addons.github.io/">Addons &amp; Tools</a>
      <span class="a2ui5-menu-head">Tools</span>
      <!-- The standalone tools: the AI Studio (an app built from a
           description, on the playground's deployment), the Layout Editor and
           the Report converter (abap2UI5-addons/abap-cloud-gui). The first
           two are on this origin, so `target="_self"` as the Linter rules row
           below says; the third is on another host and needs none, but
           carries it so the rows read alike. -->
      <a href="https://abap2ui5.github.io/playground/ai/" target="_self">AI Studio</a>
      <a href="https://abap2ui5.github.io/layout-editor/" target="_self">Layout Editor</a>
      <a href="https://abap2ui5-addons.github.io/abap-cloud-gui/" target="_self">Report converter</a>
      <a href="https://abap2ui5.github.io/linter/" target="_self">Linter rules</a>
      <!-- Everything to install beside the framework, and the tools for the
           editor, the pipeline and the agent, each with a page: a site of
           its own. Not listed here a second time. -->
      <span class="a2ui5-menu-head">Repositories</span>
      <div class="a2ui5-menu-repos">
        <div class="a2ui5-menu-group">
          <span class="a2ui5-menu-sub">Framework</span>
          <a href="https://github.com/abap2UI5/abap2UI5" target="_blank" rel="noopener">abap2UI5</a>
          <a href="https://github.com/abap2UI5/frontend" target="_blank" rel="noopener">frontend</a>
          <a href="https://github.com/abap2UI5/abap2UI5-local" target="_blank" rel="noopener">abap2UI5-local</a>
          <a href="https://github.com/abap2UI5/mirror-ajson" target="_blank" rel="noopener">mirror-ajson</a>
          <a href="https://github.com/abap2UI5/mirror-srtti" target="_blank" rel="noopener">mirror-srtti</a>
          <a href="https://github.com/abap2UI5/web-abap2UI5" target="_blank" rel="noopener">web-abap2UI5</a>
        </div>
        <div class="a2ui5-menu-group">
          <span class="a2ui5-menu-sub">Samples</span>
          <a href="https://github.com/abap2UI5/samples" target="_blank" rel="noopener">samples</a>
          <a href="https://github.com/abap2UI5/samples-controls" target="_blank" rel="noopener">samples-controls</a>
          <a href="https://github.com/abap2UI5/samples-stack" target="_blank" rel="noopener">samples-stack</a>
        </div>
        <div class="a2ui5-menu-group">
          <span class="a2ui5-menu-sub">Sites</span>
          <a href="https://github.com/abap2UI5/docs" target="_blank" rel="noopener">docs</a>
          <a href="https://github.com/abap2UI5/playground" target="_blank" rel="noopener">playground</a>
        </div>
        </div>
    </div>
  </details>
</template>
