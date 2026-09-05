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
import { useData } from "vitepress";

const { isDark } = useData();

/* The released framework version - z2ui5_if_app=>version in
 * abap2UI5/src/02/z2ui5_if_app.intf.abap is where the number comes from, and
 * check:version (scripts/lib/release.mjs) holds this line against the newest
 * release tag, the deprecations page and the changelog. It stood in a nav
 * dropdown of its own until the bar was rebuilt around the four sections; the
 * four entries under it were already in this menu, so what was left of that
 * dropdown was the NUMBER, and this is where it went. Moving it means moving
 * the pattern in release.mjs with it. */
const VERSION = "1.144.0";

const extra = ref(null);

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
  <!-- The rest of abap2UI5 behind one more button: light or dark, then the
       practical links (issues, release notes, install, support), the project's
       tools and its repositories by kind - the same rows, in the same order,
       as the other three bars carry. The switch says what a press does and
       swaps its whole label with the theme (style.css). -->
  <details ref="extra" class="a2ui5-extra">
    <summary class="a2ui5-extra-button" title="More: light or dark, and the rest of abap2UI5" aria-label="More: light or dark, and the rest of abap2UI5">
      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><circle cx="5" cy="12" r="2.2"/><circle cx="12" cy="12" r="2.2"/><circle cx="19" cy="12" r="2.2"/></svg>
    </summary>
    <div class="a2ui5-menu">
      <button class="theme" type="button" role="switch" :aria-checked="isDark" @click="toggleAppearance">
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
      <span class="a2ui5-menu-head">Tools</span>
      <a href="https://github.com/abap2UI5/linter" target="_blank" rel="noopener">Linter</a>
      <!-- SAME ORIGIN, ANOTHER DEPLOYMENT: `target="_self"`, not the
           `target="_blank"` the github.com rows carry, and not nothing - a
           link to abap2ui5.github.io outside /docs/ that carries no target at
           all is taken over by VitePress's router and lands on this site's
           404 (scripts/lib/cross-site.mjs). It was bound to a constant that
           stayed behind in SiteNav.vue when this menu became its own
           component, which made it exactly that link for one build;
           check:cross-site caught it. Written out, so the next split cannot
           lose it again. -->
      <a href="https://abap2ui5.github.io/linter/" target="_self">Linter rules</a>
      <a href="https://github.com/abap2UI5/vscode-extension" target="_blank" rel="noopener">VS Code extension</a>
      <a href="/docs/advanced/mcp_server">MCP server</a>
      <a href="https://github.com/abap2UI5/app-template" target="_blank" rel="noopener">App template</a>
      <a href="/docs/resources/addons">Add-ons</a>
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
        <div class="a2ui5-menu-group">
          <span class="a2ui5-menu-sub">Tools</span>
          <a href="https://github.com/abap2UI5/linter" target="_blank" rel="noopener">linter</a>
          <a href="https://github.com/abap2UI5/vscode-extension" target="_blank" rel="noopener">vscode-extension</a>
          <a href="https://github.com/abap2UI5/mcp-server" target="_blank" rel="noopener">mcp-server</a>
          <a href="https://github.com/abap2UI5/app-template" target="_blank" rel="noopener">app-template</a>
        </div>
        <div class="a2ui5-menu-group">
          <span class="a2ui5-menu-sub">Add-ons</span>
          <a href="https://github.com/abap2UI5-addons/popups" target="_blank" rel="noopener">popups</a>
          <a href="https://github.com/abap2UI5-addons/http-connector" target="_blank" rel="noopener">http-connector</a>
          <a href="https://github.com/abap2UI5-addons/rfc-connector" target="_blank" rel="noopener">rfc-connector</a>
          <a href="https://github.com/abap2UI5-addons/lock-manager" target="_blank" rel="noopener">lock-manager</a>
          <a href="https://github.com/abap2UI5-addons/launchpad-kpi" target="_blank" rel="noopener">launchpad-kpi</a>
          <a href="https://github.com/abap2UI5-addons/table-maintenance" target="_blank" rel="noopener">table-maintenance</a>
          <a href="https://github.com/abap2UI5-addons/se16n" target="_blank" rel="noopener">se16n</a>
          <a href="https://github.com/abap2UI5-addons/custom-controls" target="_blank" rel="noopener">custom-controls</a>
          <a href="https://github.com/abap2UI5-addons" target="_blank" rel="noopener">All add-ons</a>
        </div>
        <div class="a2ui5-menu-group">
          <span class="a2ui5-menu-sub">Apps</span>
          <a href="https://github.com/abap2UI5-apps/sql-console" target="_blank" rel="noopener">sql-console</a>
          <a href="https://github.com/abap2UI5-apps/table-content-loader" target="_blank" rel="noopener">table-content-loader</a>
          <a href="https://github.com/abap2UI5-apps" target="_blank" rel="noopener">All apps</a>
        </div>
      </div>
    </div>
  </details>
</template>
