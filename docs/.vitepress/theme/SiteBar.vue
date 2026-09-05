<script setup>
/*
 * The right-hand end of the bar, which is the playground's right-hand end and
 * the sample catalogue's (src/shell/index.html, src/catalogue/catalogue.css in
 * abap2UI5/playground) down to the numbers: a hairline, where in this project
 * you are, a hairline, the two marks, then one more button, drawn as a third
 * mark, that opens the menu - the switch for light and dark, the project's
 * tools and its repositories. Four documents carry that group now - this one,
 * the playground, the catalogue and the per-sample pages - and somebody moving
 * between them reads ONE bar, so it is kept identical by hand rather than left
 * to four interpretations of a house style.
 *
 * The two marks are not here: they are VitePress's own `socialLinks`, which
 * draws the same LinkedIn and GitHub inline. The hairlines are in style.css,
 * where the ordering that puts the three sites before the marks and the menu
 * after them also is.
 *
 * The menu is a <details>, so it opens and closes with no script; the two
 * listeners below close it the two ways a menu is expected to close and a
 * <details> does not - a click anywhere outside it, and Escape. The same lines
 * as setUpExtra() in the playground's catalogue.mjs and extra.mjs.
 */
import { inject, onMounted, ref, watch } from "vue";
import { useData } from "vitepress";
import { lastVisited } from "./site-memory.js";

const { isDark } = useData();

/* The same three items serve the bar and the menu a phone opens instead of it
 * (`nav-screen-content-after`). Only the menu behind the third mark differs:
 * down there VitePress draws its own appearance control and the Links group,
 * and two of each in one screen is one too many. */
defineProps({ theme: { type: Boolean, default: true } });

const PLAYGROUND = "https://abap2ui5.github.io/playground/";
const SAMPLES = "https://abap2ui5.github.io/playground/samples/";

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

/* The catalogue's front page until the browser says otherwise. This is what
 * the server renders, what a crawler is given and what a first visit follows;
 * onMounted only ever replaces it with a deeper page of the same site. */
const samplesHref = ref(SAMPLES);
const extra = ref(null);
/* Lifted on mount, and again whenever it can have moved while this page stayed
 * open: the catalogue narrowed in another tab, a Back that brought this page
 * out of the back-forward cache. The click itself is the lift that cannot be
 * missed - the href is set on the element there, before the browser follows
 * it, because a ref set in the handler reaches the DOM a tick too late. */
const lift = () => {
  samplesHref.value = lastVisited("samples", SAMPLES);
};
const liftNow = (e) => {
  e.currentTarget.href = lastVisited("samples", SAMPLES);
};
onMounted(() => {
  lift();
  addEventListener("pageshow", lift);
  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "visible") lift();
  });
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
  <nav class="bar-nav" aria-label="abap2UI5 sites">
    <!-- The one you are on, which is why it is a span and not a link - the
         look aria-current gets on the other three bars. -->
    <span class="here" aria-current="page">Documentation</span>
    <a :href="samplesHref" :target="SAME_TAB" data-site="samples" title="Every abap2UI5 sample, searchable" @click="liftNow">Samples</a>
    <a :href="PLAYGROUND" :target="SAME_TAB" title="Write ABAP and run it in the browser">Playground</a>
  </nav>
  <!-- The rest of abap2UI5 behind one more button: light or dark, then the
       practical links (issues, release notes, install, support), the project's
       tools and its repositories by kind - the same rows, in the same order,
       as the other three bars carry. The switch says what a press does and
       swaps its whole label with the theme (style.css). -->
  <details v-if="theme" ref="extra" class="a2ui5-extra">
    <summary class="a2ui5-extra-button" title="More: light or dark, and the rest of abap2UI5" aria-label="More: light or dark, and the rest of abap2UI5">
      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><circle cx="5" cy="12" r="2.2"/><circle cx="12" cy="12" r="2.2"/><circle cx="19" cy="12" r="2.2"/></svg>
    </summary>
    <div class="a2ui5-menu">
      <button class="theme" type="button" role="switch" :aria-checked="isDark" @click="toggleAppearance">
        <span class="when-light"><span class="glyph" aria-hidden="true">☾</span>Switch to dark</span>
        <span class="when-dark"><span class="glyph" aria-hidden="true">☀</span>Switch to light</span>
      </button>
      <a href="https://github.com/abap2UI5/abap2UI5/issues" target="_blank" rel="noopener">Issues</a>
      <a href="/docs/resources/changelog">Release notes</a>
      <a href="/docs/get_started/quickstart">Install with abapGit</a>
      <a href="/docs/resources/support">Support</a>
      <a href="/docs/resources/contribution">Contribute</a>
      <a href="/docs/resources/sponsor">Sponsor</a>
      <span class="a2ui5-menu-head">Tools</span>
      <a href="https://github.com/abap2UI5/linter" target="_blank" rel="noopener">Linter</a>
      <!-- Same origin, another deployment: the attribute above, not the
           `target="_blank"` the github.com rows carry. -->
      <a href="https://abap2ui5.github.io/linter/" :target="SAME_TAB">Linter rules</a>
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
