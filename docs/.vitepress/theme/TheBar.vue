<script setup>
/*
 * THE BAR, AS ONE ELEMENT THIS REPOSITORY OWNS.
 *
 * It used to be VitePress's bar with our parts pushed into its slots and its
 * own parts argued out of the way in CSS: 85 selector lines under
 * `.Layout .VPNav …`, fighting 42 theme elements — the brand's absolute
 * placement over the sidebar column, the flex `order` of five slot children,
 * the theme's `position: relative` on two wrappers, its "…" menu, its
 * appearance switch, its social links' margins. Every fix in that layout had
 * to undo something first; centring the search box needed two of the theme's
 * own rules taken back before it could be measured at all.
 *
 * So the bar is one element now, rendered into `nav-bar-content-before`, and
 * everything inside it is ours: the brand, the four sections, the search box,
 * the two marks, the menu. The theme keeps exactly two things, because they
 * are worth keeping and are not worth rebuilding: the hamburger and the screen
 * it opens on a phone (`nav-screen-content-after` carries the same SiteNav).
 *
 * What this buys, beyond the deleted CSS: the four documents now carry the
 * same MARKUP, not four readings of one description — this template is
 * `src/shell/index.html`'s bar, in Vue.
 */
import { withBase } from "vitepress";
import SearchBox from "./SearchBox.vue";
import SiteNav from "./SiteNav.vue";
import SiteMenu from "./SiteMenu.vue";

/* Through `withBase`, not written out: a bare `/docs/logo.png` in a template
 * is an import to Vite, which then tries to resolve it as a module and fails
 * the build. It is also the one place the site's base path would have to be
 * repeated by hand. */
const HOME = withBase("/");
const LOGO = withBase("/logo.png");
</script>

<template>
  <div class="a2ui5-bar">
    <!-- The mark and the name, closed by a hairline. A link to the home page,
         which is what a brand is everywhere else on this origin; the nav says
         which section you are in, so the brand does not say it too. -->
    <a class="a2ui5-brand" :href="HOME">
      <img :src="LOGO" alt="" width="20" height="20">
      <span>abap2UI5</span>
    </a>

    <SiteNav />

    <!-- Centred on THIS element, which spans the whole bar — the reason the
         box can simply be placed at 50% now instead of being pushed towards
         the middle by auto margins in a row that starts after the brand. -->
    <SearchBox />

    <!-- The two marks, inline SVG rather than by name: VitePress draws a named
         icon from a CSS mask and, if that mask was not generated, fetches the
         glyph from a CDN. These are the exact paths the other three bars
         carry, so the end of the bar is the same two shapes on all four
         documents rather than two drawings of the same two brands. -->
    <div class="a2ui5-socials">
      <a href="https://www.linkedin.com/company/abap2ui5/" target="_blank" rel="noopener"
         aria-label="abap2UI5 on LinkedIn" title="abap2UI5 on LinkedIn">
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path d="M20.447 20.452h-3.554v-5.569c0-1.328-.027-3.037-1.852-3.037-1.853 0-2.136 1.445-2.136 2.939v5.667H9.351V9h3.414v1.561h.046c.477-.9 1.637-1.85 3.37-1.85 3.601 0 4.267 2.37 4.267 5.455v6.286zM5.337 7.433c-1.144 0-2.063-.926-2.063-2.065 0-1.138.92-2.063 2.063-2.063 1.14 0 2.064.925 2.064 2.063 0 1.139-.925 2.065-2.064 2.065zm1.782 13.019H3.555V9h3.564v11.452zM22.225 0H1.771C.792 0 0 .774 0 1.729v20.542C0 23.227.792 24 1.771 24h20.451C23.2 24 24 23.227 24 22.271V1.729C24 .774 23.2 0 22.225 0z"/></svg>
      </a>
      <a href="https://github.com/abap2UI5/abap2UI5" target="_blank" rel="noopener"
         aria-label="abap2UI5 on GitHub" title="abap2UI5 on GitHub">
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path d="M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"/></svg>
      </a>
    </div>

    <SiteMenu />
  </div>
</template>
