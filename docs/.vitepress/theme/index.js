// https://vitepress.dev/guide/custom-theme
import { h } from 'vue'
// ...WITHOUT ITS FONTS: the theme's own fonts.css brings all fourteen Inter
// subsets and a Google Fonts @import; this site serves the two Latin subsets
// it needs from docs/public/fonts/ instead (style.css).
import DefaultTheme from 'vitepress/theme-without-fonts'
import './style.css'
import { setUpPlayground } from './playground.js'
import { setUpCodeLines, watchCodeLines } from './code-lines.js'
import { markDirective, setUpLinkToSelection } from './link-to-selection.js'
import TheBar from './TheBar.vue'
import Crumbs from './Crumbs.vue'

/** @type {import('vitepress').Theme} */
export default {
  extends: DefaultTheme,
  // The bar's right-hand end - the three sites, and the menu behind the bar's
  // last button with the light/dark switch in it - injected into the nav bar
  // and, without the menu, into the screen a phone opens instead of it. See
  // SiteNav.vue for what the group is and style.css for where in the row it
  // lands: the slot renders after VitePress's own appearance switch and
  // social links, and CSS orders the sites back in front of the marks and the
  // menu after them.
  // ONE ELEMENT, AND IT IS OURS (TheBar.vue). The theme's own bar keeps
  // exactly two parts, because they are worth keeping and not worth
  // rebuilding: the hamburger, and the screen it opens on a phone - which
  // carries the same four sections as a list.
  Layout() {
    return h(DefaultTheme.Layout, null, {
      'nav-bar-content-before': () => h(TheBar),
      // Where in the manual you are, above the title, on every page that has
      // one (Crumbs.vue). `doc-before` is the slot immediately in front of the
      // article inside the doc column, so the trail is the column's first line
      // - which is where the catalogue's own is - and the home page, which is
      // not laid out as a document, does not get one.
      'doc-before': () => h(Crumbs),
    })
  },

  enhanceApp({ app, router, siteData }) {
    // The Run button under a runnable ABAP example. One delegated listener for
    // the whole site — the browser half of docs/.vitepress/playground.mjs.
    // It answers with what arms the front door's self-starting example, which
    // a route change has to call again: thrown away, Home reached in-app
    // (about -> brand -> Home) never started its example.
    const rearmPlayground = import.meta.env.SSR ? undefined : setUpPlayground()


    // A number beside every line of every listing, and an address for it -
    // #B2L42 is line 42 of the second listing on the page, #B2L42-L58 a
    // passage of it. The sample catalogue has had this since its pages
    // started printing whole classes; the manual is where the same ABAP is
    // explained, and "look at line 40 to 55" could not be said about it.
    // code-lines.js says why the block is part of the address.
    //
    // The listeners are the document's and are hung once; the numbering runs
    // again on every route change, because this is one application and the
    // next page's listings are new elements.
    if (!import.meta.env.SSR) watchCodeLines()

    // "Copy link to selection" — a link to the words a reader marked, as a
    // text fragment (link-to-selection.js, and text-fragment.js for why the
    // link names the words rather than the line they are on). One delegated
    // listener for the whole site, like the Run button above it; the second
    // call is only ever reached by a browser too old for `:~:`, and lands it
    // where the words are instead of at the top of the page.
    if (!import.meta.env.SSR) {
      setUpLinkToSelection()
      if (document.readyState === 'complete') markDirective()
      else window.addEventListener('load', markDirective, { once: true })
    }

    // This site keeps no memory shared with the other abap2UI5 sites: it is
    // built and deployed on its own, and the way to the playground or the
    // samples is a plain link. Scrolling is the router's.
    if (!import.meta.env.SSR) {
      setUpCodeLines()
      const onAfter = router.onAfterRouteChange
      router.onAfterRouteChange = (to) => {
        // Home -> Documentation is a route change, not a load: this site is
        // one application and two of the bar's four items are pages of it.
        setUpCodeLines()
        rearmPlayground?.()
        onAfter?.(to)
      }
    }
  }
}
