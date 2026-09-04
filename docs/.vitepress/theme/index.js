// https://vitepress.dev/guide/custom-theme
import { h } from 'vue'
import DefaultTheme from 'vitepress/theme'
import './style.css'
import { setUpPlayground } from './playground.js'
import SiteBar from './SiteBar.vue'
import { rememberHere } from './site-memory.js'

/** @type {import('vitepress').Theme} */
export default {
  extends: DefaultTheme,
  // The bar's right-hand end - the three sites, and the light/dark button -
  // injected into the nav bar and into the menu a phone opens instead of it.
  // See SiteBar.vue for what the group is and style.css for where in the row
  // it lands: the slot renders after VitePress's own appearance switch and
  // social links, and CSS orders it back in front of them.
  Layout() {
    return h(DefaultTheme.Layout, null, {
      'nav-bar-content-after': () => h(SiteBar),
      'nav-screen-content-after': () => h(SiteBar, { theme: false }),
    })
  },
  enhanceApp({ app, router, siteData }) {
    // The Run button under a runnable ABAP example. One delegated listener for
    // the whole site — the browser half of docs/.vitepress/playground.mjs.
    if (!import.meta.env.SSR) setUpPlayground()

    // Where the reader is, for the Docs item on the other three bars to come
    // back to (site-memory.js). On every route change, because this site is a
    // single page application: the first page would otherwise be the only one
    // ever written down. `onAfterRouteChange` rather than `onBeforeRouteChange`
    // - the location is the new one only after the change has happened.
    if (!import.meta.env.SSR) {
      rememberHere('docs')
      const onAfter = router.onAfterRouteChange
      router.onAfterRouteChange = (to) => {
        rememberHere('docs')
        onAfter?.(to)
      }
    }
  }
}
