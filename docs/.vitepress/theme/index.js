// https://vitepress.dev/guide/custom-theme
//
// THE DEFAULT THEME, AS IT COMES. The bar, the search, the sidebar, the
// outline and the home page are VitePress's own, so the manual reads like
// every other VitePress site a developer already knows. What this repository
// adds is the family's red (style.css) and the Run button under a runnable
// ABAP example (playground.js) - nothing else.
//
// Without its fonts: the theme's own fonts.css brings all fourteen Inter
// subsets; this site serves the two Latin subsets it needs from
// docs/public/fonts/ instead (style.css). Same typeface, fewer files.
import DefaultTheme from 'vitepress/theme-without-fonts'
import './style.css'
import { setUpPlayground } from './playground.js'

/** @type {import('vitepress').Theme} */
export default {
  extends: DefaultTheme,

  enhanceApp({ router }) {
    if (import.meta.env.SSR) return
    // The Run button under a runnable ABAP example. One delegated listener for
    // the whole site — the browser half of docs/.vitepress/playground.mjs.
    // It answers with what arms the home page's self-starting example, which
    // a route change has to call again.
    const rearmPlayground = setUpPlayground()
    const onAfter = router.onAfterRouteChange
    router.onAfterRouteChange = (to) => {
      rearmPlayground?.()
      onAfter?.(to)
    }
  }
}
