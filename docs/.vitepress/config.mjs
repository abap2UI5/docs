import { defineConfig } from 'vitepress'

// https://vitepress.dev/reference/site-config
export default defineConfig({
  title: "abap2UI5",
  description: "Developing Purely in ABAP",
  themeConfig: {
    // https://vitepress.dev/reference/default-theme-config
    nav: [
      { text: 'Get Started', link: '/get_started/quickstart' },
      { text: 'Contribution', link: '/' },
      { text: 'Integration', link: '/' },
      { text: 'Resources', link: '/resources/blogs' }
    ],
    sidebar: [
      {
        text: 'Get Started',
        collapsed: true,
        items: [
          { text: 'Markdown Examples', link: '/get_started/quickstart' },
          { text: 'Runtime API Examples', link: '/api-examples' }
        ]
      },
      {
        text: 'Contribution',
        collapsed: true,
        items: [
          { text: 'Markdown Examples', link: '/markdown-examples' },
          { text: 'Runtime API Examples', link: '/api-examples' }
        ]
      },
      {
        text: 'Integration',
        collapsed: true,
        items: [
          { text: 'Business Technology Platform', link: '/markdown-examples' },
          { text: 'Build Workzone', link: '/markdown-examples' },
          { text: 'Fiori Launchpad', link: '/api-examples' }
        ]
      },
      {
        text: 'Resources',
        collapsed: true,
        items: [
          { text: 'Release Notes', link: '/resources/release_notes' },
          { text: 'Blogs', link: '/resources/blogs' },
          { text: 'Links', link: '/resources/links' }
        ]
      }
    ],
    socialLinks: [
      { icon: 'github', link: 'https://github.com/abap2UI5' }
    ]
  },
  markdown: {
    // Aktiviert den "On This Page"-Bereich
    outline: 'deep'
  }
})
