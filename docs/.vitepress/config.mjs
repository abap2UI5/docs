import { defineConfig } from 'vitepress'

// https://vitepress.dev/reference/site-config
export default defineConfig({
  base: '/abap2UI5-documentation/', // Set your base URL here
  title: "abap2UI5",
  description: "Developing Purely in ABAP",
  themeConfig: {
    editLink: {
      pattern: 'https://github.com/abap2UI5/abap2UI5-documentation/tree/main/docs/:path',
      text: 'Edit this page on GitHub'
    },
      search: {
        provider: 'local'
      },
    // https://vitepress.dev/reference/default-theme-config
    nav: [
      { text: 'Get Started', link: '/get_started/quickstart' },
      { text: 'Development', link: '/' },
      { text: 'Administration', link: '/' },
      { text: 'Technical Details', link: '/' },
      { text: 'Resources', link: '/resources/blogs' }
    ],
    sidebar: [
      {
        text: 'Get Started',
        link: '/get_started/introduction',
        collapsed: true,
        items: [
          { text: 'Introduction', link: '/get_started/introduction' },
          { text: 'Installation', items: [
            { text: 'Standard ABAP', link: '/get_started/quickstart' },
            { text: 'ABAP for Cloud', link: '/get_started/quickstart2' },
          ]},
          { text: 'Hello World', link: '/get_started/hello_world' },
          { text: 'Sample Apps', link: '/get_started/sample_apps' },
          { text: `What's next?`, link: '/get_started/overview'  }
        ]
      },
      {
        text: 'Development',
        collapsed: true,
        items: [
          { text: 'Basics', link: '/features/faq' },
          { text: 'Debugging', link: '/features/' },
          { text: 'Stateful Sessions', link: '/features/' },
          { text: 'Popups', link: '/features/' },
          { text: 'Localization, i18n', link: '/features/' },
          { text: 'External Libraries', link: '/features/' },
          { text: 'Custom Controls', link: '/features/' },
          {
            text: 'Addons', collapsed: true, items: [
              { text: 'Custom Controls', link: '/markdown-examples' },
              { text: 'Popups', link: '/api-examples' },
              { text: 'Layout & Variants', link: '/api-examples' },
            ]
          },
        ]
      },
      {
        text: 'Administration',
        collapsed: true,
        items: [
          { text: 'Configuration', collapsed: true , items: [
            { text: 'Theme', link: '/get_started/configuration#theme' },
            { text: 'UI5 Bootstrapping', link: '/get_started/configuration#ui5-bootstrapping' },
            { text: 'CSP', link: '/get_started/configuration#Content-Security-Policy' },
            { text: 'Title', link: '/get_started/configuration#title' }
          ]},
          { text: 'Remotely App Call', link: '/features/' },
          { text: 'Downporting', link: '/features/' },
          { text: 'Multiple Installations', link: '/features/' },
          { text: 'Fiori Launchpad', link: '/features/' },
          { text: 'BTP Workzone', link: '/features/' },
          { text: 'Productive Usage', link: '/features/' },
          { text: 'Performance', link: '/features/faq' },
          { text: 'Security', link: '/features/faq' },
          { text: 'UI5 Framework', link: '/features/faq' },
          {
            text: 'Setup', collapsed: true,
            items: [
              { text: 'BTP ABAP Environment', link: '/markdown-examples' },
              { text: 'S/4 Public Cloud', link: '/markdown-examples' },
              { text: 'S/4 Private Cloud', link: '/markdown-examples' },
              { text: 'R/3 Netweaver', link: '/markdown-examples' }
            ]
          }
        ]
      },
      // {
      //   text: 'Technical Details',
      //   collapsed: true,
      //   items: [
      //     { text: 'Communication', link: '/features/' },
      //     { text: 'Persistence', link: '/features/' },
      //     { text: 'Data Binding', link: '/features/' },
      //     { text: 'User Interface', link: '/features/faq' },
      //   ]
      // },
      {
        text: 'Resources',
        collapsed: true,
        items: [
          { text: 'Release Notes', link: '/resources/release_notes' },
          { text: 'Blogs', link: '/resources/blogs' },
          { text: 'Contribution', link: '/resources/release_notes' },
          { text: 'License', link: '/resources/license' },
        ]
      }
    ],
    socialLinks: [
      { icon: 'github', link: 'https://github.com/abap2UI5' },
      { icon: 'linkedin', link: 'https://www.linkedin.com/company/abap2ui5/' }
    ]
  },
  markdown: {
    // Aktiviert den "On This Page"-Bereich
    outline: "deep"
  }
})
