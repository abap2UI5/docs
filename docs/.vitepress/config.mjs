import { defineConfig } from 'vitepress'

// https://vitepress.dev/reference/site-config
export default defineConfig({
  base: '/docs/', // Set your base URL here
  title: "abap2UI5",
  description: "Developing Purely in ABAP",
  themeConfig: {
    footer: {
      message: 'Released under the MIT License',
      copyright: '2023-present abap2UI5'
    },
    editLink: {
      pattern: 'https://github.com/abap2UI5/abap2UI5-documentation/tree/main/docs/:path',
      text: 'Edit this page on GitHub'
    },
      search: {
        provider: 'local'
      },
    // https://vitepress.dev/reference/default-theme-config
    nav: [
      { text: 'Getting Started', link: '/get_started/quickstart' },
      { text: 'Development', link: '/' },
      { text: 'Setup', link: '/' },
      { text: 'Advanced', link: '/' },
      { text: 'Insights', link: '/' },
      { text: 'Resources', link: '/resources/blogs' }
    ],
    sidebar: [
      {
        text: 'Getting Started',
        link: '/get_started/get_started',
        collapsed: true,
        items: [
          { text: 'Introduction', link: '/get_started/introduction' },
          { text: 'Quickstart', items: [
            { text: 'Standard ABAP', link: '/get_started/quickstart_standard' },
            { text: 'ABAP for Cloud', link: '/get_started/quickstart_cloud' },
          ]},
          { text: 'Hello World', link: '/get_started/hello_world' },
          { text: 'Sample Apps', link: '/get_started/sample_apps' },
          { text: `What's next?`, link: '/get_started/next'  }
        ]
      },
      {
        text: 'Development',
        link: '/development/development',
        collapsed: true,
        items: [
          { text: 'Basic Concepts' },
          { text: 'Debugging', link: '/features/' },
          { text: 'Messages & Errors', link: '/features/' },
          { text: 'Translation (i18n)', link: '/features/' },
          { text: 'User Interface', link: '/features/' },
          { text: 'Navigation', link: '/features/' },
          { text: 'Runtime Typed Data', link: '/features/' },
          {
            text: 'Addons',  items: [
              { text: 'Popups', link: '/api-examples' },
              { text: 'Layouts & Variants', link: '/api-examples' },
              { text: 'External Libraries', link: '/markdown-examples' },
            ]
          },
        ]
      },

      {
        text: 'Configuration',
        link: '/configuration/configuration',
        collapsed: true,
        items: [
          { text: 'General Settings' },
          { text: 'Productive Usage', link: '/features/' },
          { text: 'Performance', link: '/features/faq' },
          { text: 'Security & Authorization', link: '/features/faq' },
          { text: 'Fiori Launchpad', link: '/features/' },
          {
            text: 'Installation', 
            items: [
              { text: 'BTP ABAP Environment', link: '/markdown-examples' },
              { text: 'S/4 Public Cloud', link: '/markdown-examples' },
              { text: 'S/4 Private Cloud', link: '/markdown-examples' },
              { text: 'R/3 Netweaver', link: '/markdown-examples' }
            ]
          },
          {
            text: 'More', 
            items: [
              { text: 'SE80 or ADT', link: '/features/faq' },
              { text: 'UI5 Compatibility', link: '/features/faq' },
              { text: 'ABAP Language Versions', link: '/features/faq' },
            ]
          }
        ]
      },
     {
         text: 'Advanced Topics',
         link: '/advanced/advanced',
         collapsed: true,
         items: [
                  { text: 'BTP Workzone', link: '/features/' },
                  { text: 'Stateful Sessions', link: '/features/' },
                  { text: 'Remotely App Call', link: '/features/' },
                  { text: 'Downporting', link: '/features/' },
                  { text: 'Renaming', link: '/features/' },
                  { text: 'JS Transpiling', link: '/features/' },
                  {
                    text: 'Extensibility',
                    items: [
                      { text: 'Custom JS', link: '/features/' },
                      { text: 'Custom Controls', link: '/features/' },
                      { text: 'External Libraries', link: '/features/' },
                    ]
                  },
        ]
       },
       {
        text: 'Technical Insights',
        link: '/insights/insights',
        collapsed: true,
        items: [
                 { text: 'BTP Workzone', link: '/features/' },
                 { text: 'Remotely App Call', link: '/features/' },
                 { text: 'Downporting', link: '/features/' },
                 { text: 'Renaming', link: '/features/' },
       ]
      },
      {
        text: 'Resources',
        link: '/resources/resources',
        collapsed: true,
        items: [
          { text: 'Release Notes', link: '/resources/release_notes' },
          { text: 'Blogs', link: '/resources/blogs' },
          { text: 'Contribution', link: '/resources/release_notes' },
          { text: 'License', link: '/resources/license' },
          { text: 'Support', link: '/resources/license' },
        ]
      }
    ],
    socialLinks: [
      { icon: 'linkedin', link: 'https://www.linkedin.com/company/abap2ui5/' },
      { icon: 'github', link: 'https://www.github.com/abap2UI5/abap2UI5' },
    
    ]
  },
  markdown: {
    // Aktiviert den "On This Page"-Bereich
    outline: "deep"
  }
})
