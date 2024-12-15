import { defineConfig } from 'vitepress'

// https://vitepress.dev/reference/site-config
export default defineConfig({
  lastUpdated: {
    text: 'Updated at',
    formatOptions: {
      dateStyle: 'full',
      timeStyle: 'medium'
    }
  },
  base: '/docs/', // Set your base URL here
  head: [
    ['link', { rel: 'shortcut icon', href: '/docs/favicon.ico' }],
    ['link', { rel: 'apple-touch-icon', sizes: '180x180', href: '/docs/favicon.ico' }],
    ['link', { rel: 'stylesheet', href: 'https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css' }],
  ],
  title: "abap2UI5",
  description: "Developing Purely in ABAP",
  themeConfig: {
    logo: '/logo.png',
    footer: {
      message: `
      <a href="/docs/resources/license">License</a> |
      <a href="/docs/resources/contact">Contact</a>`,
    copyright: `Copyright © 2023-${new Date().getFullYear()} abap2UI5`,
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
      {
        text: 'Guide', items: [
          { text: 'Getting Started', link: '/get_started/about' },
          { text: 'Development', link: '/development/general' },
          { text: 'Configuration', link: '/configuration/setup' },
          { text: 'Addons', link: '/addons/addons' },
          { text: 'Advanced', link: '/advanced/advanced' },
          { text: 'Resources', link: '/resources/resources' },
        ]
      },
      {
        text: 'Links', items: [
          {
            text: 'Project', items: [
              { text: 'abap2UI5', link: 'https://github.com/abap2UI5/abap2UI5' },
              { text: 'addons', link: 'https://github.com/abap2UI5-addons' },
              { text: 'apps', link: 'https://github.com/abap2UI5-apps' },
              { text: 'samples', link: 'https://github.com/abap2UI5/samples' },
            ]
          },
          {
            text: 'More', items: [
              { text: 'frontend', link: 'https://github.com/abap2UI5/frontend' },
              { text: 'downported', link: 'https://github.com/abap2UI5-downported' },
              { text: 'renamed', link: 'https://github.com/abap2UI5-renamed' },
              { text: 'docs', link: 'https://github.com/abap2UI5/docs' },
              { text: 'issues', link: 'https://github.com/abap2UI5/abap2UI5/issues' },
             
            ]
          },
        ]
      },
      {
        text: '1.136.0', items: [
          { text: 'Changelog', link: '/resources/changelog' },          
          { text: 'Support', link: '/resources/support' },
          { text: 'Contribution', link: '/resources/contribution' },
          { text: 'Sponsor', link: '/resources/sponsor' },
        ]
      },
    ],
    sidebar: [
      {
        text: 'Getting Started',
        link: '/get_started/about',
        collapsed: true,
        items: [
          { text: 'About', link: '/get_started/about' },
          { text: 'Quickstart', link: '/get_started/quickstart' },
          { text: 'Hello World', link: '/get_started/hello_world' },
          { text: 'Sample Apps', link: '/get_started/samples' },
          { text: 'Use Cases', link: '/get_started/use_cases' },
          { text: `What's next?`, link: '/get_started/next' }
        ]
      },
      {
        text: 'Development',
        link: '/development/general',
        collapsed: true,
        items: [
          { text: 'General', link: '/development/general' },
          { text: 'View', link: '/development/view' },
          { text: 'Model', link: '/development/model/model',  collapsed: true , items:[
            { text: 'Tables, Trees', link: '/development/model/tables' },
            { text: 'OData', link: '/development/model/odata' },
            { text: 'Device Model', link: '/development/model/device' },
          ]},
          { text: 'Events', link: '/development/events' },     
          { text: 'Navigation', link: '/development/navigation' },
          { text: 'Messages, Errors', link: '/development/messages' },
          { text: 'Translation, i18n', link: '/development/translation' },
          { text: 'Popups, Popover', link: '/development/popups' },
          { text: 'Specifics', collapsed : true , items: [
            { text: 'Barcode Scanning', link: '/development/specific/barcodes' },
            { text: 'File Handling', link: '/development/specific/files', },
            { text: 'XLSX', link: '/development/specific/xlsx', },
            { text: 'Logging', link: '/development/specific/logging' },
            { text: 'Camera', link: '/development/specific/camera' },
            { text: 'CDS, EML', link: '/development/specific/cds' },
       //     { text: 'Fuzzy Search', link: '/development/specific/fuzzy' },
            { text: 'Drag & Drop', link: '/development/specific/drag' },
            { text: 'Smart Controls', link: '/development/specific/smart_controls' },
            { text: 'Geolocation, Maps', link: '/development/specific/geolocation' },
      //      { text: 'Side Effects', link: '/development/specific/side_effects' },
           ]
          },
        ]
      },
      {
        text: 'Configuration',
        link: '/configuration/setup',
        collapsed: true,
        items: [      
          { text: 'General', link: '/configuration/setup' },
          { text: 'Security', link: '/configuration/security' },
          { text: 'Authorization', link: '/configuration/authorization' },
          { text: 'Performance', link: '/configuration/performance' },
          { text: 'UI5 Versions', link: '/configuration/ui5_versions' },
          { text: 'Productive Usage', link: '/configuration/productive_usage' },
          { text: 'Troubleshooting', link: '/configuration/troubleshooting' },
          { text: 'Installation', link: '/configuration/installation' , items: [
            { text: 'S/4 Public Cloud', link: '/configuration/s4_public_cloud' },
            { text: 'Fiori Launchpad', link: '/configuration/launchpad', },
            { text: 'BTP Workzone', link: '/configuration/btp' },
           ]
          },
        ]
      },
      {
        text: 'Addons', link: '/addons/addons', collapsed: true, items: [
          { text: 'RTTI', link: '/addons/srtti' },
          { text: 'Layouts, Variants', link: '/addons/layout' },
          { text: 'JS Libraries', link: '/addons/ext_js' },
          { text: 'UI Extensions', link: '/addons/popup' },
          { text: 'Apps', link: '/addons/apps' },
        ]
      },
      {
        text: 'Advanced',
        link: '/advanced/advanced',
        collapsed: true,
        items: [
          { text: 'Stateful Sessions', link: '/advanced/stateful' },
          { text: 'Downporting', link: '/advanced/downporting' },
          { text: 'Renaming', link: '/advanced/renaming' },
          { text: 'Remote App Calls', link: '/advanced/rfc' },
          { text: 'Extensibility',  collapsed : "false" , link: '/advanced/extensibility' , items: [
            { text: 'Custom JS', link: '/advanced/extensibility/custom_js' },
            { text: 'Custom Control', link: '/advanced/extensibility/custom_control' },
            { text: 'Frontend', link: '/advanced/extensibility/frontend' },
          ] },
          { text: 'Technical Background',  collapsed : "false" , link: '/advanced/technical/insights' , items: [
      //      { text: 'Architecture', link: '/advanced/technical/architecture' },
      //      { text: 'Designtime, Runtime', link: '/advanced/technical/designtime' },
      //      { text: 'Draft, REST', link: '/advanced/technical/rest' },
       //     { text: 'Deployment, abapGit', link: '/advanced/technical/abapgit' },
            { text: 'Linting', link: '/advanced/technical/linting' },
            { text: 'open-abap', link: '/advanced/technical/open_abap' },
          ]
          },
         ]
      },
      {
        text: 'Resources',
        link: '/resources/resources',
        collapsed: true,
        items: [
          { text: 'Changelog', link: '/resources/changelog' },
          { text: 'Blogs', link: '/resources/blogs' },
          { text: 'License', link: '/resources/license' },
          { text: 'Contact', link: '/resources/contact' },
          { text: 'Community',  items: [
            { text: 'References', link: '/resources/references' },
            { text: 'Support', link: '/resources/support' },
            { text: 'Contribution', link: '/resources/contribution' },
            { text: 'Sponsor', link: '/resources/sponsor' },
           ]
          },
        ]
      }
    ],
    outline: [2,6],
    socialLinks: [
      { icon: 'linkedin', link: 'https://www.linkedin.com/company/abap2ui5/' },
      { icon: 'github', link: 'https://www.github.com/abap2UI5/abap2UI5' },
    ]
  },
})
