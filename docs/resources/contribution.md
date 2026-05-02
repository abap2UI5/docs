---
outline: [2, 3]
---
# Contribution

abap2UI5 is an open-source project built by the ABAP community, often in free time or alongside other projects. All kinds of contributions are welcome.

## How You Can Contribute

There are several ways to get involved and help:

- Extend the view by adding a new property
- Build a control to extend existing features
- Build custom controls for specific scenarios
- Share your demos and apps to showcase what's possible with abap2UI5
- Improve the documentation and polish code snippets to make abap2UI5 easier to learn
- Propose something new — every contribution is appreciated, no matter the size

## Documentation Style

The [docs repository](https://github.com/abap2UI5/docs) uses [VitePress](https://vitepress.dev). When editing or creating pages, follow the conventions below to keep navigation, search, and the in-page outline working.

### Frontmatter

Every page starts with:

```yaml
---
outline: [2, 3]
---
# Page Title
```

`outline: [2, 3]` keeps the right-hand outline focused on `##` and `###` levels.

### Headings

- `# Title` — exactly one per page, matches the sidebar entry.
- `## Section` — major sections; show up in the outline.
- `### Subsection` — used inside a major section.
- Don't skip levels. Don't start a page with `###` (the outline will be empty).

### Admonitions

VitePress callouts use this convention:

```markdown
::: tip Optional Heading
Best practice or helpful pointer.
:::

::: warning Optional Heading
Stolperstein, breaking change, or behavior to watch out for.
:::

::: info Optional Heading
Background context, not strictly required reading.
:::

::: details Optional Heading
Collapsed by default — use for ABAP Cloud variants, long screenshots, or extended examples.
:::
```

The optional heading is plain text — do **not** wrap it in `**bold**`.

### Code Blocks

Always tag the language:

````markdown
```abap
" ABAP code
```

```javascript
// JavaScript
```

```text
Plain text or ASCII diagrams
```
````

For ABAP / ABAP Cloud variants of the same snippet, use `code-group`:

````markdown
::: code-group

```abap [ABAP]
" Standard ABAP version
```

```abap [ABAP Cloud]
" ABAP Cloud version
```
:::
````

### Linking to Samples

When mentioning a demo class (e.g., `Z2UI5_CL_DEMO_APP_167`), link to the [Samples Index](/resources/samples_index) so readers can find it on GitHub:

```markdown
See sample [`Z2UI5_CL_DEMO_APP_167`](/resources/samples_index) for a complete example.
```

### Internal Links

Use absolute, extensionless paths — no `.md`:

```markdown
[Quickstart](/get_started/quickstart)
```

### Images

- Prefer images checked into `docs/public/` over external URLs.
- Always add descriptive alt text.
- For wide images, use the inline `{ width=60% }` syntax to keep page layout balanced.

### Next Steps

Every guide page benefits from a `## Next Steps` section at the bottom that points readers to the obvious follow-up reading.

## Need Help?

If you hit any issues or have questions, open an [issue](https://github.com/abap2UI5/abap2UI5/issues).
