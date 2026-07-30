# Grove Extra

This is an extension for [Grove](https://github.com/jonathanchu/grove).

It requires [graph-fa2](https://github.com/elij/graph-fa2) to render the force directed graph.

https://github.com/user-attachments/assets/6bdd8aac-201b-49d2-82eb-4d555d665437

## Features

- Date format localisation
- Integration with other note formats (Markdown with YAML frontmatter, Denote, Org)
- Speedbar integration for browsing the file tree
- Extended graph rendering engines (Mermaid via `mmdr` and animated physics via `graph-fa2`)
- Localised graph view centered on active buffer using `grove-graph-max-distance`
- Tag-based node colouring using `grove-graph-tag-groups`
- Per note sidebar (imenu outline, tags, links and backlinks) as a speedbar display mode

<img alt="speedbar-display-modes" src="https://github.com/user-attachments/assets/ea2f3b04-7702-4c6f-97da-192ddceaf2bb" />


## Quick start

```elisp
(use-package graph-fa2)

(use-package grove
  :bind-keymap ("C-c v" . grove-command-map)
  :custom
  (grove-directory "~/")
  (grove-tree-icons t))

(use-package grove-extra
  :after grove
  :demand t
  :custom
  (grove-default-extension "md")
  (grove-file-extensions '("md" "org"))
  (grove-graph-renderer 'fa2)
  (grove-graph-default-zoom 1.0)
  (grove-graph-max-distance 2)
  (grove-graph-tag-groups '(("concept" . "#a6e3a1")
                            ("person"  . "#f38ba8")))
  (grove-graph-mmdr-direction "TD")
  :config
  (global-grove-mode 1)
  (grove-extra-mode 1))
```

## Keymaps and hooks

- `grove-extra-graph-mode`: Minor mode for interactive graph buffer features and hover hooks.
- `grove-extra-capture-mode`: Minor mode for capture buffer setup, preserving keymaps by running after major mode initialisation.
- `grove-extra-node-hover-functions`: Hook executed when hovering over nodes in graph views.

