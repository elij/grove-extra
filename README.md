# Grove Extra

This is an extension for [Grove](https://github.com/jonathanchu/grove).

It requires [graph-fa2](https://github.com/elij/graph-fa2) to render the force directed graph.

https://github.com/user-attachments/assets/6bdd8aac-201b-49d2-82eb-4d555d665437

## Features
- Date format localisation
- Work with other noting schemas in grove (md+frontmatter, denote, org)
- Further graph rendering options
  -  Additional graph engines ([mmdr](https://github.com/1jehuang/mermaid-rs-renderer) and no runtime force directed graph [graph-fa2](https://github.com/elij/graph-fa2))
  - Local graph rendering with `grove-extra-graph-max-distance`
  - Node tag colours with `graph-graph-tag-groups`

## Quick Start

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
