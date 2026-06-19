# Grove Extra

This is an extension for [Grove](https://github.com/jonathanchu/grove).

https://github.com/user-attachments/assets/6bdd8aac-201b-49d2-82eb-4d555d665437

Allows working in md/org, and implements mmdr support and built in fa2 rendering.

```elisp
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

### New Features

- **Local Graph Support**: Pass a numerical prefix argument to `grove-graph` or set `grove-graph-max-distance` to render a subset of the graph surrounding the current buffer. Useful for large workspaces.
- **Node Tag Colours**: Set `grove-graph-tag-groups` with an alist (e.g. `(("tag" . "#hex"))`) to conditionally colour specific nodes if they possess that tag. Currently applied within the physics engine (fa2).
