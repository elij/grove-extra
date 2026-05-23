# Grove Extra

This is an extension for [Grove](https://github.com/jonathanchu/grove).

https://github.com/user-attachments/assets/c0070d8c-bfe4-48be-9620-65918107e41a

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
  (grove-graph-mmdr-direction "TD")
  :config
  (global-grove-mode 1)
  (grove-extra-mode 1))
```
