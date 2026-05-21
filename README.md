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
  ;; Extension preferences
  (grove-default-extension "md")
  (grove-file-extensions '("md" "org"))
  
  ;; Set the Graph renderer (dot, mmdr, or fa2)
  (grove-graph-renderer 'fa2)
  
  ;; ForceAtlas2 specific variables
  (grove-graph-default-zoom 1.0)
  
  ;; Mermaid Settings
  (grove-graph-mmdr-direction "TD")
  
  :config
  ;; NOW we turn on Grove! It will boot up using our Markdown/FA2 patches.
  (global-grove-mode 1))
```
