;; Built-in project.el (replaces projectile with zero external dependencies)
(use-package project
  :ensure nil
  :bind-keymap ("C-c p" . project-prefix-map)
  :config
  (setq project-vc-extra-root-markers '("Cargo.toml" "package.json" "go.mod" "WORKSPACE" "MODULE.bazel"))
  (setq project-list-file (expand-file-name "projects" var-dir)))

