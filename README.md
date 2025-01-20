# ftag-mode

`ftag-mode` is an Emacs major mode for editing `.ftag` files (See
[this project](https://github.com/ranjeethmahankali/ftag) to
understand what `.ftag` files are). It provides the following
features:

- Basic syntax highlighting.
- Autocompletion for filepaths and tags.
- Command to preview the file at point: `ftag-preview-file`. Bound to `C-c C-c`
  by default.
- Minor mode to always preview the file at point as you move around
  the buffer: `ftag-follow-mode`. Bound to `C-c C-f` by default.
- Open the file at point in the default application with:
  `ftag-open-file-at-point`, bound to `C-c C-o` by default.
- Command to quickly track all untracked files in the directory:
  `ftag-track-untracked-files`.
