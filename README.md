# Link to Remote

An Emacs package to link the current file to its remote repository URL with branch selection and multiple actions.

## Features

- Interactive branch selection via `completing-read`
- Multiple actions via a Transient menu:
  - Open link in browser
  - Echo link to message buffer
  - Copy link to kill ring
  - Copy plain link (without line numbers) to kill ring
- Support for line numbers and line ranges (when region is active)
- Works with GitHub, GitLab, and Bitbucket repositories
- Cross-platform browser support via `browse-url`

## Installation

### Manual

1. Clone this repo.
2. Add to your `load-path`:
   ```emacs-lisp
   (add-to-list 'load-path "~/path/to/link-to-remote")
   (require 'link-to-remote)
   ```

### With `use-package`

```emacs-lisp
(use-package link-to-remote
  :ensure nil  ; until available on MELPA
  :load-path "~/path/to/link-to-remote"
  :bind ("C-c l" . link-to-remote))
```

## Usage

1. `M-x link-to-remote` opens a Transient menu.
2. Choose an action:
   - `o`: Open link in browser
   - `e`: Echo link to messages
   - `k`: Copy link to kill ring
   - `p`: Copy plain link (without line numbers) to kill ring
3. Select a branch when prompted (defaults to current branch)

## Examples

- If you have a region selected, the link will include the line range (e.g., `#L10-L15`)
- Otherwise, it links to the current line (e.g., `#L42`)

## Dependencies

- Emacs 26.1+
- Magit 3.0+
- Transient 0.3.0+

## Development

### Running Tests

```sh
emacs -Q -batch -L . -l ert -l link-to-remote.el -l link-to-remote-test.el -f ert-run-tests-batch-and-exit
```

### Contributing

Contributions are welcome! Please feel free to open an issue or submit a pull request.

## License

This project is licensed under the GPL-3.0 License - see the LICENSE file for details.