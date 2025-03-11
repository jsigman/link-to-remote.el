;;; link-to-remote.el --- Link current file to its remote repository -*- lexical-binding: t; -*-

;; Author: John Sigman
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (magit "3.0") (transient "0.3.0") (cl-lib "0.5"))
;; Keywords: tools, git, convenience
;; URL: https://github.com/jsigman/link-to-remote

;;; Commentary:
;; This package provides an interactive way to link the current file to its
;; remote repository URL, with branch selection and action choices via Transient.

;;; Code:

(require 'magit)
(require 'transient)
(require 'browse-url)

(defun link-to-remote--get-repo-url ()
  "Get the repository's remote URL in web-friendly format."
  (let ((remote (magit-get "remote" "origin" "url")))
    (when remote
      (cond
       ((string-match "git@github.com:\\(.*\\)\\.git" remote)
        (format "https://github.com/%s" (match-string 1 remote)))
       ((string-match "https://github.com/\\(.*\\)\\.git" remote)
        (format "https://github.com/%s" (match-string 1 remote)))
       ((string-match "git@gitlab.com:\\(.*\\)\\.git" remote)
        (format "https://gitlab.com/%s" (match-string 1 remote)))
       ((string-match "https://gitlab.com/\\(.*\\)\\.git" remote)
        (format "https://gitlab.com/%s" (match-string 1 remote)))
       ((string-match "git@bitbucket.org:\\(.*\\)\\.git" remote)
        (format "https://bitbucket.org/%s" (match-string 1 remote)))
       ((string-match "https://bitbucket.org/\\(.*\\)\\.git" remote)
        (format "https://bitbucket.org/%s" (match-string 1 remote)))
       (t
        (error "Unsupported remote URL format: %s" remote))))))

(defun link-to-remote--get-file-path ()
  "Get the relative file path from the repository root."
  (let ((root (magit-toplevel))
        (file (buffer-file-name)))
    (if (and root file)
        (file-relative-name file root)
      (error "Not in a Git repository or no file visited"))))

(defun link-to-remote--get-branch ()
  "Prompt for a branch to link to, defaulting to the current branch."
  (let ((branches (magit-list-branch-names))
        (current-branch (magit-get-current-branch)))
    (completing-read "Select branch: " branches
                     nil
                     t
                     current-branch)))

(defun link-to-remote--get-line-info ()
  "Get line number or range if region is active."
  (if (use-region-p)
      (let ((start (line-number-at-pos (region-beginning)))
            (end (line-number-at-pos (region-end))))
        (if (= start end)
            (format "#L%d" start)
          (format "#L%d-L%d" start end)))
    (format "#L%d" (line-number-at-pos))))

(defun link-to-remote--build-url (branch &optional without-line)
  "Build the full remote URL for the current file and given BRANCH.
If WITHOUT-LINE is non-nil, don't include line number information."
  (let* ((repo-url (link-to-remote--get-repo-url))
         (file-path (link-to-remote--get-file-path))
         (line-info
          (unless without-line
            (link-to-remote--get-line-info))))
    (format "%s/blob/%s/%s%s"
            repo-url
            branch
            file-path
            (or line-info ""))))

;;;###autoload
(transient-define-prefix
 link-to-remote
 ()
 "Open or manage a link to the current file on its remote repository."
 ["Link to Remote"
  ("o" "Open in browser" link-to-remote-open)
  ("e" "Echo to messages" link-to-remote-echo)
  ("k" "Copy to kill ring" link-to-remote-kill)
  ("p" "Copy plain URL (no line numbers)" link-to-remote-kill-plain)])

(defun link-to-remote--dispatch (action)
  "Dispatch ACTION on the constructed URL with branch selection."
  (let* ((branch (link-to-remote--get-branch))
         (without-line (eq action 'plain))
         (url (link-to-remote--build-url branch without-line)))
    (pcase action
      ('open (browse-url url))
      ('echo (message "Link: %s" url))
      ('kill
       (kill-new url) (message "Link copied to kill ring: %s" url))
      ('plain
       (kill-new url)
       (message "Plain link copied to kill ring: %s" url))
      (_ (error "Unknown action: %s" action)))))

;;;###autoload
(defun link-to-remote-open ()
  "Open the current file in the default browser."
  (interactive)
  (link-to-remote--dispatch 'open))

;;;###autoload
(defun link-to-remote-echo ()
  "Echo the current file's remote URL to the message buffer."
  (interactive)
  (link-to-remote--dispatch 'echo))

;;;###autoload
(defun link-to-remote-kill ()
  "Copy the current file's remote URL to the kill ring."
  (interactive)
  (link-to-remote--dispatch 'kill))

;;;###autoload
(defun link-to-remote-kill-plain ()
  "Copy the current file's remote URL without line info to the kill ring."
  (interactive)
  (link-to-remote--dispatch 'plain))

(provide 'link-to-remote)
;;; link-to-remote.el ends here
