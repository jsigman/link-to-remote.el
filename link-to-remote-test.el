;;; link-to-remote-test.el --- Tests for link-to-remote -*- lexical-binding: t; -*-

(require 'ert)
(require 'link-to-remote)

(ert-deftest link-to-remote--get-repo-url-github-ssh-test ()
  "Test GitHub SSH repository URL parsing."
  (cl-letf (((symbol-function 'magit-get)
             (lambda (&rest _) "git@github.com:user/repo.git")))
    (should
     (string=
      (link-to-remote--get-repo-url)
      "https://github.com/user/repo"))))

(ert-deftest link-to-remote--get-repo-url-github-https-test ()
  "Test GitHub HTTPS repository URL parsing."
  (cl-letf (((symbol-function 'magit-get)
             (lambda (&rest _) "https://github.com/user/repo.git")))
    (should
     (string=
      (link-to-remote--get-repo-url)
      "https://github.com/user/repo"))))

(ert-deftest link-to-remote--get-repo-url-gitlab-ssh-test ()
  "Test GitLab SSH repository URL parsing."
  (cl-letf (((symbol-function 'magit-get)
             (lambda (&rest _) "git@gitlab.com:user/repo.git")))
    (should
     (string=
      (link-to-remote--get-repo-url)
      "https://gitlab.com/user/repo"))))

(ert-deftest link-to-remote--get-repo-url-gitlab-https-test ()
  "Test GitLab HTTPS repository URL parsing."
  (cl-letf (((symbol-function 'magit-get)
             (lambda (&rest _) "https://gitlab.com/user/repo.git")))
    (should
     (string=
      (link-to-remote--get-repo-url)
      "https://gitlab.com/user/repo"))))

(ert-deftest link-to-remote--build-url-with-line-test ()
  "Test URL building with a mock branch and line number."
  (cl-letf (((symbol-function 'magit-get)
             (lambda (&rest _) "git@github.com:user/repo.git"))
            ((symbol-function 'buffer-file-name)
             (lambda () "/path/repo/file.el"))
            ((symbol-function 'magit-toplevel)
             (lambda () "/path/repo"))
            ((symbol-function 'line-number-at-pos)
             (lambda (&rest _) 42))
            ((symbol-function 'use-region-p) (lambda () nil)))
    (should
     (string=
      (link-to-remote--build-url "main")
      "https://github.com/user/repo/blob/main/file.el#L42"))))

(ert-deftest link-to-remote--build-url-without-line-test ()
  "Test URL building with a mock branch without line number."
  (cl-letf (((symbol-function 'magit-get)
             (lambda (&rest _) "git@github.com:user/repo.git"))
            ((symbol-function 'buffer-file-name)
             (lambda () "/path/repo/file.el"))
            ((symbol-function 'magit-toplevel)
             (lambda () "/path/repo")))
    (should
     (string=
      (link-to-remote--build-url "main" t)
      "https://github.com/user/repo/blob/main/file.el"))))

(ert-deftest link-to-remote--get-line-info-no-region-test ()
  "Test getting line info without an active region."
  (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
            ((symbol-function 'line-number-at-pos)
             (lambda (&rest _) 42)))
    (should (string= (link-to-remote--get-line-info) "#L42"))))

(ert-deftest link-to-remote--get-line-info-with-region-test ()
  "Test getting line info with an active region spanning multiple lines."
  (cl-letf (((symbol-function 'use-region-p) (lambda () t))
            ((symbol-function 'region-beginning) (lambda () 100))
            ((symbol-function 'region-end) (lambda () 200))
            ((symbol-function 'line-number-at-pos)
             (lambda (pos)
               (if (= pos 100)
                   10
                 15))))
    (should (string= (link-to-remote--get-line-info) "#L10-L15"))))

(ert-deftest
    link-to-remote--get-line-info-with-single-line-region-test
    ()
  "Test getting line info with an active region on a single line."
  (cl-letf (((symbol-function 'use-region-p) (lambda () t))
            ((symbol-function 'region-beginning) (lambda () 100))
            ((symbol-function 'region-end) (lambda () 110))
            ((symbol-function 'line-number-at-pos) (lambda (pos) 42)))
    (should (string= (link-to-remote--get-line-info) "#L42"))))

(ert-deftest link-to-remote-toggle-line-numbers-test ()
  "Test toggling the line numbers flag."
  (let ((link-to-remote-include-line-numbers t))
    (link-to-remote-toggle-line-numbers)
    (should-not link-to-remote-include-line-numbers)
    (link-to-remote-toggle-line-numbers)
    (should link-to-remote-include-line-numbers)))

(provide 'link-to-remote-test)
