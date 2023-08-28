
(require 'dired-gitignore)
(require 'ls-lisp)

(defmacro fixture-tmp-dir (&rest body)
  `(let ((tmp-dir (make-temp-file "dired-gitignore-test-repo" 'directory))
         (home (getenv "HOME")))
     (unwind-protect
         (progn
           (shell-command-to-string (concat "tar -xf test/test-repo.tar --directory " tmp-dir))
           (dired (concat (file-name-as-directory tmp-dir) "test-repo"))
           (goto-char (point-min))
           (let ((abbreviated-home-dir (concat "\\`" tmp-dir "\\(/\\|\\'\\)" )))
             (setenv "HOME" tmp-dir)
             ,@body))
       (kill-current-buffer)
       (delete-directory tmp-dir 'recursively)
       (setenv "HOME" home)
       (setq dired-mode-hook nil)
       (setq dired-after-readin-hook nil))))

(ert-deftest test-dired-gitignore--add-hook ()
  (add-hook 'dired-mode-hook 'dired-gitignore-mode)
  (fixture-tmp-dir
   (should (eq (count-lines (point-min) (point-max)) 7))
   (dired-gitignore-mode -1)
   (should (eq (count-lines (point-min) (point-max)) 9))))

(ert-deftest test-dired-gitignore--mark-nothing ()
  (fixture-tmp-dir
   (should (eq (dired-get-marked-files) nil))
   (should (eq (count-lines (point-min) (point-max)) 9))))

(ert-deftest test-dired-gitignore--hide--7-entries-remaining ()
  (fixture-tmp-dir
   (dired-gitignore-mode)
   (goto-char (point-min))
   (message "first line %s %s" (thing-at-point 'line t) emacs-version)
   (goto-char (point-max))
   (message "last line %s" (thing-at-point 'line t))
   (message "complete buffer\n%s\n" (buffer-string))
   (message "dired-listing-switches: %s" dired-listing-switches)
   (message "dired-use-ls-dired %s" dired-use-ls-dired)
   (message "dired-free-space %s" dired-free-space)
   (message "ls-lisp-use-insert-directory-program: %s" ls-lisp-use-insert-directory-program)
   (should (eq (count-lines (point-min) (point-max)) 7))))

(ert-deftest test-dired-gitignore--hide--fish ()
  (fixture-tmp-dir
   (let ((shell-file-name "/usr/bin/fish"))
     (dired-gitignore-mode)
     (should (eq (count-lines (point-min) (point-max)) 7)))))

(ert-deftest test-dired-gitignore--hide--hidden-items-not-present ()
  (fixture-tmp-dir
   (dired-gitignore-mode)
   (should (not (string-match-p " to-be-ignored.txt" (buffer-string))))))

(ert-deftest test-dired-gitignore-mode--be-back-at-point-min ()
  (fixture-tmp-dir
   (dired-gitignore-mode)
   (should (eq (point) (point-min)))))

(ert-deftest test-dired-gitignore-mode--no-file-marked ()
  (fixture-tmp-dir
   (dired-gitignore-mode)
   (should (eq (dired-get-marked-files) nil))))

(ert-deftest test-dired-gitignore-mode--get-back-to-earlier-pos ()
  (fixture-tmp-dir
   (let ((marked-file (concat (file-name-as-directory tmp-dir) "test-repo/not-to-be-ignored.txt")))
     (dired-goto-file marked-file)
     (dired-gitignore-mode)
     (should (equal (dired-file-name-at-point) (concat tmp-dir "/test-repo/not-to-be-ignored.txt"))))))

(ert-deftest test-dired-gitignore-mode--file-not-marked-after-hide ()
  (fixture-tmp-dir
   (let ((marked-file (concat (file-name-as-directory tmp-dir) "test-repo/not-to-be-ignored.txt")))
     (dired-goto-file marked-file)
     (dired-gitignore-mode)
     (should (not (string-prefix-p "*" (thing-at-point 'line)))))))

(ert-deftest test-dired-gitignore-mode--dir-not-marked-after-hide ()
  (fixture-tmp-dir
   (let ((marked-file (concat (file-name-as-directory tmp-dir) "test-repo/visible-directory")))
     (dired-goto-file marked-file)
     (dired-gitignore-mode)
     (should (not (string-prefix-p "*" (thing-at-point 'line)))))))

(ert-deftest test-dired-gitignore-mode--marked-after-hide-if-marked-before ()
  (fixture-tmp-dir
   (let ((marked-file (concat (file-name-as-directory tmp-dir) "test-repo/not-to-be-ignored.txt")))
     (dired-goto-file marked-file)
     (dired-mark 1)
     (dired-goto-file marked-file)
     (dired-gitignore-mode)
     (should (string-prefix-p "*" (thing-at-point 'line))))))

(ert-deftest test-dired-gitignore--mark-.cache ()
  (fixture-tmp-dir
   (dired-gitignore--mark-file ".cache")
   (should (equal (dired-get-marked-files)
                  `(,(concat (file-name-as-directory tmp-dir) "test-repo/.cache"))))))

(ert-deftest test-dired-gitignore--mark-non-existant ()
  (fixture-tmp-dir
   (dired-gitignore--mark-file "non-existant-file")
   (should (equal (dired-get-marked-files) nil))))


(ert-deftest test-dired-gitignore--restore-marks-no-ignored-file-marked ()
  (fixture-tmp-dir
   (let ((marked-file (concat (file-name-as-directory tmp-dir) "test-repo/not-to-be-ignored.txt")))
     (dired-goto-file marked-file)
     (dired-mark 1)
     (goto-char (point-min))
     (dired-gitignore-mode)
     (should (equal (dired-get-marked-files) `(,marked-file))))))


(ert-deftest test-dired-gitignore--restore-marks-ignored-file-marked ()
  (fixture-tmp-dir
   (let ((marked-file (concat (file-name-as-directory tmp-dir) "test-repo/to-be-ignored.txt")))
     (dired-goto-file marked-file)
     (dired-mark 1)
     (goto-char (point-min))
     (dired-gitignore-mode)
     (should (equal (dired-get-marked-files) nil)))))
