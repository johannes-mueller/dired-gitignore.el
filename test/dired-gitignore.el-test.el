
(require 'dired-gitignore)

(defun fixture-tmp-dir (body)
  (let ((tmp-dir (make-temp-file "dired-gitignore-test-repo" 'directory))
        (home (getenv "HOME")))
    (unwind-protect
        (progn
          (shell-command-to-string (concat "tar -xf test/test-repo.tar --directory " tmp-dir))
          (dired (concat (file-name-as-directory tmp-dir) "test-repo"))
          (goto-char (point-min))
          (let ((abbreviated-home-dir (concat "\\`" tmp-dir "\\(/\\|\\'\\)" )))
            (setenv "HOME" tmp-dir)
            (funcall body)))
      (kill-current-buffer)
      (delete-directory tmp-dir 'recursively)
      (setenv "HOME" home)
      (setq dired-mode-hook nil)
      (setq dired-after-readin-hook nil))))

(ert-deftest test-dired-gitignore--add-hook ()
  (add-hook 'dired-mode-hook 'dired-gitignore-mode)
  (fixture-tmp-dir
   (lambda ()
     (should (eq (count-lines (point-min) (point-max)) 8))
     (dired-gitignore-mode -1)
     (should (eq (count-lines (point-min) (point-max)) 10)))))

(ert-deftest test-dired-gitignore--mark-nothing ()
  (fixture-tmp-dir
   (lambda ()
     (should (eq (dired-get-marked-files) nil))
     (should (eq (count-lines (point-min) (point-max)) 10)))))

(ert-deftest test-dired-gitignore--hide--8-entries-remaining ()
  (fixture-tmp-dir
   (lambda ()
     (dired-gitignore-mode)
     (should (eq (count-lines (point-min) (point-max)) 8)))))

(ert-deftest test-dired-gitignore--hide--hidden-items-not-present ()
  (fixture-tmp-dir
   (lambda ()
     (dired-gitignore-mode)
     (should (not (string-match-p " to-be-ignored.txt" (buffer-string)))))))

(ert-deftest test-dired-gitignore-mode--be-back-at-point-min ()
  (fixture-tmp-dir
   (lambda ()
     (dired-gitignore-mode)
     (should (eq (point) (point-min))))))

(ert-deftest test-dired-gitignore-mode--no-file-marked ()
  (fixture-tmp-dir
   (lambda ()
     (dired-gitignore-mode)
     (should (eq (dired-get-marked-files) nil)))))

(ert-deftest test-dired-gitignore-mode--get-back-to-earlier-pos ()
  (fixture-tmp-dir
   (lambda ()
     (let ((marked-file (concat (file-name-as-directory tmp-dir) "test-repo/not-to-be-ignored.txt")))
       (dired-goto-file marked-file)
       (dired-gitignore-mode)
       (should (equal (dired-file-name-at-point) (concat tmp-dir "/test-repo/not-to-be-ignored.txt")))))))

(ert-deftest test-dired-gitignore-mode--file-not-marked-after-hide ()
  (fixture-tmp-dir
   (lambda ()
     (let ((marked-file (concat (file-name-as-directory tmp-dir) "test-repo/not-to-be-ignored.txt")))
       (dired-goto-file marked-file)
       (dired-gitignore-mode)
       (should (not (string-prefix-p "*" (thing-at-point 'line))))))))

(ert-deftest test-dired-gitignore-mode--dir-not-marked-after-hide ()
  (fixture-tmp-dir
   (lambda ()
     (let ((marked-file (concat (file-name-as-directory tmp-dir) "test-repo/visible-directory")))
       (dired-goto-file marked-file)
       (dired-gitignore-mode)
       (should (not (string-prefix-p "*" (thing-at-point 'line))))))))

(ert-deftest test-dired-gitignore-mode--marked-after-hide-if-marked-before ()
  (fixture-tmp-dir
   (lambda ()
     (let ((marked-file (concat (file-name-as-directory tmp-dir) "test-repo/not-to-be-ignored.txt")))
       (dired-goto-file marked-file)
       (dired-mark 1)
       (dired-goto-file marked-file)
       (dired-gitignore-mode)
       (should (string-prefix-p "*" (thing-at-point 'line)))))))

(ert-deftest test-dired-gitignore--mark-.cache ()
  (fixture-tmp-dir
   (lambda ()
     (dired-gitignore--mark-file ".cache")
     (should (equal (dired-get-marked-files)
                    `(,(concat (file-name-as-directory tmp-dir) "test-repo/.cache")))))))

(ert-deftest test-dired-gitignore--mark-non-existant ()
  (fixture-tmp-dir
   (lambda ()
     (dired-gitignore--mark-file "non-existant-file")
     (should (equal (dired-get-marked-files) nil)))))


(ert-deftest test-dired-gitignore--restore-marks-no-ignored-file-marked ()
  (fixture-tmp-dir
   (lambda ()
     (let ((marked-file (concat (file-name-as-directory tmp-dir) "test-repo/not-to-be-ignored.txt")))
       (dired-goto-file marked-file)
       (dired-mark 1)
       (goto-char (point-min))
       (dired-gitignore-mode)
       (should (equal (dired-get-marked-files) `(,marked-file)))))))


(ert-deftest test-dired-gitignore--restore-marks-ignored-file-marked ()
  (fixture-tmp-dir
   (lambda ()
     (let ((marked-file (concat (file-name-as-directory tmp-dir) "test-repo/to-be-ignored.txt")))
       (dired-goto-file marked-file)
       (dired-mark 1)
       (goto-char (point-min))
       (dired-gitignore-mode)
       (should (equal (dired-get-marked-files) nil))))))
