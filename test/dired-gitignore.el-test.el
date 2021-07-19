
(require 'dired-gitignore)

(defun fixture-tmp-dir (body)
  (let ((tmp-dir (make-temp-file "dired-gitignore-test-repo" 'directory)))
    (unwind-protect
       (progn
	 (shell-command-to-string (concat "tar -xf test/test-repo.tar --directory " tmp-dir))
	 (dired (concat (file-name-as-directory tmp-dir) "test-repo"))
	 (goto-char (point-min))
	 (funcall body))
      (kill-current-buffer)
      (delete-directory tmp-dir 'recursively))))

(ert-deftest test-dired-gitignore--mark-nothing ()
  (fixture-tmp-dir
   (lambda ()
     (should (eq (dired-get-marked-files) nil))
     (should (eq (count-lines (point-min) (point-max)) 9)))))

(ert-deftest test-dired-gitignore--hide--7-entries-remaining ()
  (fixture-tmp-dir
   (lambda ()
     (dired-gitignore--hide)
     (should (eq (count-lines (point-min) (point-max)) 7)))))

(ert-deftest test-dired-gitignore--hide--hidden-items-not-present ()
  (fixture-tmp-dir
   (lambda ()
     (dired-gitignore--hide)
     (should (not (string-match-p " to-be-ignored.txt" (buffer-string)))))))

(ert-deftest test-dired-gitignore--hide--be-back-at-point-min ()
  (fixture-tmp-dir
   (lambda ()
     (dired-gitignore--hide)
     (should (eq (point) (point-min))))))

(ert-deftest test-dired-gitignore--hide--no-file-marked ()
  (fixture-tmp-dir
   (lambda ()
     (dired-gitignore--hide)
     (should (eq (dired-get-marked-files) nil)))))

(ert-deftest test-dired-gitignore--hide--get-back-to-earlier-pos ()
  (fixture-tmp-dir
   (lambda ()
     (let ((marked-file (concat (file-name-as-directory tmp-dir) "test-repo/not-to-be-ignored.txt")))
       (dired-goto-file marked-file)
       (should (not (equal (point) (point-min))))
       (dired-gitignore--hide)
       (should (equal (dired-get-marked-files) `(,marked-file)))))))

(ert-deftest test-dired-gitignore--mark-.cask ()
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
