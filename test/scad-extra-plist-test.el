;;; scad-extra-plist-test.el --- Tests for copying SCAD plists -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with Emacs in batch mode and scad-extra's dependencies on `load-path'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'scad-extra)

(defmacro scad-extra-plist-test--with-buffer (source &rest body)
  "Evaluate BODY in a temporary SCAD syntax buffer containing SOURCE."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (set-syntax-table scad-mode-syntax-table)
     (insert ,source)
     (goto-char (point-min))
     (let ((kill-ring '("previous kill"))
           (kill-ring-yank-pointer nil)
           (interprogram-cut-function nil))
       ,@body)))

(ert-deftest scad-extra-plist-test-selected-pairs ()
  "Copy only the selected pairs from the user's example."
  (scad-extra-plist-test--with-buffer
      (concat "rplidar_c1_plist = [\"size\", [55.6, 55.6],\n"
              "                    // round corner\n"
              "                    \"corner_r\", 4,\n"
              "                    \"color\", matte_black];")
    (search-forward "[\"")
    (let ((beg (1- (point)))
          (end (progn (search-forward "\"corner_r\", 4,") (point))))
      (scad-extra-copy-plist-with-format
       beg end "%k = plist_get(%K, %p);")
      (should (equal (car kill-ring)
                     (concat "size = plist_get(\"size\", plist);\n"
                             "corner_r = plist_get(\"corner_r\", plist);"))))))

(ert-deftest scad-extra-plist-test-nested-and-string-values ()
  "Keep nested plists, calls, commas and comment markers inside values."
  (scad-extra-plist-test--with-buffer
      (concat "\"bolt_head_type\", \"counter,sunk\",\n"
              "\"cable_exit\", [\"side\", \"bottom\", \"position\", \"center\"],\n"
              "\"offset\", concat([1, 2], [3, 4]) + [5, 6],\n"
              "\"label\", \"// a /* string */\",")
    (scad-extra-copy-plist-with-format
     (point-min) (point-max) "%k = plist_get(%K, %p, %v);" "sensor")
    (should
     (equal (car kill-ring)
            (concat "bolt_head_type = plist_get(\"bolt_head_type\", sensor, \"counter,sunk\");\n"
                    "cable_exit = plist_get(\"cable_exit\", sensor, [\"side\", \"bottom\", \"position\", \"center\"]);\n"
                    "offset = plist_get(\"offset\", sensor, concat([1, 2], [3, 4]) + [5, 6]);\n"
                    "label = plist_get(\"label\", sensor, \"// a /* string */\");")))))

(ert-deftest scad-extra-plist-test-comments-and-trailing-comma ()
  "Trim trailing comments without removing comments inside expressions."
  (dolist (ending '("" "," " // no final newline" " /* final comment */"))
    (scad-extra-plist-test--with-buffer
        (concat "/* leading */ \"first\" /* key */, /* value */\n"
                "2 /* inline */ + 3 // trailing line comment\n"
                ", // between pairs\n"
                "\"second\", [1, 2] /* trailing block comment */ ,\n"
                "\"third\", 4" ending)
      (scad-extra-copy-plist-with-format
       (point-min) (point-max) "%k = %v;")
      (should (equal (car kill-ring)
                     "first = 2 /* inline */ + 3;\nsecond = [1, 2];\nthird = 4;")))))

(ert-deftest scad-extra-plist-test-preserves-quoted-key-source ()
  "Use the OpenSCAD spelling of quoted keys in custom format strings."
  (scad-extra-plist-test--with-buffer "\"a\\\"b\", 2, \"path\\\\key\", 3"
    (scad-extra-copy-plist-with-format
     (point-min) (point-max) "plist_get(%K, %p, %v)" "sensor")
    (should (equal (car kill-ring)
                   "plist_get(\"a\\\"b\", sensor, 2)\nplist_get(\"path\\\\key\", sensor, 3)"))))

(ert-deftest scad-extra-plist-test-bracketed-selection ()
  "Accept an entire list with surrounding comments and a trailing comma."
  (scad-extra-plist-test--with-buffer
      "// leading\n[\"size\", [1, 2], /* next */ \"corner_r\", 4,] // trailing"
    (scad-extra-copy-plist-with-format
     (point-min) (point-max) "%k = plist_get(%K, %p);")
    (should (equal (car kill-ring)
                   "size = plist_get(\"size\", plist);\ncorner_r = plist_get(\"corner_r\", plist);"))))

(ert-deftest scad-extra-plist-test-invalid-selection-preserves-kill-ring ()
  "Reject missing pairs, separators and unbalanced values before copying."
  (dolist (source '("" "[]" "// comment only" "\"size\"" "\"size\","
                    "\"size\", // comment only" "\"size\", /* comment only */"
                    "size, 2" "\"size\" 2" "\"size\", [1, 2"
                    "\"size\", \"unterminated" "\"size\", 2, \"corner_r\""
                    "\"size\", 2,, \"corner_r\", 4"
                    "\"size\", 2; \"corner_r\", 4"
                    "[\"size\", 2] unrelated"))
    (scad-extra-plist-test--with-buffer source
      (should-error
       (scad-extra-copy-plist-with-format
        (point-min) (point-max) "%k = plist_get(%K, %p);")
       :type 'user-error)
      (should (equal kill-ring '("previous kill"))))))

(ert-deftest scad-extra-plist-test-respects-selection-end ()
  "Do not complete a selected key or vector using text outside the region."
  (dolist (selected-end '("\"size\"," "[1,"))
    (scad-extra-plist-test--with-buffer "\"size\", [1, 2], \"corner_r\", 4"
      (search-forward selected-end)
      (let ((end (point))
            (original-max (point-max)))
        (should-error
         (scad-extra-copy-plist-with-format
          (point-min) end "%k = plist_get(%K, %p);")
         :type 'user-error)
        (should (= (point) end))
        (should (= (point-max) original-max))
        (should (equal kill-ring '("previous kill")))))))

(ert-deftest scad-extra-plist-test-interactive-preview-and-prompt-order ()
  "Preview variants, remove the preview, then read the plist variable name."
  (scad-extra-plist-test--with-buffer "\"size\", [55.6, 55.6],\n\"corner_r\", 4,"
    (let* ((transient-mark-mode t)
           (scad-extra-plist-copy-formats
            '("%k = plist_get(%K, %p);" "%k = plist_get(%K, %p, %v);"))
           (original-overlay (make-overlay (point-min) (1+ (point-min))))
           calls original)
      (add-text-properties (point-min) (1+ (point-min)) '(help-echo "existing"))
      (setq original (buffer-substring (point-min) (point-max)))
      (set-buffer-modified-p nil)
      (setq buffer-undo-list nil)
      (set-mark (point-max))
      (setq mark-active t)
      (cl-letf (((symbol-function 'scad-extra--completing-read-with-preview-action)
                 (lambda (prompt collection preview-action &rest _)
                   (should (equal prompt "Formatter: "))
                   (should (equal collection scad-extra-plist-copy-formats))
                   (push 'formatter calls)
                   (let ((preview (car (cl-remove original-overlay
                                                  (overlays-in (point-min) (point-max))))))
                     (should (equal (overlay-get preview 'display)
                                    "size = plist_get(\"size\", plist);\ncorner_r = plist_get(\"corner_r\", plist);"))
                     (funcall preview-action (cadr collection))
                     (should (equal (overlay-get preview 'display)
                                    "size = plist_get(\"size\", plist, [55.6, 55.6]);\ncorner_r = plist_get(\"corner_r\", plist, 4);")))
                   (car collection)))
                ((symbol-function 'read-string)
                 (lambda (prompt _initial history default &rest _)
                   (should (equal calls '(formatter)))
                   (should (equal prompt "Plist variable: "))
                   (should (eq history 'scad-extra--plist-name-history))
                   (should (equal default "plist"))
                   (should (equal (overlays-in (point-min) (point-max))
                                  (list original-overlay)))
                   (push 'variable calls)
                   "rplidar_c1_plist")))
        (call-interactively #'scad-extra-copy-plist-with-format))
      (should (equal calls '(variable formatter)))
      (should (equal (car kill-ring)
                     "size = plist_get(\"size\", rplidar_c1_plist);\ncorner_r = plist_get(\"corner_r\", rplidar_c1_plist);"))
      (should (equal-including-properties original
                                           (buffer-substring (point-min) (point-max))))
      (should-not (buffer-modified-p))
      (should-not buffer-undo-list)
      (should (equal (overlays-in (point-min) (point-max))
                     (list original-overlay))))))

(ert-deftest scad-extra-plist-test-interactive-quit-cleans-preview ()
  "Quitting either prompt preserves source properties, overlays and kills."
  (dolist (quit-stage '(formatter variable))
    (scad-extra-plist-test--with-buffer "\"size\", [1, 2],"
      (let ((transient-mark-mode t)
            original quit-caught)
        (add-text-properties (point-min) (1+ (point-min)) '(help-echo "existing"))
        (setq original (buffer-substring (point-min) (point-max)))
        (set-buffer-modified-p nil)
        (setq buffer-undo-list nil)
        (set-mark (point-max))
        (setq mark-active t)
        (cl-letf (((symbol-function 'scad-extra--completing-read-with-preview-action)
                   (lambda (_prompt collection preview-action &rest _)
                     (funcall preview-action (car collection))
                     (when (eq quit-stage 'formatter)
                       (signal 'quit nil))
                     (car collection)))
                  ((symbol-function 'read-string)
                   (lambda (&rest _)
                     (should (eq quit-stage 'variable))
                     (should-not (overlays-in (point-min) (point-max)))
                     (signal 'quit nil))))
          (condition-case nil
              (call-interactively #'scad-extra-copy-plist-with-format)
            (quit (setq quit-caught t))))
        (should quit-caught)
        (should (equal-including-properties original
                                             (buffer-substring (point-min) (point-max))))
        (should-not (buffer-modified-p))
        (should-not buffer-undo-list)
        (should-not (overlays-in (point-min) (point-max)))
        (should (equal kill-ring '("previous kill")))))))

(ert-deftest scad-extra-plist-test-interactive-opening-bracket ()
  "Without a region, select the list at point and use the default variable."
  (scad-extra-plist-test--with-buffer "sensor = [\"size\", [1, 2]]; unrelated = 3;"
    (search-forward "[")
    (backward-char)
    (let ((transient-mark-mode t)
          (mark-active nil))
      (cl-letf (((symbol-function 'scad-extra--completing-read-with-preview-action)
                 (lambda (_prompt collection &rest _) (car collection)))
                ((symbol-function 'read-string)
                 (lambda (_prompt _initial _history default &rest _) default)))
        (call-interactively #'scad-extra-copy-plist-with-format))
      (should (equal (car kill-ring) "size = plist_get(\"size\", plist);"))
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest scad-extra-plist-test-preview-follows-candidate-navigation ()
  "Refresh previews for candidate changes even when input text is unchanged."
  (with-temp-buffer
    (setq-local post-command-hook nil)
    (let ((minibuffer-setup-hook nil)
          current seen)
      (cl-letf (((symbol-function 'minibufferp) (lambda (&optional _) t))
                ((symbol-function 'minibuffer-selected-window)
                 (lambda () (selected-window)))
                ((symbol-function 'scad-extra--minibuffer-current-candidate)
                 (lambda () (cons nil current)))
                ((symbol-function 'completing-read)
                 (lambda (&rest _)
                   (run-hooks 'minibuffer-setup-hook)
                   (setq current "first")
                   (run-hooks 'post-command-hook)
                   (run-hooks 'post-command-hook)
                   (setq current "second")
                   (run-hooks 'post-command-hook)
                   (setq current nil)
                   (run-hooks 'post-command-hook)
                   "second")))
        (should (equal
                 (scad-extra--completing-read-with-preview-action
                  "Formatter: " '("first" "second")
                  (lambda (candidate) (push candidate seen)))
                 "second")))
      (should (equal (nreverse seen) '("first" "second")))
      (should (equal (buffer-string) "")))))

(provide 'scad-extra-plist-test)
;;; scad-extra-plist-test.el ends here
