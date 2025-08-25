;;; scad-extra.el --- Additional commands for scad-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/scad-extra
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "29.1") (scad-mode "96.0") (project "0.11.1") (transient "0.9.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides miscellaneous additional commands for `scad-mode'
;; and `scad-preview-mode'.
;;
;; Main commands for `scad-mode':
;;
;; - `scad-extra-import-project-file': Insert a "use" directive for the given
;;   project file. With a prefix argument, insert an "include" directive.
;; - `scad-extra-enable-flymake-check-unused-vars': Adds a flymake backend for
;;   checking unused variables defined inside modules.
;; - `scad-extra-find-unused-variables-in-file': Show unused top-level
;;   variables in the file.
;; - `scad-extra-find-unused-variables-in-project': Show unused top-level
;;   variables in the project.
;; - `scad-extra-rename-symbol': Rename occurrences of a symbol across the
;;   entire project.
;; - `scad-extra-comment-dwim': With a prefix argument, use block comment
;;   style; without a prefix argument, use the standard (line) comment style.
;; - `scad-extra-reload-preview-mode': Toggle automatic reloading of related
;;   SCAD preview buffers on save. This is useful, for example, when you have a
;;   separate file that defines variables/parameters and another file open in a
;;   different window whose preview depends on those parameters: `scad-preview-mode'
;;   will normally refresh a preview buffer only when the buffer's own file is
;;   edited, but enabling this mode causes the preview to refresh when an
;;   imported file is saved.
;;
;; Commands for `scad-preview-mode' include additional commands for
;; screen-aligned translation, commands that reposition the preview camera to
;; display standard views (top, bottom, left, right, front, back), and a
;; transient menu (`scad-extra-menu').

;;; Code:

(require 'subr-x)
(require 'scad-mode)
(require 'project)
(require 'transient)

(defcustom scad-extra-default-export-directory
  'scad-extra-project-export-directory
  "Directory path or function for exporting SCAD files.

Specifies the default directory for exporting SCAD files.

The value can be a directory path as a string, a function that
returns a directory path, or nil to use the default directory.

If set to a function, the function should return a string
representing the directory path.

When exporting, the specified directory will be used unless
overridden by user input.

This allows for flexible export configurations based on project
requirements or user preferences."
  :group 'scad-extra
  :type '(radio
          (function-item scad-extra-project-export-directory)
          (directory :tag "Directory")
          (const :tag "None (use default directory)" nil)
          (function :tag "Custom function")))

(defcustom scad-extra-allow-reverse t
  "Allow cycling of view commands.

This setting applies to the following pairs of commands:
- `scad-extra-top-view'  <->  `scad-extra-bottom-view'
- `scad-extra-left-view' <->  `scad-extra-right-view'
- `scad-extra-front-view' <->  `scad-extra-back-view'

For example, if you call `scad-extra-top-view' while the coordinates already
represent a top view,the function will invoke its reverse command
\(`scad-extra-bottom-view') and vice versa."
  :group 'scad-extra
  :type 'boolean)

(defcustom scad-extra-allowed-unused-variables '("$fa" "$fs" "$fn" "$t" "$vpr"
                                                 "$vpt" "$vpd" "$vpf"
                                                 "$preview")
  "List of variable names that the unused-variable analysis ignores.

Each element is a string naming a variable, such as
\"$fa\" or \"$preview\".

Names are matched literally and case-sensitively.

Applies only to variables detected in module bodies; parameters
are not affected.

Can be customized to include project-specific names that are safe to
leave unused.

Defaults include common OpenSCAD special variables and viewer
settings."
  :group 'scad-extra
  :type '(repeat (string)))

(defcustom scad-extra-comment-dwim-default-style '((nil
                                                    (comment-start . "// ")
                                                    (comment-end . "")
                                                    (comment-padding . " ")
                                                    (comment-style . indent)
                                                    (comment-continue . nil)
                                                    (comment-multi-line . t))
                                                   ((4 16)
                                                    (comment-start . "/** ")
                                                    (comment-end . " */")
                                                    (comment-padding . " ")
                                                    (comment-style . extra-line)
                                                    (comment-continue . " * ")
                                                    (comment-multi-line . t)))
  "Default comment styles for different prefix arguments.

Defines the default comment styles for use with the `scad-extra-comment-dwim'
function.

The value is a list of pairs, where each pair consists of a list of prefix
arguments and a set of comment style properties.

The prefix arguments list can be empty, indicating the default style, or contain
integers specifying the prefix argument values that trigger the associated
style. Each set of comment style properties includes keys such as
`comment-start', `comment-end', `comment-padding', `comment-style',
`comment-continue', and `comment-multi-line'. These properties determine the
appearance and behavior of comments, such as the strings used to start and end
comments, padding around comments, the style of comments, continuation strings
for multi-line comments, and whether multi-line comments are supported."
  :group 'scad-extra
  :type `(repeat
          (cons
           (repeat :tag "Prefix args (empty for default)"
            (integer))
           (set
            (cons :tag "Multi Line" (const comment-multi-line)
             (boolean :value t))
            (cons :tag "Comment Style"
             (const comment-style)
             (radio ,@(mapcar (lambda (s)
                                `(const :tag ,(format "%s: %s" (car s)
                                               (nth 5 s))
                                  ,(car s)))
                       comment-styles)))
            (cons :tag "Comment Continue"
             (const comment-continue)
             (radio
              (const :tag "None" nil)
              string))
            (cons :tag "Comment Start"
             (const comment-start)
             (string :value "/** "))
            (cons :tag "Comment End" (const comment-end)
             (string :value "*/ "))
            (cons :tag "Comment Padding"
             (const comment-padding)
             (choice :value " "
              (string :value " ")
              (integer)
              (const nil)))))))

(defcustom scad-extra-saveable-variables '(scad-preview-projection
                                           scad-preview-camera
                                           scad-preview-view)
  "List of scad-related variables eligible for saving.

This includes variables that can be saved using the command
`scad-extra-save-variables'."
  :group 'scad-extra
  :type '(set :greedy t
          (const scad-preview-view)
          (const scad-preview-projection)
          (const scad-preview-camera)
          (repeat :inline t
           (symbol))))

(defconst scad-extra--import-regexp
  "\\_<\\(include\\|use\\)\\_>[\s]*<\\([.A-Za-z_/][^>]*\\)>"
  "Regular expression matching SCAD import statements.")

(defconst scad-extra--variable-regex
  "^[[:space:]]*\\([0-9A-Z_a-z]*\\)[[:space:]]*\\(=\\)[[:space:]]*\\([^=][^;]+;\\)"
  "Regex pattern matching SCAD variable declarations.")

(defconst scad-extra--comment-start-re "//\\|/\\*")

(defmacro scad-extra--with-temp-buffer (file &rest body)
  "Execute BODY in a temporary buffer containing FILE's contents in SCAD mode.

Argument FILE specifies the file to be loaded into the temporary buffer.

Remaining arguments BODY are forms to be executed within the temporary buffer
context."
  (let ((filename (make-symbol "filename"))
        (buff  (make-symbol "buff")))
    `(let* ((,filename ,file)
            (,buff (get-file-buffer ,filename)))
      (with-temp-buffer
        (if ,buff
            (insert-buffer-substring ,buff)
          (insert-file-contents ,filename))
        (goto-char (point-min))
        (let ((scad-mode-hook nil))
         (scad-mode))
        (let ((default-directory
               (file-name-parent-directory
                ,filename)))
         ,@body)))))

(defun scad-extra-project-export-directory ()
  "Return the \"stl\" directory in the project's root, if it exists."
  (when-let* ((proj-root
               (when-let* ((project (ignore-errors (project-current))))
                 (if (fboundp 'project-root)
                     (project-root project)
                   (with-no-warnings
                     (car (project-roots project)))))))
    (expand-file-name "stl" proj-root)))

;;;###autoload
(defun scad-extra-export (file)
  "Export a FILE to a specified directory using a customizable path.

Argument FILE is the path to which the SCAD file will be exported."
  (interactive (list
                (or
                 (read-file-name
                  "Export to: "
                  (when-let* ((dir
                               (cond ((functionp
                                       scad-extra-default-export-directory)
                                      (funcall
                                       scad-extra-default-export-directory))
                                     ((stringp
                                       scad-extra-default-export-directory)
                                      scad-extra-default-export-directory))))
                    (file-name-as-directory dir))
                  nil nil
                  (concat (file-name-base (buffer-file-name))
                          scad-export-extension)))))
  (scad-export file))

(defun scad-extra--preview-update-coords (x y z)
  "Update the camera preview's orientation coordinates.

This helper function takes three numeric parameters, each representing
an angle (in degrees), and updates the corresponding orientation values in
the global variable `scad-preview-camera'. The function sets the 4th,
5th, and 6th elements of the list, which control the preview camera's
rotation.

Parameters:
  X - the angle (in degrees) to set as the first orientation coordinate.
  Y - the angle (in degrees) to set as the second orientation coordinate.
  Z - the angle (in degrees) to set as the third orientation coordinate."
  (let* ((vals (list x y z)))
    (dotimes (i (length vals))
      (let ((val (nth i vals)))
        (setf (nth (+ i 3) scad-preview-camera)
              val)))))

(defun scad-extra--current-x-y-z ()
  "Return a subsequence of camera coordinates from indices 3 to 5."
  (seq-subseq scad-preview-camera 3 6))


(defun scad-extra--update-coords-or-invoke (x y z alternative-fn &optional msg
                                              alternative-message)
  "Update the camera coordinates or invoke an alternative function.

X is a numeric value representing the first orientation coordinate.
Y is a numeric value representing the second orientation coordinate.
Z is a numeric value representing the third orientation coordinate.

ALTERNATIVE-FN is the function called when the current coordinates match X, Y,
and Z.

Optional MSG is a string displayed when the coordinates are updated.

Optional ALTERNATIVE-MESSAGE is a string displayed when ALTERNATIVE-FN is
called."
  (if (equal (scad-extra--current-x-y-z)
             (list x y z))
      (when scad-extra-allow-reverse
        (funcall alternative-fn)
        (message (or alternative-message "Reversed")))
    (scad-extra--preview-update-coords x y z)
    (scad--preview-render)
    (when msg
      (message msg))))

(defun scad-extra-top-view ()
  "Render the preview from a top view orientation.

Configures `scad-preview-camera' to simulate a bird's-eye view,
where the viewpoint is directly above the object.

Example diagram (roughly):
   y
   |
   z---x

This view is useful for understanding the object's layout when viewed from
overhead."
  (interactive)
  (scad-extra--update-coords-or-invoke 0 0 0 #'scad-extra-bottom-view
                                       "Top view"
                                       "Bottom view"))

(defun scad-extra-bottom-view ()
  "Render the preview from a bottom view orientation.

Sets the camera orientation to display the scene from beneath the object,
providing an inverse perspective relative to the top view. Once the angles
are updated, the preview is rendered.

Example diagram (roughly):
   z---x
   |
   y

This view can help in visualizing undercuts or bottom details."
  (interactive)
  (scad-extra--update-coords-or-invoke 180 0 0 #'scad-extra-top-view
                                       "Bottom view"
                                       "Top view"))


(defun scad-extra-left-view ()
  "Render the preview from a left side view orientation.

Adjusts the camera so that the left side of the object is shown.
After updating the perspective angles, the preview is rendered.

Example diagram (roughly):
       z
       |
   y---x

This view gives insight into the left-facing features of the design."
  (interactive)
  (scad-extra--update-coords-or-invoke 90 0 270 #'scad-extra-right-view
                                       "Left view"
                                       "Right view"))


(defun scad-extra-right-view ()
  "Render the preview from a right side view orientation.

Updates the camera settings to display the right side of the object.
The orientation angles are set accordingly before invoking the preview
render command.

Example diagram (roughly):
   z
   |
   x---y

Use this view to inspect right-side details of your 3D model."
  (interactive)
  (scad-extra--update-coords-or-invoke 90 0 90 #'scad-extra-left-view
                                       "Right view"
                                       "Left view"))


(defun scad-extra-front-view ()
  "Render the preview from a front view orientation.

Modifies the camera orientation to a frontal perspective, showing the
face of the object. After updating the camera angles, the rendering
function is called to display the view.

Example diagram (roughly):
   z
   |
   y---x

This view is optimal for analyzing front-facing features."
  (interactive)
  (scad-extra--update-coords-or-invoke 90 0 0 #'scad-extra-back-view
                                       "Front view"
                                       "Back view"))


(defun scad-extra-back-view ()
  "Render the preview from a back view orientation.

Configures the preview camera to display the object from behind.
This is achieved by setting the proper camera angles and then rendering
the preview.

Example diagram (roughly):
       z
       |
   x---y

The back view is particularly useful for inspecting rear section details."
  (interactive)
  (scad-extra--update-coords-or-invoke 90 0 180 #'scad-extra-front-view
                                       "Back view"
                                       "Front view"))



(defun scad-extra--deg-to-rad (deg)
  "Convert DEG (in degrees) to radians."
  (/ (* deg float-pi) 180.0))


;;
;; We assume the Euler rotation order:
;;   1. Rotate by RX about X,
;;   2. then by RY about Y,
;;   3. then by RZ about Z.
;;
;; The individual matrices (with angles in radians) are:
;;
;;   Rx = [ 1    0       0      ]
;;        [ 0   cos(rx) -sin(rx)]
;;        [ 0   sin(rx)  cos(rx)]
;;
;;   Ry = [ cos(ry)  0  sin(ry)]
;;        [    0     1     0   ]
;;        [ -sin(ry) 0  cos(ry)]
;;
;;   Rz = [ cos(rz) -sin(rz) 0]
;;        [ sin(rz)  cos(rz) 0]
;;        [   0         0    1]
;;
;; The composite is R = Rz * Ry * Rx.
(defun scad-extra--compute-rotation-matrix (rx ry rz)
  "Return the composite rotation matrix for rotations RX, RY, and RZ (in degrees).

The result is a list of 9 numbers (row-major order):
 [ r00 r01 r02
   r10 r11 r12
   r20 r21 r22 ]."
  (let* ((rxd (scad-extra--deg-to-rad rx))
         (ryd (scad-extra--deg-to-rad ry))
         (rzd (scad-extra--deg-to-rad rz))
         (cx (cos rxd))
         (sx (sin rxd))
         (cy (cos ryd))
         (sy (sin ryd))
         (cz (cos rzd))
         (sz (sin rzd))
         ;;
         (r00 (* cz cy))
         (r01 (- (* cz (* sy sx))
                 (* sz cx)))
         (r02 (+ (* cz (* sy cx))
                 (* sz sx)))
         (r10 (* sz cy))
         (r11 (+ (* sz (* sy sx))
                 (* cz cx)))
         (r12 (- (* sz (* sy cx))
                 (* cz sx)))
         (r20 (- sy))
         (r21 (* cy sx))
         (r22 (* cy cx)))
    (list r00 r01 r02
          r10 r11 r12
          r20 r21 r22)))


(defun scad-extra--vector-add (v1 v2)
  "Return the addition of 3D vectors V1 and V2."
  (list (+ (nth 0 v1) (nth 0 v2))
        (+ (nth 1 v1) (nth 1 v2))
        (+ (nth 2 v1) (nth 2 v2))))

(defun scad-extra--vector-subtract (v1 v2)
  "Return the subtraction of 3D vector V2 from V1 (V1 - V2)."
  (list (- (nth 0 v1) (nth 0 v2))
        (- (nth 1 v1) (nth 1 v2))
        (- (nth 2 v1) (nth 2 v2))))

(defun scad-extra--vector-scale (v s)
  "Scale 3D vector V by scalar S."
  (list (* (nth 0 v) s)
        (* (nth 1 v) s)
        (* (nth 2 v) s)))

(defun scad-extra--vector-dot (v1 v2)
  "Return the dot product of 3D vectors V1 and V2."
  (+ (* (nth 0 v1) (nth 0 v2))
     (* (nth 1 v1) (nth 1 v2))
     (* (nth 2 v1) (nth 2 v2))))

(defun scad-extra--vector-cross (v1 v2)
  "Return the cross product of 3D vectors V1 and V2 (V1 × V2)."
  (list (- (* (nth 1 v1) (nth 2 v2))
           (* (nth 2 v1) (nth 1 v2)))
        (- (* (nth 2 v1) (nth 0 v2))
           (* (nth 0 v1) (nth 2 v2)))
        (- (* (nth 0 v1) (nth 1 v2))
           (* (nth 1 v1) (nth 0 v2)))))

(defun scad-extra--vector-norm (v)
  "Return the Euclidean norm (length) of 3D vector V."
  (sqrt (scad-extra--vector-dot v v)))

(defun scad-extra--vector-normalize (v)
  "Return a normalized copy of the 3D vector V.

If V is too small, return V unchanged."
  (let ((norm (scad-extra--vector-norm v)))
    (if (> norm 1e-8)
        (scad-extra--vector-scale v (/ 1.0 norm))
      v)))

;;
(defun scad-extra--compute-screen-basis ()
  "Return plist with keys :forward, :right, :left, and :up for onscreen directions.

The values are computed from the camera’s rotation in `scad-preview-camera'
positions 3-5."
  (let* ((rx (nth 3 scad-preview-camera))
         (ry (nth 4 scad-preview-camera))
         (rz (nth 5 scad-preview-camera))
         (mat (scad-extra--compute-rotation-matrix rx ry rz))
         (r02 (nth 2 mat))
         (r12 (nth 5 mat))
         (r22 (nth 8 mat))
         (forward-raw (list (- r02)
                            (- r12)
                            (- r22)))
         (forward (scad-extra--vector-normalize forward-raw))
         (world-up '(0 0 1))
         (dot (scad-extra--vector-dot world-up forward))
         (proj (scad-extra--vector-scale forward dot))
         (screen-up-raw (scad-extra--vector-subtract world-up proj))
         (screen-up (if (< (scad-extra--vector-norm screen-up-raw) 1e-3)
                        (if (< (nth 2 forward) 0)
                            '(0 -1 0)
                          '(0 1 0))
                      (scad-extra--vector-normalize screen-up-raw)))
         (right-raw (scad-extra--vector-cross screen-up forward))
         (right (scad-extra--vector-normalize right-raw))
         (left (scad-extra--vector-scale right -1)))
    (list
     :forward forward
     :up screen-up
     :right right
     :left left)))


(defun scad-extra--apply-translation (dir step)
  "Add a translation along the 3D direction vector DIR scaled by STEP. ."
  (let* ((tx (nth 0 scad-preview-camera))
         (ty (nth 1 scad-preview-camera))
         (tz (nth 2 scad-preview-camera))
         (dx (* (nth 0 dir) step))
         (dy (* (nth 1 dir) step))
         (dz (* (nth 2 dir) step)))
    (setf (nth 0 scad-preview-camera)
          (+ tx dx))
    (setf (nth 1 scad-preview-camera)
          (+ ty dy))
    (setf (nth 2 scad-preview-camera)
          (+ tz dz))
    (scad--preview-render)))


(defun scad-extra-translate-left (&optional step)
  "Translate the camera to the left by STEP (default 10) in a screen-aligned way."
  (interactive)
  (let* ((step (or step 10))
         (basis (scad-extra--compute-screen-basis))
         (raw-rx (nth 3 scad-preview-camera))
         (rx (mod raw-rx 360))
         (left (plist-get basis (if (< 0 rx 180)
                                    :right
                                  :left))))
    (scad-extra--apply-translation left step)))

(defun scad-extra-translate-right (&optional step)
  "Translate the camera to the right by STEP (default 10) in a screen-aligned way."
  (interactive)
  (let* ((step (or step 10))
         (basis (scad-extra--compute-screen-basis))
         (raw-rx (nth 3 scad-preview-camera))
         (rx (mod raw-rx 360))
         (right (plist-get basis (if (< 0 rx 180)
                                     :left
                                   :right))))
    (scad-extra--apply-translation right step)))

(defun scad-extra-translate-up (&optional step)
  "Translate the camera upward by STEP (default 10) in a screen-aligned way."
  (interactive)
  (let* ((step (or step 10))
         (basis (scad-extra--compute-screen-basis))
         (raw-rx (nth 3 scad-preview-camera))
         (rx (mod raw-rx 360))
         (up (plist-get basis :up)))
    (scad-extra--apply-translation up
                                   (if (< 0 rx 180)
                                       step
                                     (-
                                      step)))))

(defun scad-extra-translate-down (&optional step)
  "Translate the camera downward by STEP (default 10) in a screen-aligned way.
This is simply the inverse of `scad-extra-translate-up'."
  (interactive)
  (let ((step (or step 10)))
    (scad-extra-translate-up (- step))))


(defun scad-extra-translate-forward (&optional step)
  "Translate the camera forward by STEP (default 10)."
  (interactive)
  (let* ((step (or step 10))
         (basis (scad-extra--compute-screen-basis))
         (forward (plist-get basis :forward)))
    (scad-extra--apply-translation forward step)))

(defun scad-extra-translate-backward (&optional step)
  "Translate the camera backward by STEP (default 10).
This is the inverse of `scad-extra-translate-forward'."
  (interactive)
  (let ((step (or step 10)))
    (scad-extra-translate-forward (- step))))


(defun scad-extra--check-file-imported-p (file &optional visited-files)
  "Determine if FILE is imported, checking recursively through dependencies.

Argument FILE is the file to check for import status.

Optional argument VISITED-FILES is a list of files that have already been
checked to avoid infinite loops."
  (let ((files))
    (catch 'found
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward scad-extra--import-regexp nil t 1)
            (let* ((filename (match-string-no-properties 2))
                   (full-name (expand-file-name filename default-directory)))
              (when (file-equal-p file filename)
                (throw 'found t))
              (when (and (file-exists-p full-name)
                         (not (file-directory-p full-name))
                         (not (member full-name visited-files)))
                (push full-name files))))))
      (while files
        (let* ((filename (pop files))
               (buff (get-file-buffer filename)))
          (push filename visited-files)
          (when (if buff
                    (with-current-buffer buff
                      (scad-extra--check-file-imported-p file visited-files))
                  (with-temp-buffer
                    (insert-file-contents file)
                    (let ((default-directory (file-name-parent-directory file)))
                      (scad-extra--check-file-imported-p file visited-files))))
            (throw 'found t)))))))

(defun scad-extra--reload-related-preview-buffer ()
  "Reload the preview buffer if the current file is imported."
  (let ((wnd)
        (wnd-lst (window-list))
        (curr-buff (current-buffer))
        (curr-wind (selected-window))
        (file buffer-file-name))
    (while (and file (setq wnd (pop wnd-lst)))
      (unless (eq wnd curr-wind)
        (let* ((wnd-buff (window-buffer wnd))
               (mode (buffer-local-value 'major-mode wnd-buff))
               (orig-buff (and (eq mode 'scad-preview-mode)
                               (buffer-local-value 'scad--preview-buffer
                                                   wnd-buff))))
          (when (and orig-buff
                     (buffer-live-p orig-buff)
                     (not (eq orig-buff curr-buff))
                     (with-current-buffer orig-buff
                       (let
                           ((imported (scad-extra--check-file-imported-p file)))
                         (message "Checking file %s in buffer %s=%s " file
                                  orig-buff imported))))
            (when (buffer-live-p wnd-buff)
              (with-current-buffer wnd-buff
                (message "Refreshing %s" wnd-buff)
                (scad--preview-render)))))))))


;;;###autoload
(define-minor-mode scad-extra-reload-preview-mode
  "Toggle automatic reloading of related SCAD preview buffers on save.

When enabled, saving a SCAD source file automatically triggers a refresh of any
visible preview buffers that display SCAD files which import the saved file.
This ensures that changes in any included files are immediately reflected in
the rendered views without requiring manual intervention.

Example:
  1. A SCAD file (e.g., one defining design parameters) is open in a buffer.
  2. Another SCAD file that imports the parameter file via an `include' or `use'
     statement is also open, with its preview buffer visible.
  3. When any changes are made to the parameter file and it is saved, the
     preview buffer of the importing file automatically re-renders to
     incorporate the updates."
  :lighter " scad-extra-reload-preview"
  :global nil
  (if scad-extra-reload-preview-mode
      (add-hook 'after-save-hook
                #'scad-extra--reload-related-preview-buffer nil 'local)
    (remove-hook 'after-save-hook
                 #'scad-extra--reload-related-preview-buffer 'local)))

(defun scad-extra-comment-dwim (&optional arg)
  "Customize comment style based on ARG or use default settings.

Optional argument ARG is a prefix argument that determines the style
of the comment to be used."
  (interactive "P")
  (let* ((arg-num (car arg))
         (vars (cdr (seq-find (pcase-lambda (`(,args . _))
                                (or (eq arg-num args)
                                    (memq arg-num args)))
                              scad-extra-comment-dwim-default-style))))
    (if vars
        (let ((comment-start (or (cdr (assq 'comment-start vars))
                                 comment-start))
              (comment-end (or (cdr (assq 'comment-end vars)) comment-end))
              (comment-padding  (cdr (assq 'comment-padding vars)))
              (comment-style (or (cdr (assq 'comment-style vars))
                                 comment-style))
              (comment-continue  (cdr (assq 'comment-continue vars)))
              (comment-multi-line  (cdr (assq 'comment-multi-line vars))))
          (comment-dwim nil))
      (comment-dwim nil))))



(defun scad-extra--format-toggle (description value &optional on-label off-label
                                              left-separator right-separator
                                              divider align)
  "Format a toggle switch with DESCRIPTION and alignment options.

Argument DESCRIPTION is a string that provides a description for the toggle.

Argument VALUE is a boolean that determines the toggle state.

Optional argument ON-LABEL is a string label for the \"on\" state, defaulting to
\"+\".

Optional argument OFF-LABEL is a string label for the \"off\" state, defaulting
to \"-\".

Optional argument LEFT-SEPARATOR is a string used as the left separator,
defaulting to \"[\".

Optional argument RIGHT-SEPARATOR is a string used as the right separator,
defaulting to \"]\".

Optional argument DIVIDER is a character used to fill space, defaulting to
\".\".

Optional argument ALIGN is an integer specifying the alignment width, defaulting
to 20."
  (let* ((description (or description ""))
         (align (apply #'max (list (+ 5 (length description))
                                   (or align 20))))
         (face (if value 'success 'transient-inactive-value)))
    (concat
     (substring (concat
                 (or description "")
                 " "
                 (make-string (1- (1- align))
                              (if (stringp divider)
                                  (string-to-char divider)
                                (or divider ?\.)))
                 " ")
                0
                align)
     (or left-separator "[")
     (if value
         (propertize
          (or on-label "+")
          'face
          face)
       (propertize
        (or off-label "-")
        'face
        face))
     (or right-separator "]")
     " ")))

(defun scad-extra--standard-custom-value (sym)
  "Return the standard value of the symbol SYM."
  (eval (car (get sym 'standard-value))))

(defun scad-extra--saved-custom-value (sym)
  "Return the saved value of the symbol SYM."
  (eval (car (get sym 'saved-value))))

(defun scad-extra--custom-variable-changed-p (sym)
  "Return non nil if variable SYM is saveable and differs from the default."
  (when (custom-variable-p sym)
    (let ((val (symbol-value sym))
          (has-saved-val (get sym 'saved-value))
          (has-standard-val (get sym 'standard-value)))
      (when (or has-saved-val has-standard-val)
        (let ((custom-val (funcall (if has-saved-val
                                       #'scad-extra--saved-custom-value
                                     #'scad-extra--standard-custom-value)
                                   sym)))
          (not (equal val
                      custom-val)))))))


(defun scad-extra--set-variable (var value &optional save comment)
  "Set or SAVE a variable VAR to VALUE, optionally with COMMENT.

Argument VAR is the variable to set.

Argument VALUE is the new value for the variable VAR.

Optional argument SAVE is a boolean; if non-nil, the variable is saved to the
user's custom file.

Optional argument COMMENT is a string used as a comment when saving the
variable. It defaults to \"Saved by scad-extra.\"."
  (let ((customp (custom-variable-p var)))
    (if (and save customp)
        (customize-save-variable var value
                                 (or comment "Saved by scad-extra."))
      (if customp
          (funcall (or (get var 'custom-set) 'set-default) var value)
        (set-default var value)))))

(defun scad-extra--get-modified-variables ()
  "Return modified variables from `scad-extra-saveable-variables'."
  (seq-filter #'scad-extra--custom-variable-changed-p
              scad-extra-saveable-variables))

(defun scad-extra-save-variables ()
  "Save value of modified variables from `scad-extra-saveable-variables'."
  (interactive)
  (dolist (var (scad-extra--get-modified-variables))
    (scad-extra--set-variable var (symbol-value var)
                               t)))

(defun scad-extra--format-menu-heading (title &optional note)
  "Format TITLE as a menu heading.
When NOTE is non-nil, append it the next line."
  (let ((no-wb (= (frame-bottom-divider-width) 0)))
    (format "%s%s%s"
            (propertize title 'face `(:inherit transient-heading
                                      :overline ,no-wb)
                        'display '((height 1.1)))
            (propertize " " 'face `(:inherit transient-heading
                                    :overline ,no-wb)
                        'display '(space :align-to right))
            (propertize (if note (concat "\n" note) "") 'face
                        'font-lock-doc-face))))

(defun scad-extra-change-theme (next-val)
  "Change the SCAD preview theme to the selected NEXT-VAL.

Argument NEXT-VAL is the name of the theme to be applied."
  (interactive (list (completing-read "Theme"
                                      (remove (scad--preview-colorscheme)
                                              '("Cornfield"
                                                "Metallic"
                                                "Sunset"
                                                "Starnight"
                                                "BeforeDawn"
                                                "Nature"
                                                "DeepOcean"
                                                "Solarized"
                                                "Tomorrow"
                                                "Tomorrow Night")))))
  (cond ((derived-mode-p
          'scad-preview-mode)
         (setq-local scad-preview-colorscheme next-val)
         (scad--preview-render))
        (t
         (setq scad-preview-colorscheme next-val)))
  (when transient-current-command
    (transient-setup transient-current-command)))

(defvar scad-extra--views-options-suffixes
  (append
   (list '("p"
           (lambda ()
             (interactive)
             (let ((next-val
                    (if
                        (eq
                         scad-preview-projection
                         'ortho)
                        'perspective
                      'ortho)))
              (cond ((derived-mode-p
                      'scad-preview-mode)
                     (setq-local scad-preview-projection next-val)
                     (scad--preview-render))
               (t
                (setq scad-preview-projection next-val)))
              (transient-setup
               transient-current-command)))
           :description
           (lambda ()
             (concat "("
              (propertize "["
               'face
               'transient-inactive-value)
              (mapconcat (lambda (sym)
                           (let ((face
                                  (if
                                      (eq
                                       scad-preview-projection
                                       sym)
                                      'transient-value
                                    'transient-inactive-value)))
                            (propertize
                             (capitalize
                              (format
                               "%s"
                               sym))
                             'face
                             face)))
               (list 'perspective 'ortho)
               (propertize "|" 'face
                'transient-inactive-value))
              (propertize "]" 'face
               'transient-inactive-value)
              ")")))
         '("R" "Reset"
           (lambda ()
             (interactive)
             (scad--preview-reset))
           :inapt-if (lambda ()
                       (and
                        (equal scad-preview-camera
                         (copy-sequence (default-value
                                         'scad-preview-camera)))
                        (equal scad-preview-projection
                         (default-value 'scad-preview-projection)))))
         '("T" scad-extra-change-theme
           :description (lambda ()
                          (concat "Change Theme ("
                           (propertize
                            (format
                             "%s"
                             (scad--preview-colorscheme))
                            'face
                            'transient-value)
                           ")"))))
   (mapcar
    (pcase-lambda (`(,key ,value ,doc))
      (let ((sym (make-symbol (concat
                               "scad-extra--toggle-"
                               value))))
        (defalias sym
          (lambda ()
            (interactive)
            (let ((next-val
                   (if
                       (member
                        value
                        scad-preview-view)
                       (remove
                        value
                        scad-preview-view)
                     (append
                      scad-preview-view
                      (list
                       value)))))
              (cond ((derived-mode-p
                      'scad-preview-mode)
                     (setq-local scad-preview-view next-val)
                     (scad--preview-render))
                    (t
                     (setq scad-preview-view next-val)))
              (transient-setup
               transient-current-command)))
          doc)
        (list key sym
              :description
              (lambda ()
                (scad-extra--format-toggle
                 value
                 (member
                  value
                  scad-preview-view))))))
    '(("x" "axes"
       "Toggle displaying orthogonal axes indicator.")
      ("c" "crosshairs"
       "Toggle displaying crosshairs.")
      ("e" "edges"
       "Toggle rendering edges as well as faces")
      ("s" "scales"
       "Toggle displaying scales.")
      ("w" "wireframe"
       "Toggle displaying wireframe.")))))

(defvar scad-extra--saveable-options
  (append (mapcar
           (pcase-lambda (`(,key ,doc ,var))
             (list key
                   (lambda ()
                     (interactive)
                     (scad-extra--set-variable var
                                               (symbol-value var) t
                                               "Saved by scad-extra.")
                     (transient-setup transient-current-command))
                   :description (lambda ()
                                  (let ((val (symbol-value var)))
                                    (if (eq var 'scad-preview-camera)
                                        doc
                                      (concat
                                       doc
                                       " "
                                       (unless (listp val)
                                         "(")
                                       (propertize
                                        (format "%s" val)
                                        'face
                                        'transient-value)
                                       (unless (listp val)
                                         ")")))))
                   :inapt-if-not
                   (lambda ()
                     (scad-extra--custom-variable-changed-p var))))
           '(("-t" "theme" scad-preview-colorscheme)
             ("-p" "projection" scad-preview-projection)
             ("-v" "preview view" scad-preview-view)
             ("-o" "camera orientation" scad-preview-camera)))
          (list '("S" scad-extra-save-variables
                  :inapt-if-not scad-extra--get-modified-variables
                  :description
                  (lambda ()
                    (concat "all changed variables "
                     (mapconcat (lambda (it)
                                  (propertize
                                   (substring-no-properties (symbol-name
                                                             it))
                                   'face
                                   'transient-value))
                      (scad-extra--get-modified-variables)
                      ", ")))))))

;;;###autoload (autoload 'scad-extra-menu "scad-extra" nil t)
(transient-define-prefix scad-extra-menu ()
  "Provide a transient menu for `scad-preview-mode'."
  :transient-non-suffix #'transient--do-stay
  [:if-derived scad-preview-mode
   :description
   (lambda ()
     (scad-extra--format-menu-heading
      "Scad menu"
      (when (derived-mode-p 'scad-preview-mode)
        scad--preview-mode-camera)))
   ["Translate"
    ("M-<up>" "Up" scad-extra-translate-up :transient t)
    ("M-<down>" "Down" scad-extra-translate-down :transient t)
    ("M-<left>" "Left" scad-extra-translate-left :transient t)
    ("M-<right>" "Right" scad-extra-translate-right :transient t)
    ("B" "Backward" scad-extra-translate-backward :transient t)
    ("F" "Forward" scad-extra-translate-forward :transient t)]
   ["Rotate"
    ("t" "Top"  scad-extra-top-view :transient t)
    ("b" "Bottom" scad-extra-bottom-view :transient t)
    ("r" "Right" scad-extra-right-view :transient t)
    ("l" "Left" scad-extra-left-view :transient t)
    ("f" "Front" scad-extra-front-view :transient t)
    ("b" "Back" scad-extra-back-view :transient t)]
   [:description
    "View Options"
    :class transient-column
    :setup-children
    (lambda (&rest _argsn)
      (mapcar
       (apply-partially #'transient-parse-suffix
                        (oref transient--prefix command))
       scad-extra--views-options-suffixes))]
   ["Save"
    :class transient-column
    :setup-children
    (lambda (&rest _argsn)
      (mapcar
       (apply-partially #'transient-parse-suffix
                        (oref transient--prefix command))
       scad-extra--saveable-options))]]
  (interactive)
  (transient-setup #'scad-extra-menu))

(defun scad-extra--top-level-p (&optional pos)
  "Determine if POS is at top-level, not in a comment or string.

Optional argument POS is a buffer position, defaulting to the current point."
  (scad-extra--check-level-p 0 pos))

(defun scad-extra--check-level-p (level &optional pos)
  "Check if LEVEL matches current syntax level and not in comment/string.

Argument LEVEL is the desired syntax depth level to check against.

Optional argument POS is the position in the buffer to check, defaulting to the
current point."
  (let* ((pps (syntax-ppss (or pos (point))))
         (curr-level (car pps)))
    (and (equal curr-level level)
         (not (scad-extra--inside-comment-or-stringp)))))

(defun scad-extra--inside-comment-or-stringp (&optional pos pps)
  "Check if POS is inside a comment or string using `syntax-ppss'.

Optional argument POS is the position to check, defaulting to the current point.

Optional argument PPS is the precomputed `syntax-ppss' state, defaulting to
nil."
  (let ((pps (or pps
                 (syntax-ppss (or pos (point))))))
    (or (nth 4 pps)
        (nth 3 pps))))


(defun scad-extra--all-top-level-variables ()
  "Return a list of all top-level variable names with their positions."
  (let ((names))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while
            (re-search-forward scad-extra--variable-regex nil t 1)
          (let ((start (match-beginning 0))
                (end (match-end 0))
                (var-name (match-string-no-properties 1)))
            (when (and (save-excursion
                         (scad-extra--check-level-p 0 start))
                       (scad-extra--check-level-p 0 end))
              (push (cons var-name (cons start end)) names))))))
    (nreverse names)))

(defun scad-extra-find-unused-top-level-variables (in-file)
  "Identify and return unused top-level variables in a SCAD project file.

Argument IN-FILE is the file path to check for unused top-level variables."
  (interactive (list buffer-file-name))
  (setq in-file (expand-file-name in-file))
  (let* ((project (ignore-errors (project-current)))
         (variables (with-current-buffer
                        (get-file-buffer in-file)
                      (scad-extra--all-top-level-variables)))
         (var-names (mapcar #'car variables))
         (files (project-files project))
         (project-dir (scad-extra--project-name project))
         (file))
    (sit-for 0.01)
    (message "Checking for %d variables in %d files"
             (length var-names)
             (length files))
    (while
        (when var-names
          (setq file (pop files)))
      (setq file (expand-file-name file))
      (when (equal (file-name-extension file) "scad")
        (let ((shortname (substring-no-properties
                          file
                          (length project-dir))))
          (message "Checking %s" shortname)
          (with-temp-buffer
            (insert-file-contents file)
            (let ((scad-mode-hook nil))
              (scad-mode))
            (let ((is-file-current (file-equal-p file in-file)))
              (when (or is-file-current
                        (let ((default-directory
                               (file-name-parent-directory file))
                              (buffer-file-name file))
                          (scad-extra--check-file-imported-p
                           in-file)))
                (save-excursion
                  (goto-char (point-min))
                  (let ((regex
                         (concat "\\_<\\(" (mapconcat
                                            (lambda (it)
                                              (regexp-quote
                                               it))
                                            var-names
                                            "\\|")
                                 "\\)\\_>")))
                    (while (re-search-forward regex nil t 1)
                      (let ((name (match-string-no-properties 0))
                            (pos (match-end 0)))
                        (unless (or
                                 (scad-extra--inside-comment-or-stringp)
                                 (when is-file-current
                                   (pcase-let
                                       ((`(,beg . ,end)
                                         (cdr (assoc-string name
                                                            variables))))
                                     (< beg pos end))))
                          (setq var-names
                                (delete name var-names))))))))))
          (sit-for 0.01))))
    (if var-names
        (message "Found %d unused variables: %s"
                 (length var-names)
                 (string-join var-names
                              " "))
      (message "No unused variables was found"))
    var-names))

(defun scad-extra--top-level-modules ()
  "Return a list of all top-level module names with their positions."
  (let ((names))
    (save-excursion
      (save-match-data
        (goto-char (point-max))
        (with-syntax-table scad-mode-syntax-table
          (while
              (re-search-backward
               "\\_<\\(module\\)\\_>[\s\t\n]+\\([0-9A-Z_a-z]+\\)[\s\t\n]*\\((\\)"
               nil t 1)
            (let ((start (match-beginning 0))
                  (end (match-end 0))
                  (var-name (match-string-no-properties 1)))
              (when (and (save-excursion
                           (scad-extra--check-level-p 0 start))
                         (scad-extra--check-level-p 0 end))
                (push (cons var-name (cons start end)) names)))))))
    (nreverse names)))

(defun scad-extra--forward-whitespace ()
  "Advance the point past whitespace and comments in the buffer."
  (let ((pps (syntax-ppss (point))))
    (cond ((nth 4 pps)
           (goto-char (nth 8 pps))
           (forward-comment 1)
           (skip-chars-forward "\s\t\n"))
          (t
           (skip-chars-forward "\s\t\n")))
    (while (looking-at scad-extra--comment-start-re)
      (forward-comment 1)
      (skip-chars-forward "\s\t\n"))))

(defun scad-extra--backward-whitespace ()
  "Skip backward over whitespace and comments."
  (while (progn
           (skip-chars-backward "\s\t\n")
           (let ((pps (syntax-ppss (point))))
             (and (> (point)
                     (point-min))
                  (cond ((nth 4 pps)
                         (goto-char (nth 8 pps))
                         t)
                        ((looking-back "\\*/" 0)
                         (forward-comment -1)
                         t)))))))


(defun scad-extra--forward-sexp ()
  "Move forward over a SCAD expression, handling comments and whitespace."
  (let ((max-pos (point-max)))
    (while
        (progn
          (scad-extra--forward-whitespace)
          (when (> max-pos (point))
            (pcase (char-to-string (char-after (point)))
              ("{" (forward-sexp)
               nil)
              ((or "(" "[" "\"")
               (forward-sexp)
               (> max-pos
                  (point)))
              ("/"
               (if (not (looking-at scad-extra--comment-start-re))
                   (progn (forward-char 1)
                          (> (point-max)
                             (point)))
                 (scad-extra--forward-whitespace)
                 (> max-pos
                    (point))))
              ((or ")" "]" "}" "," ";") nil)
              (_ (> (skip-chars-forward "^;,)]([\"/") 0))))))))

(defun scad-extra--variable-used-p (name &optional body-end)
  "Check if a variable NAME is used in the code before BODY-END.

Argument NAME is a string representing the variable name to search for.

Optional argument BODY-END is a position indicating the end of the search
area within the buffer."
  (let ((found))
    (while
        (and (not found)
             (re-search-forward (concat
                                 "\\_<\\("
                                 (regexp-quote
                                  name)
                                 "\\)\\_>")
                                body-end t 1))
      (unless (scad-extra--inside-comment-or-stringp)
        (scad-extra--forward-whitespace)
        (setq found (or
                     (not (looking-at "="))
                     (progn (forward-char 1)
                            (scad-extra--forward-whitespace)
                            (looking-at "="))))))
    found))

(defun scad-extra--find-unused-body-vars (&optional body-end)
  "Identify and return unused variable assignments within a code body.

Optional argument BODY-END specifies the position up to which the search for
unused variables should be conducted, defaulting to the end of the buffer."
  (let ((vars)
        (case-fold-search t))
    (while (re-search-forward "=" body-end t 1)
      (unless (scad-extra--inside-comment-or-stringp)
        (let ((value-end)
              (value-start (point)))
          (when (save-excursion
                  (scad-extra--forward-whitespace)
                  (unless (looking-at "=")
                    (scad-extra--forward-sexp)
                    (when (looking-at ";")
                      (setq value-end (1+ (point))))))
            (save-excursion
              (forward-char -1)
              (scad-extra--backward-whitespace)
              (cond ((looking-back "\\([0-9A-Z_a-z$]+\\)" 0)
                     (let* ((end (point))
                            (beg (progn (skip-chars-backward "0-9A-Z_a-z$")
                                        (point)))
                            (name (buffer-substring-no-properties beg end)))
                       (when (string-match-p "[a-z_]" name)
                         (unless (scad-extra--variable-used-p name body-end)
                           (push (list name beg end value-start value-end)
                                 vars)))))))))))
    vars))


(defun scad-extra--unused-vars-in-module-at-point ()
  "Identifies unused variables and arguments in a module at point.

It returns a list containing:
- the parent’s name,
- the definition type (currently only \"module\"),
- a list of unused parameters, and
- a list of unused variables.

Both unused parameters and variables follow the same structure - each is a list
consisting of:
- the variable or argument name,
- the starting position of the name,
- the ending position of the name,
- the starting position of the variable’s value, and
- the ending position of the variable’s value."
  (when (looking-at "\\_<\\(module\\)\\_>")
    (let ((module-name)
          (case-fold-search t)
          (args-end)
          (args)
          (unused-args)
          (unused-vars))
      (skip-chars-forward "a-z")
      (scad-extra--forward-whitespace)
      (when (looking-at "\\([0-9A-Z_a-z]+\\)")
        (setq module-name (match-string-no-properties 0))
        (skip-chars-forward "0-9A-Z_a-z")
        (scad-extra--forward-whitespace)
        (when (looking-at "(")
          (setq args-end (save-excursion
                           (forward-sexp 1)
                           (point)))
          (forward-char 1)
          (scad-extra--forward-whitespace)
          (let ((max-arg-pos (1- args-end)))
            (while (and (looking-at "\\([0-9A-Z_a-z$]+\\)")
                        (< (point) max-arg-pos))
              (let ((arg (match-string-no-properties 0))
                    (beg (match-beginning 0))
                    (end (match-end 0))
                    (val-start)
                    (val-end))
                (goto-char end)
                (scad-extra--forward-whitespace)
                (when (looking-at "=[^=]")
                  (forward-char 1)
                  (setq val-start (point))
                  (scad-extra--forward-sexp)
                  (scad-extra--forward-whitespace)
                  (setq val-end (point)))
                (when (looking-at ",")
                  (forward-char 1)
                  (scad-extra--forward-whitespace))
                (setq args (push (list arg beg end
                                       val-start
                                       val-end)
                                 args))))
            (when (= (point) max-arg-pos)
              (forward-char 1)
              (scad-extra--forward-whitespace)
              (when (looking-at "{")
                (let* ((body-end (save-excursion
                                   (forward-sexp 1)
                                   (point))))
                  (setq unused-vars
                        (seq-remove
                         (lambda (it) (member
                                       (car it)
                                       scad-extra-allowed-unused-variables))
                         (save-excursion
                           (forward-char 1)
                           (scad-extra--find-unused-body-vars body-end))))
                  (dolist (it args)
                    (unless (save-excursion
                              (scad-extra--variable-used-p
                               (car it) body-end))
                      (setq unused-args (push it unused-args))))))))))
      (when (or unused-vars unused-args)
        (list module-name
              "module"
              unused-args unused-vars)))))


(defun scad-extra--find-unused-variables-in-definitions ()
  "Identify and return unused variables in SCAD definitions.

The result is a list where each element is itself a list comprising:
- the parent’s name,
- the definition type (currently only \"module\"),
- a list of unused parameters, and
- a list of unused variables.

Both unused variables and parameters share the same structure: each is a list
comprising:
- the variable or argument name,
- the starting position of the name,
- the ending position of the name,
- the starting position of the variable's value, and
- the ending position of the variable's value."
  (let ((result))
    (save-excursion
      (save-match-data
        (goto-char (point-max))
        (with-syntax-table scad-mode-syntax-table
          (while (re-search-backward
                  "\\_<\\(module\\)\\_>[\s\t\n]+\\([0-9A-Z_a-z]+\\)[\s\t\n]*\\((\\)"
                  nil t 1)
            (when (scad-extra--check-level-p 0 (point))
              (when-let* ((found
                           (save-excursion
                             (scad-extra--unused-vars-in-module-at-point))))
                (setq result (push found result))))))))
    result))

;;;###autoload
(define-derived-mode scad-extra-report-mode tabulated-list-mode
  "Scad Extra Report Mode."
  "Display a tabulated list with columns for unused variables in project or file."
  (setq tabulated-list-format
        [("Name" 30 t)
         ("Parent" 20 t)
         ("Parent Type" 5 t)
         ("Type" 10 t)
         ("File" 15 t)])
  (tabulated-list-init-header)
  (setq tabulated-list-padding 2)
  (setq-local imenu-prev-index-position-function
              (lambda ()
                (unless (bobp)
                  (forward-line -1))))
  (setq-local imenu-extract-index-name-function
              (lambda ()
                (unless (bobp)
                  (string-trim
                   (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))))))

(defun scad-extra--to-tabulated-entry (vars-and-args &optional file)
  "Convert variable and argument data into a tabulated list entry.

Argument VARS-AND-ARGS is a list containing module name, module type, arguments,
and variables.

Optional argument FILE is the file associated with the tabulated entry."
  (let* ((project (ignore-errors (project-current)))
         (project-dir
          (scad-extra--project-name project))
         (module-name (car vars-and-args))
         (mod-type (cadr vars-and-args))
         (args (nth 2 vars-and-args))
         (vars (nth 3 vars-and-args))
         (arg-names (mapcar #'car args)))
    (mapcar
     (pcase-lambda (`(,name ,beg ,end ,_val-start ,val-end))
       (apply #'vector (list (list
                              (format "%s" name)
                              'action
                              #'scad-extra--jump-to-entry
                              'button-data
                              (list file beg
                                    (or val-end end)))
                             module-name
                             mod-type
                             (if (member name arg-names)
                                 "Argument"
                               "Variable")
                             (list
                              (format "%s"
                                      (if project-dir
                                          (substring-no-properties
                                           (expand-file-name file)
                                           (length project-dir))
                                        (abbreviate-file-name file)))
                              'action
                              #'scad-extra--jump-to-entry
                              'button-data
                              (list file beg
                                    (or val-end end))))))
     (append args
             vars))))



(defun scad-extra--unused-vars-to-tabulated-entries (vars &optional file)
  "Convert unused variables list to tabulated entries format.

Argument VARS is a list of variables to be processed.

Optional argument FILE is a file associated with the variables."
  (mapcar (lambda (it)
            (list nil it))
          (mapcan (lambda (var)
                    (scad-extra--to-tabulated-entry var file))
                  vars)))

(defun scad-extra--jump-to-entry (button-data)
  "Navigate to a specified file region and highlight it momentarily.

Argument BUTTON-DATA is a cons cell containing a file path and a pair of buffer
positions."
  (let* ((file (car button-data))
         (bounds (cdr button-data))
         (beg (car bounds))
         (end (cadr bounds))
         (buff (or (get-file-buffer file)
                   (find-file-noselect file)))
         (wnd (or (get-buffer-window buff)
                  (get-buffer-window (pop-to-buffer buff)))))
    (with-selected-window wnd
      (when (and beg end)
        (goto-char beg)
        (set-window-point wnd beg)
        (pulse-momentary-highlight-region beg end)))))

(defvar-local scad-extra--tabulated-list-file nil)
(defvar-local scad-extra--tabulated-list-project nil)

(defun scad-extra--tabulated-list-revert ()
  "Revert and update tabulated list entries for unused SCAD variables."
  (cond (scad-extra--tabulated-list-file
         (setq tabulated-list-entries
               (scad-extra--unused-vars-to-tabulated-entries
                (if-let* ((buff
                           (get-file-buffer scad-extra--tabulated-list-file)))
                    (with-current-buffer buff
                      (scad-extra--find-unused-variables-in-definitions))
                  (with-temp-buffer
                    (insert-file-contents scad-extra--tabulated-list-file)
                    (let ((scad-mode-hook nil))
                      (scad-mode)
                      (scad-extra--find-unused-variables-in-definitions))))
                scad-extra--tabulated-list-file))
         (tabulated-list-print))
        (scad-extra--tabulated-list-project
         (let* ((vars (scad-extra--find-unused-variables-in-project
                       scad-extra--tabulated-list-project)))
           (setq tabulated-list-entries
                 vars)
           (tabulated-list-print)))))


(defun scad-extra--find-unused-variables-in-project (project)
  "Identify and return unused variables in SCAD files within a project.

Argument PROJECT is the project to search for unused variables."
  (let* ((files (seq-filter (lambda (it)
                              (equal (file-name-extension it)
                                     "scad"))
                            (project-files project)))
         (results))
    (dolist (file files)
      (message "Checking %s" (abbreviate-file-name file))
      (let ((vars (scad-extra-find-unused-variables-in-file file t)))
        (when vars
          (setq results (append results
                                (scad-extra--unused-vars-to-tabulated-entries
                                 vars
                                 file)))))
      (sit-for 0.01))
    results))

(defun scad-extra--read-scad-file (prompt &optional default-file)
  "PROMPT for a SCAD file name with completion, defaulting to DEFAULT-FILE.

Argument PROMPT is a string used to prompt the user for input.

Optional argument DEFAULT-FILE is a string representing the default file
to use if provided."
  (let* ((default-file (or default-file
                           (and buffer-file-name
                                (equal
                                 (file-name-extension
                                  buffer-file-name)
                                 "scad")
                                buffer-file-name)))
         (dir (if default-file
                  (file-name-parent-directory default-file)
                default-directory))
         (initial-input (and default-file
                             (file-name-nondirectory
                              (directory-file-name default-file)))))
    (read-file-name prompt
                    dir
                    default-file
                    t
                    initial-input
                    (lambda (file)
                      (or (string-suffix-p "/" file)
                          (string-suffix-p ".scad" file))))))


;;;###autoload
(defun scad-extra-find-unused-variables-in-file (file &optional no-report)
  "Identify and optionally report unused variables in a SCAD file.

Argument FILE is the path to the SCAD file to be analyzed for unused variables.

Optional argument NO-REPORT, when non-nil, suppresses the report generation of
unused variables."
  (interactive
   (list (scad-extra--read-scad-file "Find unused variables in: ")))
  (let ((vars
         (if-let* ((buff (get-file-buffer file)))
             (with-current-buffer buff
               (scad-extra--find-unused-variables-in-definitions))
           (with-temp-buffer
             (insert-file-contents file)
             (let ((scad-mode-hook nil))
               (scad-mode)
               (scad-extra--find-unused-variables-in-definitions))))))
    (unless no-report
      (let* ((report-buff-name (concat
                                "*scad-extra-unused-vars-in-"
                                file "-*"))
             (report-buff (get-buffer report-buff-name))
             (results  (scad-extra--unused-vars-to-tabulated-entries
                        vars
                        file)))
        (if (and (not vars)
                 (not (buffer-live-p report-buff)))
            (message "No unused variables found in %s" file)
          (with-current-buffer (get-buffer-create report-buff-name)
            (unless (derived-mode-p 'scad-extra-report-mode)
              (scad-extra-report-mode))
            (setq scad-extra--tabulated-list-file file)
            (add-hook 'tabulated-list-revert-hook
                      #'scad-extra--tabulated-list-revert nil t)
            (setq tabulated-list-entries results)
            (tabulated-list-print)
            (pop-to-buffer (current-buffer))))))
    vars))

;;;###autoload
(defun scad-extra-find-unused-variables-in-project ()
  "Identify and display unused variables in the current project."
  (interactive)
  (let* ((project (ignore-errors (project-current)))
         (results (scad-extra--find-unused-variables-in-project project))
         (report-buff-name "*scad-extra-unused-vars-in-project*")
         (report-buff (get-buffer report-buff-name)))
    (if (or results report-buff)
        (with-current-buffer (get-buffer-create
                              report-buff-name)
          (unless (derived-mode-p 'scad-extra-report-mode)
            (scad-extra-report-mode))
          (setq scad-extra--tabulated-list-project (project-current))
          (add-hook 'tabulated-list-revert-hook
                    #'scad-extra--tabulated-list-revert nil t)
          (setq tabulated-list-entries
                results)
          (tabulated-list-print)
          (pop-to-buffer (current-buffer)))
      (message "No unused variables is found"))))

(defun scad-extra--flymake-check-unused-vars (&optional report-fn &rest _args)
  "Identify and report unused variables and parameters in SCAD modules.

Optional argument REPORT-FN is a function that, if provided, will be called with
the list of problems found.

Remaining arguments _ARGS are ignored and not used in the function."
  (let* ((buff (current-buffer))
         (modules
          (scad-extra--find-unused-variables-in-definitions))
         (problems
          (mapcan (lambda (vars-and-args)
                    (let* ((args (nth 2 vars-and-args))
                           (vars (nth 3 vars-and-args))
                           (arg-names (mapcar #'car args)))
                      (mapcar
                       (pcase-lambda (`(,name ,beg ,end ,_val-start ,_val-end))
                         (flymake-make-diagnostic buff
                                                  beg end
                                                  :note (concat
                                                         "Unused "
                                                         (if (member name
                                                                     arg-names)
                                                             "parameter"
                                                           "variable"))))
                       (append args vars))))
                  modules)))
    (if report-fn
        (funcall report-fn problems)
      problems)))

;;;###autoload
(defun scad-extra-disable-flymake-check-unused-vars ()
  "Disable Flymake's check for unused variables and maybe deactivate Flymake mode."
  (interactive)
  (require 'flymake)
  (remove-hook 'flymake-diagnostic-functions
               #'scad-extra--flymake-check-unused-vars
               t)
  (when (and (bound-and-true-p flymake-mode)
             (not flymake-diagnostic-functions))
    (flymake-mode -1)))

;;;###autoload
(defun scad-extra-enable-flymake-check-unused-vars ()
  "Enable Flymake check for unused variables in current buffer."
  (interactive)
  (require 'flymake)
  (add-hook 'flymake-diagnostic-functions
            #'scad-extra--flymake-check-unused-vars nil t)
  (unless (bound-and-true-p flymake-mode)
    (flymake-mode 1))
  (when (fboundp 'flymake-start)
    (flymake-start)))


;;;###autoload
(defun scad-extra-rename-symbol (symb new-name)
  "Rename all occurrences of a symbol SYMB to NEW-NAME in project files.

Argument SYMB is the symbol to be renamed in the project files.

Argument NEW-NAME is the new name for the symbol SYMB."
  (interactive
   (let* ((sym (read-string "Symbol: "
                            (when-let* ((sym (symbol-at-point)))
                              (format "%s" sym))))
          (new-name (read-string (format "Rename %s to: " sym)
                                 sym)))
     (list sym new-name)))
  (let* ((project (ignore-errors (project-current)))
         (files (project-files project))
         (regex (concat  "\\_<\\(" (regexp-quote symb) "\\)\\_>"))
         (renamed-count 0)
         (in-files))
    (dolist-with-progress-reporter (file files)
        "Processing..."
      (sit-for 0.01)
      (setq file (expand-file-name file))
      (when (equal (file-name-extension file) "scad")
        (scad-extra--with-temp-buffer
         file
         (let ((renamed)
               (buffer))
           (save-excursion
             (goto-char (point-max))
             (while
                 (re-search-backward regex
                                     nil t 1)
               (unless (scad-extra--inside-comment-or-stringp)
                 (replace-match new-name nil nil nil 0)
                 (setq renamed t)
                 (setq renamed-count (1+ renamed-count))
                 (unless (member file in-files)
                   (push file in-files)))))
           (when renamed
             (setq buffer (current-buffer))
             (let ((orig-buff (get-file-buffer
                               file)))
               (if orig-buff
                   (with-current-buffer
                       orig-buff
                     (let ((pos (point)))
                       (delete-region (point-min)
                                      (point-max))
                       (insert-buffer-substring
                        buffer)
                       (if (>= (point-max) pos)
                           (goto-char pos)))
                     (save-buffer))
                 (write-region nil nil file nil))))))))
    (if (> renamed-count 0)
        (message "Renamed %d occurenced in %d files" renamed-count (length
                                                                    in-files))
      (message "No symbols renamed"))))

(defun scad-extra--project-name (&optional project)
  "Return expanded PROJECT root directory path for project or current project.

Optional argument PROJECT specifies the current project object. If not provided,
it attempts to use the current PROJECT by default."
  (when-let* ((project (or project
                           (ignore-errors (project-current))))
              (proj-dir (if (fboundp 'project-root)
                            (project-root (or project))
                          (with-no-warnings
                            (car (project-roots project))))))
    (expand-file-name proj-dir)))

(defun scad-extra--imported-files ()
  "Collect and return a list of imported file specifications in the buffer."
  (let ((files))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward scad-extra--import-regexp nil t 1)
          (let ((filename (match-string-no-properties 2))
                (beg (match-beginning 0))
                (end (match-end 0))
                (type (match-string-no-properties 1)))
            (when (and filename
                       (not
                        (scad-extra--inside-comment-or-stringp)))
              (push (list filename beg end type) files))))))
    (nreverse files)))


;;;###autoload
(defun scad-extra-import-project-file (file)
  "Insert a SCAD import declaration into the current buffer's project file.

Argument FILE is the path to the file within the project directory to be
imported into the current buffer."
  (interactive
   (let* ((include current-prefix-arg)
          (curr-file buffer-file-name)
          (project (ignore-errors (project-current)))
          (proj-name (scad-extra--project-name project))
          (file
           (let* ((files (seq-filter (lambda (it)
                                       (equal (file-name-extension it)
                                              "scad"))
                                     (project-files project)))
                  (proj-name-len (length proj-name))
                  (relnames (mapcar (lambda (file)
                                      (if (file-name-absolute-p file)
                                          (substring-no-properties
                                           (expand-file-name file)
                                           proj-name-len)
                                        file))
                                    files))
                  (longest
                   (if relnames
                       (propertize " " 'display
                                   (list 'space :align-to
                                         (apply #'max
                                                (mapcar #'length relnames))))
                     10))
                  (imported-files (scad-extra--imported-files))
                  (annotf (lambda (str)
                            (when imported-files
                              (let* ((fullname (expand-file-name str
                                                                 proj-name))
                                     (imported
                                      (assoc-string (file-relative-name
                                                     fullname
                                                     default-directory)
                                                    imported-files)))
                                (concat longest
                                        (when imported
                                          " Imported"))))))
                  (prompt (if include "Include: " "Use: ")))
             (completing-read prompt
                              (lambda (str pred action)
                                (if (eq action 'metadata)
                                    `(metadata
                                      (annotation-function . ,annotf))
                                  (complete-with-action
                                   action relnames str pred)))
                              (lambda (f)
                                (and (not (equal
                                           (expand-file-name f
                                                             proj-name)
                                           curr-file))))))))
     (list (file-relative-name (expand-file-name file proj-name)
                               default-directory))))
  (let ((imported-files
         (scad-extra--imported-files)))
    (if (assoc-string file imported-files)
        (message "%s is already imported" file)
      (save-excursion
        (pcase-let* ((import-type (if current-prefix-arg
                                      "include"
                                    "use"))
                     (`(,_ ,_ ,last-imp-end ,last-imp-type)
                      (car (last (or (seq-filter (pcase-lambda
                                                   (`(,_ ,_ ,_ ,type))
                                                   (equal type import-type))
                                                 imported-files)
                                     imported-files))))
                     (imp-str (concat import-type " <" file ">\n")))
          (cond ((not last-imp-type)
                 (goto-char (point-min))
                 (when (save-excursion
                         (skip-chars-forward "\s\t\n")
                         (looking-at scad-extra--comment-start-re))
                   (scad-extra--forward-whitespace)
                   (skip-chars-backward "\s\t\n")
                   (when (or
                          (nth 4 (syntax-ppss (point)))
                          (looking-back "\\*/" 0))
                     (setq imp-str (concat "\n" imp-str))))
                 (insert imp-str))
                ((or (equal last-imp-type import-type)
                     (not current-prefix-arg))
                 (goto-char last-imp-end)
                 (insert (concat "\n"
                                 imp-str
                                 (unless (looking-at "\n\n\n")
                                   "\n"))))
                (t
                 (goto-char (cadar imported-files))
                 (insert imp-str))))))))

(provide 'scad-extra)
;;; scad-extra.el ends here