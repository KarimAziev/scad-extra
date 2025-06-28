;;; scad-extra.el --- Additional commands for scad-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/scad-extra
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "29.1") (scad-mode "96.0") (project "0.11.1") (transient "0.8.7"))
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

;; Additional commands for scad-mode that rearrange the preview camera to display
;; different standard views (top, bottom, left, right, front, and back).


;;; Code:

(require 'scad-mode)
(require 'subr-x)
(require 'project)
(require 'transient)

(defcustom scad-extra-default-export-directory 'scad-extra-project-export-directory
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

(defconst scad-extra--import-regexp
  "\\_<\\(include\\|use\\)\\_>[\s]*<\\([A-Za-z_][^>]*\\)>")

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
                                     ((stringp scad-extra-default-export-directory)
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
the global variable `scad-preview-camera`. The function sets the 4th,
5th, and 6th elements of the list, which control the preview camera's
rotation.

Parameters:
  X -- the angle (in degrees) to set as the first orientation coordinate.
  Y -- the angle (in degrees) to set as the second orientation coordinate.
  Z -- the angle (in degrees) to set as the third orientation coordinate."
  (let* ((vals (list x y z)))
    (dotimes (i (length vals))
      (let ((val (nth i vals)))
        (setf (nth (+ i 3) scad-preview-camera)
              val)))))

(defun scad-extra--current-x-y-z ()
  "Return a subsequence of camera coordinates from indices 3 to 5."
  (seq-subseq scad-preview-camera 3 6))


(defun scad-extra--update-coords-or-invoke (x y z alternative-fn)
  "Update camera coordinates or invoke an alternative function.

Argument X is a numeric value representing the first orientation coordinate.

Argument Y is a numeric value representing the second orientation coordinate.

Argument Z is a numeric value representing the third orientation coordinate.

Argument ALTERNATIVE-FN is a function to call if the current coordinates match
the given ones."
  (if (equal (scad-extra--current-x-y-z)
             (list x y z))
      (progn
        (funcall alternative-fn)
        (message "Reversed"))
    (scad-extra--preview-update-coords x y z)
    (scad--preview-render)))

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
  (scad-extra--update-coords-or-invoke 0 0 0 #'scad-extra-bottom-view))

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
  (scad-extra--update-coords-or-invoke 180 0 0 #'scad-extra-top-view))


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
  (scad-extra--update-coords-or-invoke 90 0 270 #'scad-extra-right-view))


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
  (scad-extra--update-coords-or-invoke 90 0 90 #'scad-extra-left-view))


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
  (scad-extra--update-coords-or-invoke 90 0 0 #'scad-extra-back-view))


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
  (scad-extra--update-coords-or-invoke 90 0 180 #'scad-extra-front-view))


;;;###autoload
(defun scad-extra-increase-number-at-point ()
  "Increase the number at point by a small step, adjusting its format."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'number)))
    (if (not bounds)
        (progn
          (require 'move-text nil t)
          (when (fboundp 'move-text-up)
            (call-interactively #'move-text-up)))
      (let* ((start (car bounds))
             (end (cdr bounds))
             (numstr (buffer-substring-no-properties start end))
             (pt (point))
             (dot-pos (string-match "\\." numstr))
             (curr-col (current-column))
             (new-str))
        (if (not dot-pos)
            (let ((num (string-to-number numstr)))
              (setq new-str (number-to-string (1+ num))))
          (let* ((frac-length (- (length numstr) dot-pos 1))
                 (offset (- pt start dot-pos 1))
                 (step
                  (if (or (< offset 0)
                          (>= offset frac-length))
                      (/ 1.0 (expt 10 frac-length))
                    (/ 1.0 (expt 10 (- frac-length offset)))))
                 (num (string-to-number numstr))
                 (newnum (+ num step))
                 (fmt (format "%%.%df" frac-length)))
            (setq new-str (format fmt newnum))))
        (replace-region-contents start end (lambda () new-str))
        (when (>= (length new-str)
                  (length numstr))
          (move-to-column curr-col))))))

;;;###autoload
(defun scad-extra-decrease-number-at-point ()
  "Decrease the number at point by a small step."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'number)))
    (if (not bounds)
        (progn
          (require 'move-text nil t)
          (when (fboundp 'move-text-down)
            (call-interactively #'move-text-down)))
      (let* ((start (car bounds))
             (end (cdr bounds))
             (numstr (buffer-substring-no-properties start end))
             (pt (point))
             (dot-pos (string-match "\\." numstr))
             (curr-col (current-column))
             (new-str))
        (if (not dot-pos)
            (let ((num (string-to-number numstr)))
              (setq new-str (number-to-string (1- num))))
          (let* ((frac-length (- (length numstr) dot-pos 1))
                 (offset (- pt start dot-pos 1))
                 (step
                  (if (or (< offset 0)
                          (>= offset frac-length))
                      (/ 1.0 (expt 10 frac-length))
                    (/ 1.0 (expt 10 (- frac-length offset)))))
                 (num (string-to-number numstr))
                 (newnum (- num step))
                 (fmt (format "%%.%df" frac-length)))
            (setq new-str (format fmt newnum))))
        (replace-region-contents start end (lambda () new-str))
        (when (>= (length new-str)
                  (length numstr))
          (move-to-column curr-col))))))


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
                       (message "Checking file %s in buffer %s " file orig-buff)
                       (scad-extra--check-file-imported-p file)))
            (when (buffer-live-p wnd-buff)
              (with-current-buffer wnd-buff
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



(provide 'scad-extra)
;;; scad-extra.el ends here