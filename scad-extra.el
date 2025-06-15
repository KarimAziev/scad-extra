;;; scad-extra.el --- Additional commands for scad-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/scad-extra
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "28.1") (scad-mode "96.0") (project "0.11.1"))
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
(require 'project)

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
  (scad-extra--preview-update-coords 0 0 0)
  (scad--preview-render))

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
  (scad-extra--preview-update-coords 180 0 0)
  (scad--preview-render))


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
  (scad-extra--preview-update-coords 90 0 270)
  (scad--preview-render))


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
  (scad-extra--preview-update-coords 90 0 90)
  (scad--preview-render))


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
  (scad-extra--preview-update-coords 90 0 0)
  (scad--preview-render))


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
  (scad-extra--preview-update-coords 90 0 180)
  (scad--preview-render))

(provide 'scad-extra)
;;; scad-extra.el ends here