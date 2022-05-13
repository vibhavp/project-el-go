;;; project-go.el --- Project implementation for the Go Programming Language -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Vibhav Pant

;; Author: Vibhav Pant <vibhavp@gmail.com>
;; URL: https://github.com/vibhavp/project-el-go
;; Package-Requires: ((emacs "28.1"))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Go support for project.el.  Enable with
;; `(add-to-list 'project-find-functions #'project-go-try-go)'

;;; Code:

(defgroup project-go nil
  "Project implementation for the Go programming language."
  :package-version '(project-go . "0.1.0")
  :group 'project
  :link '(url-link "https://github.com/vibhavp/project-el-go"))

(defcustom project-go-goarch ""
  "The GOARCH value to use for this project."
  :group 'project-go
  :type 'string
  :safe #'stringp)

(defcustom project-go-goos ""
  "The GOOS value to use for this project."
  :group 'project-go
  :type 'string
  :safe #'stringp)

(defcustom project-go-goroot ""
  "The GOROOT value to use for this project."
  :group 'project-go
  :type 'string
  :safe #'stringp)

(defcustom project-go-gopath ""
  "The GOPATH value to use for this project."
  :group 'project-go
  :type 'string
  :safe #'stringp)

(defcustom project-go-build-flags '("")
  "Build flags to use for this project."
  :group 'project-go
  :type '(repeat string)
  :safe #'sequencep)

(defvar project-go-go-path "go")
(defvar project-go--files-include-type '(:GoFiles :CgoFiles :CFiles :CXXFiles
                                                  :MFiles :HFiles :FFiles
                                                  :SFiles :SwigFiles :SwigCXXFiles
                                                  :TestGoFiles :XTestGoFiles
                                                  :EmbedFiles :TestEmbedFiles
                                                  :XTestEmbedFiles))
(defvar project-go--files-ignored-type '(:IgnoredGoFiles :IgnoredOtherFiles))

(cl-defstruct project-go--project
  (root nil :type project-go--module)
  (main-modules nil :type list)
  (modules nil :type list))

(cl-defstruct project-go--module
  (path "" :type string)
  (main nil :type bool)
  (dir "" :type string)
  (go-mod "" :type string)
  (go-version "" :type string))

(define-inline project-go--json-to-module (json)
  (inline-letevals (json)
    (inline-quote
     (map-let (:Path :Main :Dir :GoMod :GoVersion) ,json
       (make-project-go--module :path Path
                                :main Main
                                :dir Dir
                                :go-mod GoMod
                                :go-version GoVersion)))))

;;;###autoload
(defun project-go-try-go (dir)
  "Try finding a Go project in DIR.
Returns a `project-go--project' type if successful, else nil."
  (with-temp-buffer
    (if (zerop (call-process "go" nil (current-buffer) t
                             "list" "-m" "-json"))
        (progn
          (goto-char (point-min))
          (let ((main-modules nil)
                (modules nil))
            (while (not (eq (point) (1- (point-max))))
              (let ((module (project-go--json-to-module
                             (json-parse-buffer :object-type 'plist :array-type 'list))))
                (unless (string= (project-go--module-path module) "command-line-arguments")
                  (if (project-go--module-main module)
                      (push module main-modules)
                    (push module modules)))))
            (if (and (null main-modules) (null modules))
                nil
              (setq modules (nreverse modules)
                    main-modules (nreverse main-modules))
              (make-project-go--project :root (seq-first main-modules)
                                        :main-modules main-modules
                                        :modules modules))))
      nil)))


(defun project-go--get-files (project dirs props)
  (with-temp-buffer
    (let ((root (project-go--module-path (project-go--project-root project))))
      (apply #'call-process "go" nil (current-buffer) t "list" "-json"
             (seq-map (lambda (dir) (concat dir "/...")) dirs))
      (goto-char (point-min))
      (let ((final-files nil))
        (while (not (eq (point) (1- (point-max))))
          (pcase-let* ((json (json-parse-buffer :object-type 'plist :array-type 'list))
                       (map ((:Dir package-dir)) json)
                       (files-list nil))
            (seq-do (lambda (property)
                      (if-let* ((files (plist-get json property)))
                          (setq files-list
                                (seq-concatenate 'list files-list
                                                 (seq-map (lambda (file)
                                                            (expand-file-name file package-dir))
                                                          files)))))
                    props)
            (setq final-files
                  (if (null final-files)
                      files-list
                    (seq-concatenate 'list final-files files-list)))))
        final-files))))

(cl-defmethod project-files ((project project-go--project) &optional dirs)
  (project-go--get-files project dirs project-go--files-include-type))

(cl-defmethod project-ignores ((project project-go--project) dir)
  (project-go--get-files project (list dir) project-go--files-ignored-type))

(cl-defmethod project-external-roots ((project project-go--project))
  (with-temp-buffer
    (let* ((root-project (project-go--project-root project))
           (root-dir (project-go--module-dir root-project))
           (default-directory root-dir)
           (external-roots nil))
      (call-process "go" nil (current-buffer) t "list" "-json" "-m" "all")
      (goto-char (point-min))
      (while (not (eq (point) (1- (point-max))))
        (pcase-let* ((json (json-parse-buffer :object-type 'plist :array-type 'list))
                     ((map :Dir :Main) json))
          (unless (or Main
                      (null Dir))
            (push Dir external-roots))))
      (nreverse external-roots))))

(cl-defmethod project-root ((project project-go--project))
  (project-go--module-dir (project-go--project-root project)))

(provide 'project-go)
;;; project-go.el ends here
