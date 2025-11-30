;;; eglot-java-lombok.el --- Lombok support for eglot-java -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Lucas A. F. Vitor

;; Version: 0.1.0
;; Author: Lucas A. F. Vitor <luca.oetdbem@gmail.com>
;; Maintainer: Lucas A. F. Vitor <luca.oetdbem@gmail.com>
;; URL: https://github.com/neoveil/.dotties/blob/main/emacs/.emacs.d/local/eglot-java-lombok.el
;; Keywords: convenience, languages, java, lombok
;; Package-Requires: ((emacs "30.2"))

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

;; Lombok support for `eglot-java'.
;;
;; As of now, `eglot-java' does not support lombok usage into JDTLS
;; This comes as a facility and glue between them.
;;
;; Just to note that this only setup the `eglot-java-eclipse-jdt-args'
;; variable of `eglot-java' to have a javaagent pointing to the lombok
;; jar.
;;
;; It also provides automatic installations of the lombok jar, and i
;; think that it is the defacto feature of this module
;;
;; You can force the installation of the lombok jar with `eglot-java-lombok-install'
;; You can update the lombok jar with `eglot-java-lombok-update'
;; To setup installation if not already installed and setup JDTLS args
;; use `eglot-java-lombok-ensure'
;;
;; An usage example (using `use-package'):
;;
;; (use-feature eglot-java-lombok
;;  :after eglot-java
;;  :config
;;  (eglot-java-lombok-ensure))

;;; Code:

(require 'url)
(require 'eglot-java)

(defgroup eglot-java-lombok nil
  "Lombok integration for eglot-java."
  :prefix "eglot-java-lombok-"
  :group 'eglot-java
  :link '(url-link :tag "GitHub" "https://github.com/neoveil/.dotties/blob/main/emacs/.emacs.d/local/eglot-java-lombok.el"))

(defcustom eglot-java-lombok-jar-directory
  (expand-file-name "share/lombok" user-emacs-directory)
  "Directory where the Lombok JAR should be installed."
  :type 'directory
  :group 'eglot-java-lombok)

(defcustom eglot-java-lombok-download-url
  "https://projectlombok.org/downloads/lombok.jar"
  "URL used to download the Lombok JAR."
  :type 'string
  :group 'eglot-java-lombok)

(defun eglot-java-lombok--jar-path ()
  "Return the full path to the Lombok JAR."
  (expand-file-name "lombok.jar" eglot-java-lombok-jar-directory))

(defun eglot-java-lombok--download (dest &optional quiet)
  "Download Lombok JAR to DEST.

If QUIET is non-nil, do not echo progress messages.
Signals `user-error' on failure."
  (make-directory (file-name-directory dest) t)
  (condition-case err
      (progn
        (url-copy-file eglot-java-lombok-download-url dest t)
        (unless quiet
          (message "eglot-java-lombok: downloaded Lombok to %s" dest))
        dest)
    (error
     (user-error "eglot-java-lombok: failed to download Lombok: %s"
                 (error-message-string err)))))

(defun eglot-java-lombok--inject ()
  "Inject Lombok javaagent into `eglot-java-eclipse-jdt-args'."
  (let* ((jar (eglot-java-lombok--jar-path))
         (arg (concat "-javaagent:" jar)))
    (unless (file-exists-p jar)
      (user-error "eglot-java-lombok: JAR not found at %s; run `eglot-java-lombok-install' or `eglot-java-lombok-ensure'" jar))
    (unless (member arg eglot-java-eclipse-jdt-args)
      (setq eglot-java-eclipse-jdt-args
            (cons arg eglot-java-eclipse-jdt-args)))))

;;;###autoload
(defun eglot-java-lombok-install (&optional force)
  "Download and install the Lombok JAR.

By default, if the file already exists in `eglot-java-lombok-jar-directory',
ask for confirmation before overwriting it.

Interactively, with prefix argument FORCE, overwrite without asking."
  (interactive "P")
  (let ((dest (eglot-java-lombok--jar-path)))
    (when (and (file-exists-p dest)
               (not force)
               (not (yes-or-no-p
                     (format "Lombok already installed at %s. Overwrite? " dest))))
      (user-error "eglot-java-lombok: install aborted"))
    (eglot-java-lombok--download dest)
    (message "eglot-java-lombok: Lombok installed at %s" dest)))

;;;###autoload
(defun eglot-java-lombok-update ()
  "Update the Lombok JAR by re-downloading it.

This overwrites the existing JAR without confirmation.
If the JAR does not yet exist, acts as `eglot-java-lombok-install'"
  (interactive)
  (let ((dest (eglot-java-lombok--jar-path)))
    (eglot-java-lombok--download dest t)
    (message "eglot-java-lombok: Lombok updated at %s" dest)))

;;;###autoload
(defun eglot-java-lombok-ensure ()
  "Ensure Lombok is installed and injected into JDT LS.

If the JAR does not exist under `eglot-java-lombok-jar-directory',
download it. Then inject the corresponding `-javaagent' flag into
`eglot-java-eclipse-jdt-args'."
  (interactive)
  (let ((jar (eglot-java-lombok--jar-path)))
    (unless (file-exists-p jar)
      (eglot-java-lombok--download jar t))
    (eglot-java-lombok--inject)
    (message "eglot-java-lombok: ensured Lombok at %s and injected into eglot-java" jar)))

(provide 'eglot-java-lombok)

;;; eglot-java-lombok.el ends here
