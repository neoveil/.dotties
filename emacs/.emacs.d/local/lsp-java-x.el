;; -*- lexical-binding: t; -*-

(defcustom lsp-java-lombok-enabled t
  "If non-nil starts provides lombok support to JDTLS."
  :type 'boolean
  :group 'lsp-java-lombok)

(defcustom lsp-java-lombok-version nil
  "If non-nil, use the specified lombok version, otherwise use the latest."
  :type '(choice (string)
                 (const nil))
  :group 'lsp-java-lombok)

(defcustom lsp-java-lombok-install-dir (file-name-concat user-emacs-directory ".cache")
  "The path on disk where lombok jars are installed."
  :type 'directory
  :group 'lsp-java-lombok)

(defconst lsp-java-lombok--jar-base-url "https://projectlombok.org/downloads/"
  "The base path to download lombok jars from.")

(defun lsp-java-lombok--jar-file ()
  "Get the filename for the lombok jar."
  (concat "lombok"
          (when lsp-java-lombok-version "-")
          lsp-java-lombok-version
          ".jar"))

(defun lsp-java-lombok--jar-path ()
  "Generate the path on disk for the lombok jar."
  (expand-file-name (file-name-concat lsp-java-lombok-install-dir (lsp-java-lombok--jar-file))))

(defun lsp-java-lombok--ensure-install-dir ()
  (unless (file-exists-p lsp-java-lombok-install-dir)
    (make-directory lsp-java-lombok-install-dir t)))

(defun lsp-java-lombok--install-jar ()
  "Download and install the lombok jar."
  (lsp-java-lombok--ensure-install-dir)
  (let* ((lombok-url (url-generic-parse-url lsp-java-lombok--jar-base-url))
         (base-path (file-name-as-directory (url-filename lombok-url)))
         (file-path (file-name-concat base-path (lsp-java-lombok--jar-file))))
    (setf (url-filename lombok-url) file-path)
    (url-copy-file lombok-url (lsp-java-lombok--jar-path) t)))

(defun lsp-java-lombok--append-vmargs ()
  "Apply lombok args to lsp-java-vmargs."
  (defvar lsp-java-vmargs)
  (add-to-list 'lsp-java-vmargs (concat "-javaagent:" (lsp-java-lombok--jar-path)) t))

(defun lsp-java-lombok-setup ()
  "Setup lsp-java-lombok."
  (when lsp-java-lombok-enabled
    (when (not (file-exists-p (lsp-java-lombok--jar-path)))
      (message "Could not find lombok for lsp-java. Installing...")
      (lsp-java-lombok--install-jar))
    (lsp-java-lombok--append-vmargs)))

(defun lsp-java-replace-vmargs (pairs)
  (defvar lsp-java-vmargs)
  (dolist (pair pairs)
    (let* ((prefix (car pair))
           (new-value (cdr pair))
           (full-arg (concat prefix new-value)))
      (setq-default lsp-java-vmargs
                    (mapcar (lambda (arg)
                              (if (string-prefix-p prefix arg)
                                  full-arg
                                arg))
                            lsp-java-vmargs)))))

(provide 'lsp-java-x)
