(defvar base-directory (expand-file-name "~/Projects/github.io/"))
(defvar public-directory (file-name-concat base-directory "docs"))

(mkdir public-directory t)

(require 'package)
(setq package-user-dir (file-name-concat base-directory ".packages"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(require 'ox-publish)
(require 'cl-lib)

(use-package htmlize
  :ensure t)

(defvar inline-html-publish-index-subpages nil)
(defvar inline-html-publish-toc-marker "<!-- Inline html toc marker -->")
(defvar inline-html-publish-subpage-marker "<!-- Inline html subpage marker -->")
(defvar inline-html-publish-page-format "<h1 class='subpage-title' id='%s'><a href='%s'>%s</a></h1>")

(setq org-html-head-include-default-style t
      org-html-preamble t
      org-html-htmlize-output-type 'css
      org-html-postamble t
      org-html-head (concat
                     "<link rel='stylesheet' type='text/css' href='https://latex.now.sh/style.css'/>"
                     "<link rel='stylesheet' type='text/css' href='org-htmlize-style.css'/>"
                     "<link rel='stylesheet' type='text/css' href='style.css'/>"))

(setq org-publish-project-alist
      `(("contents"
         :base-directory ,base-directory
         :base-extension "org"
         :exclude "index.org"
         :publishing-directory ,public-directory
         :recursive t
         :publishing-function org-html-publish-to-html
         :html-postamble ,(concat
                           "<p class=\"author\">%a</p>"
                           "<p class=\"author\">Date: %T</p>"
                           "<p class=\"author\">%c</p>")
         :headline-levels 4
         :auto-preamble t)
        ("static"
         :base-directory ,base-directory
         :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :include ("CNAME")
         :exclude "docs"
         :publishing-directory ,public-directory
         :recursive t
         :publishing-function org-publish-attachment)
        ("index"
         :inline-components ("contents")
         :inline-plist (:body-only t :publishing-function inline-html-publish)
         :publishing-function inline-html-publish-index
         :base-directory ,base-directory
         :base-extension "org"
         :exclude ".*"
         :include ("index.org")
         :publishing-directory ,public-directory
         :recursive nil
         :headline-levels 4
         :auto-preamble t)
        ("site" :components ("index" "contents" "static"))))

(defmacro with-tag (tag attribute-alist &rest body)
  "Insert <TAG `attributes'>, execute BODY, then insert </TAG>.
ATTRIBUTE-ALIST's key-value pairs are converted into HTML attributes."
  (declare (indent 1) (debug t))
  (let ((attribute-list (gensym "attribute-list"))
        (attributes (gensym "attributes")))
    `(let* ((,attribute-list (cl-loop for (prob . val) in ,attribute-alist collect
                                      (concat prob "='" val "'")))
            (,attributes (string-join ,attribute-list " ")))
       (insert (concat "<" ,tag " " ,attributes ">"))
       ,@body
       (insert (concat "</" ,tag ">")))))

(defun inline-html-publish (plist filename pub-dir)
  "Publish FILENAME into `inline-html-publish-index-subpages'.
Stored as (timestamp output-filename title html-string).  Update the
`:inline-components' in PLIST with `:inline-plist'.  PUB-DIR is
ignored."
  (interactive)
  (let* ((org-inhibit-startup t)
         (visiting (find-buffer-visiting filename))
         (work-buffer (or visiting (find-file-noselect filename)))
         (output-filename (org-publish-file-relative-name
                           (file-name-with-extension filename ".html")
                           plist))
         (temp-buffer (generate-new-buffer " *temp*" t))
         (title nil))
    (with-current-buffer work-buffer
      (setq title (org-get-title))
      (org-export-to-buffer 'html temp-buffer nil nil nil t))

    (with-current-buffer temp-buffer
      (when title
        (insert (format inline-html-publish-page-format
                        (file-name-sans-extension output-filename)
                        output-filename
                        title)))
      (push `(,(org-publish-cache-mtime-of-src filename)
              ,output-filename
              ,title
              ,(buffer-string))
            inline-html-publish-index-subpages))

    (kill-buffer temp-buffer)
    (unless visiting (kill-buffer work-buffer))))

(defun inline-html-publish-index (plist filename pub-dir)
  "Build a mono-page from FILENAME and `inline-html-publish-index-subpages'.
Merges the `:inline-plist' with the plist of `:inline-components' in PLIST"
  (interactive)
  (let* ((org-inhibit-startup t)
         (org-html-preamble-format `(("en" "<h1 class='author'>%a</h1>")))
         (org-html-postamble-format `(("en" ,(concat
                                              "<p class=\"author\">Date: %T</p>"
                                              "<p class=\"author\">%c</p>"))))
         (visiting (find-buffer-visiting filename))
         (work-buffer (or visiting (find-file-noselect filename)))
         (output-filename (file-name-concat
                           pub-dir
                           (file-name-with-extension
                            (org-publish-file-relative-name filename plist)
                            ".html")))
         (html-buffer (find-file-noselect output-filename nil t))
         (inline-plist (plist-get plist :inline-plist))
         (inline-projects (cl-loop
                           for component in (plist-get plist :inline-components)
                           collect
                           (let* ((project (assoc component org-publish-project-alist))
                                  (project-name (car project))
                                  (project-plist (cdr project)))
                             (cons project-name (append inline-plist project-plist))))))
    (with-current-buffer work-buffer
      (org-export-to-buffer 'html html-buffer nil nil nil nil))

    (let ((org-publish-use-timestamps-flag nil))
      (org-publish-projects inline-projects))

    (setq inline-html-publish-index-subpages
          (mapcar #'cdr
                  (sort inline-html-publish-index-subpages
                        #'(lambda (x y) (not (time-less-p (car x) (car y)))))))

    (with-current-buffer html-buffer
      (goto-char (point-min))
      (search-forward inline-html-publish-toc-marker)
      (with-tag "ul" nil
                (cl-loop for (html-filename title html) in inline-html-publish-index-subpages do
                         (with-tag "li" nil
                                   (with-tag "a" `(("href" . ,(concat "#" (file-name-sans-extension html-filename))))
                                             (insert title)))))

      (goto-char (point-min))
      (search-forward inline-html-publish-subpage-marker)
      (cl-loop for (html-filename title html) in inline-html-publish-index-subpages do
               (with-tag "div" '(("class" . "subpage"))
                         (insert html)))
      (save-buffer))

    (kill-buffer html-buffer)
    (unless visiting (kill-buffer work-buffer))))
