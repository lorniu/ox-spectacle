;;; ox-spectacle.el --- spectacle.js Presentation Back-End for Org Export Engine

;; Copyright (C) 2018 imfine

;; Author: imfine <lorniu@gmail.com>
;; Created: 2018-11-11
;; Version: 1.0
;; Package-Requires: ((org "8.3"))
;; Keywords: presentation

;; This file is not part of GNU Emacs.

;;; Copyright Notice:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; View README.org for detail.

;;; Code:

(require 'cl-lib)
(require 'ox-html)


;;; Backend

(org-export-define-derived-backend 'spectacle 'html
  :menu-entry
  '(?s "Export *Spectacle* to HTML"
       ((?s "As buffer" ox-export-as-spectacle)
	    (?f "As file" ox-export-to-spectacle)))

  :options-alist
  '((:html-doctype nil nil ox-spec--doctype)
    (:html-table-data-tags nil nil ox-spec-table-data-tags)
    (:html-table-header-tags nil nil ox-spec-table-header-tags)
    (:pre-header "PRE_HEAD" nil ox-spec-pre-head newline)
    (:custom-header "HEAD" nil ox-spec-custom-head newline)
    (:pre-defined "PRED" nil ox-spec-custom-pred newline)
    (:code-theme "CODE_THEME" nil ox-spec-default-code-theme space)
    (:theme "THEME" nil ox-spec-default-theme space)
    (:props "PROPS" nil nil space)
    (:anim  "ANIM" nil ox-spec-default-anim))

  :translate-alist
  '((template        . ox-spec-template)
    (inner-template  . ox-spec-inner-template)
    (headline        . ox-spec-headline)
    (section         . ox-spec-section)
    (src-block       . ox-spec-src-block)
    (quote-block     . ox-spec-quote-block)
    (code            . ox-spec-code)
    (verbatim        . ox-spec-verbatim)
    ;; (plain-list      . ox-spec-plain-list)
    ;; (item            . ox-spec-item)
    (table           . ox-spec-table)
    (table-row       . ox-spec-table-row)
    (link            . ox-spec-link)
    (paragraph       . ox-spec-paragraph)
    (horizontal-rule . ox-spec-horizontal-rule))

  :filters-alist
  '((:filter-parse-tree . org-html-image-link-filter)
    (:filter-final-output . org-html-final-function)))



;;; Variables

(defgroup ox-export-spectacle nil
  "Options for exporting Orgmode files to spectacle HTML pressentations."
  :tag "Org Export Spectacle"
  :group 'org-export)

(defcustom ox-spec-default-anim "slide"
  "Default animation."
  :group 'ox-export-spectacle
  :type 'string)

(defcustom ox-spec-default-theme "{ primary: '#fefefe' }"
  "Default theme."
  :group 'ox-export-spectacle
  :type 'string)

(defcustom ox-spec-default-code-theme "{ backgroundColor: '#2a2734', color: '#9a86fd' }"
  "Theme for code pane."
  :group 'ox-export-spectacle
  :type 'string)

(defcustom ox-spec-pre-head "
    <style>
      .org-pre-container {
          text-align: left;
          margin: auto;
          font-size: 1.2rem;
          font-weight: normal;
          min-width: 100%%;
      }
      pre {
          font-family: monospace;
          line-height: 1.8;
          direction: ltr;
          text-align: left;
          word-spacing: normal;
          word-break: normal;
          tab-size: 2;
          -webkit-hyphens: none;
          -moz-hyphens: none;
          -ms-hyphens: none;
          hyphens: none;
          white-space: pre-wrap;
          padding: 0.7rem;
          margin: 0;
      }
      .org-ul, .org-ol {
          text-align: left;
          line-height: 1.3;
      }
      .org-ul > li, .org-ol > li {
          font-size: 2.66rem;
      }
      .org-dl > dt {
          marginBottom: 1em;
          font-size: 3rem;
          font-weight: bold;
      }
    </style>"
    "Html head, js/css etc."
  :group 'ox-export-spectacle
  :type 'string)

(defcustom ox-spec-custom-head ""
  "Head append `ox-spec-custom-head'."
  :group 'ox-export-spectacle
  :type 'string)

(defcustom ox-spec-custom-pred ""
  "Scripts evel before Component return."
  :group 'ox-export-spectacle
  :type 'string)

;;; inner

(defvar ox-spec--template "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width initial-scale=1 user-scalable=no\" />
    <title>Spectacle</title>
    <link href=\"https://fonts.googleapis.com/css?family=Lobster+Two:400,700\" rel=\"stylesheet\" type=\"text/css\">
    <link href=\"https://fonts.googleapis.com/css?family=Open+Sans+Condensed:300,700\" rel=\"stylesheet\" type=\"text/css\">
    <link href=\"https://unpkg.com/normalize.css@7/normalize.css\" rel=\"stylesheet\" type=\"text/css\">
    <script>
    var tcss = `%s
    <style>
    .code-theme {
       %s
    }
    </style>
    %s
    `;
    window.addEventListener('load', function () {
        setTimeout(function() {
          document.querySelector('head').insertAdjacentHTML('beforeend', tcss);
        }, 1);
    });
    </script>
</head>
<body>
    <div id=\"root\"></div>
    <script src=\"https://unpkg.com/prop-types@15/prop-types.js\"></script>
    <script src=\"https://unpkg.com/react@16/umd/react.production.min.js\"></script>
    <script src=\"https://unpkg.com/react-dom@16/umd/react-dom.production.min.js\"></script>
    <script src=\"https://unpkg.com/@babel/standalone/babel.js\"></script>
    <script src=\"https://unpkg.com/spectacle@^4/dist/spectacle.js\"></script>
    <script src=\"https://unpkg.com/spectacle@^4/lib/one-page.js\"></script>
    <script type=\"text/spectacle\">
      () => {
        const { themes: { defaultTheme } } = Spectacle;
        const theme = defaultTheme(%s);

%s

        return (
          %s
        );
      }
    </script>
</body>
</html>")

(defvar ox-spec-table-data-tags '("<TableItem%s>" . "</TableItem>"))

(defvar ox-spec-table-header-tags '("<TableHeaderItem scope=\"%s\"%s>" . "</TableHeaderItem>"))

(defvar ox-spec--doctype "xhtml")

(defvar ox-spec--valid-tags
  '("Appear" "BlockQuote" "Cite" "CodePane" "Code" "ComponentPlayground" "Deck" "Fill" "Fit" "Heading" "Image"
    "GoToAction" "Layout" "Link" "ListItem" "List" "Magic" "Markdown" "MarkdownSlides" "Notes" "Quote" "S" "Slide"
    "SlideSet" "TableBody" "TableHeader" "TableHeaderItem" "TableItem" "TableRow" "Table" "Text" "Typeface" "themes"))



;;; Transcoders

(defun ox-spec-template (body info)
  "Return complete document string after HTML conversion.
BODY is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((anim (plist-get info :anim))
         (magicp (string-match-p "[Mm]agic" anim))
         (props
          (concat " theme={theme} "
                  (plist-get info :props)
                  (unless magicp
                    (let ((ps (split-string anim " ")))
                      (format " transition={[%s]} transitionDuration='%s'"
                              (mapconcat 'identity (mapcar (lambda (s) (concat "'" s "'")) (split-string (car ps) "/")) ",")
                              (or (cadr ps) 1000))))))
         (body-ret
          (format ox-spec--template
                  (plist-get info :pre-header)
                  (ox-spec--normalize-json-to-css (plist-get info :code-theme))
                  (plist-get info :custom-header)
                  (plist-get info :theme)
                  (plist-get info :pre-defined)
                  (concat
                   "\n<Deck" props ">"
                   (if magicp "<Magic>\n")
                   body
                   (if magicp "\n</Magic>\n")
                   "\n</Deck>"))))
    body-ret))

(defun ox-spec-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  contents)

(defun ox-spec-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((case-fold-search nil)
         (title (org-export-data (org-element-property :title headline) info))
         (level (+ (org-export-get-relative-level headline info)
                   (1- (plist-get info :html-toplevel-hlevel))))
         (notes (org-element-property :NOTES headline))
         (divs  (org-element-property :DIVS headline))
         (props (concat (org-element-property :PROPS headline) (if notes (format " notes=\"%s\"" notes)))))
    (if (<= level 2)
        (let ((id (concat " id='p" (number-to-string (car (org-export-get-headline-number headline info))) "'")))
          (if divs
              (concat "<Slide" id (ox-spec-wa props) ">\n<div" (ox-spec-wa divs) ">\n" contents "</div>\n</Slide>")
            (concat "<Slide" id (ox-spec-wa props) ">\n" contents "\n</Slide>")))
      (cond ((string= title "nil") contents)
            ((string= title "Appear")
             (concat "<Appear" (ox-spec-wa props) ">\n<div" (ox-spec-wa divs) ">\n" contents "\n</div></Appear>"))
            ((or (string-match-p "^[a-z]\\{1,7\\}[1-9]?$" title)
                 (member title ox-spec--valid-tags))
             (concat "<" title (ox-spec-wa props) ">\n" contents "</" title ">"))
            (t (concat "<div" (ox-spec-wa props) (ox-spec-wa divs) ">\n" contents "</div>"))))))

(defun ox-spec-section (_section contents _info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)

(defun ox-spec-src-block (src-block _content info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((lang (org-element-property :language src-block))
        (code (org-html-format-code src-block info))
        (attributes
         (org-html--make-attribute-string
          (org-export-read-attribute :attr_html src-block))))
    (format "<div className='org-pre-container'>\n<pre className='org-pre code-theme'%s dangerouslySetInnerHTML={{__html: `%s`}}></pre>\n</div>"
            (ox-spec-wa attributes)
            (replace-regexp-in-string "{" "&#123;" code))))

(defun ox-spec-quote-block (quote-block contents _info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((attributes (org-export-read-attribute :attr_html quote-block))
         (cite (plist-get attributes :cite))
         (a (ox-spec-wa (org-html--make-attribute-string (ox-spec--plist-delete attributes :cite)))))
    (format "<div>\n<BlockQuote>\n<Quote%s>\n%s</Quote>\n%s</BlockQuote>\n</div>"
            a contents (if cite (concat "<Cite>" cite "</Cite>\n") ""))))

(defun ox-spec-code (code _contents info)
  "Transcode CODE from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "<Code>%s</Code>" (org-html-encode-plain-text (org-element-property :value code))))

(defun ox-spec-verbatim (verbatim _contents info)
  "Transcode VERBATIM from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "<Code>%s</Code>" (org-html-encode-plain-text (org-element-property :value verbatim))))

(defun ox-spec-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((headline (org-export-get-parent-headline plain-list))
         (level (+ (org-export-get-relative-level headline info)
                   (1- (plist-get info :html-toplevel-hlevel))))
         (ordered (eq (org-element-property :type plain-list) 'ordered))
         (attributes (org-export-read-attribute :attr_html plain-list)))
    (format "<List%s %s>\n%s</List>\n"
            (if ordered " ordered" "")
            (org-html--make-attribute-string attributes)
            contents)))

(defun ox-spec-item (item contents info)
  "Transcode an ITEM element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (concat "<ListItem>"
          (and (string-match-p "^<" contents) "\n")
          (org-trim contents)
          (and (string-match-p "^<" contents) "\n")
          "</ListItem>"))

(defun ox-spec-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (let ((attributes
	     (org-html--make-attribute-string (org-export-read-attribute :attr_html table))))
    (format "\n<div>\n<Table%s>\n%s</Table>\n</div>\n"
            (ox-spec-wa attributes) contents)))

(defun ox-spec-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to HTML.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((group (org-export-table-row-group table-row info))
	       (number (org-export-table-row-number table-row info))
	       (start-group-p
	        (org-export-table-row-starts-rowgroup-p table-row info))
	       (end-group-p
	        (org-export-table-row-ends-rowgroup-p table-row info))
	       (topp (and (equal start-group-p '(top))
		              (equal end-group-p '(below top))))
	       (bottomp (and (equal start-group-p '(above))
			             (equal end-group-p '(bottom above))))
           (row-open-tag
            (pcase "<TableRow>"
              ((and accessor (pred functionp))
               (funcall accessor number group start-group-p end-group-p topp bottomp))
	          (accessor accessor)))
           (row-close-tag
            (pcase "</TableRow>"
              ((and accessor (pred functionp))
               (funcall accessor
			            number group start-group-p end-group-p topp bottomp))
	          (accessor accessor)))
	       (group-tags
	        (cond
	         ((not (= 1 group)) '("\n<TableBody>" . "\n</TableBody>"))
	         ((org-export-table-has-header-p
	           (org-export-get-parent-table table-row) info)
	          '("<TableHeader>" . "\n</TableHeader>"))
	         (t '("\n<TableBody>" . "\n</TableBody>")))))
      (concat (and start-group-p (car group-tags))
	          (concat "\n" row-open-tag contents "\n" row-close-tag)
	          (and end-group-p (cdr group-tags))))))

(defun ox-spec-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((link-org-files-as-html-maybe
	      (lambda (raw-path info)
	        ;; Treat links to `file.org' as links to `file.html', if needed.  See `org-html-link-org-files-as-html'.
	        (cond
	         ((and (plist-get info :html-link-org-files-as-html)
		           (string= ".org" (downcase (file-name-extension raw-path "."))))
	          (concat (file-name-sans-extension raw-path) "." (plist-get info :html-extension)))
	         (t raw-path))))
	     (type (org-element-property :type link))
	     (raw-path (org-element-property :path link))
	     ;; Ensure DESC really exists, or set it to nil.
	     (desc (org-string-nw-p desc))
	     (path
	      (cond
	       ((member type '("http" "https" "ftp" "mailto" "news"))
	        (url-encode-url (org-link-unescape (concat type ":" raw-path))))
	       ((string= type "file")
	        (setq raw-path
		          (org-export-file-uri
		           (org-publish-file-relative-name raw-path info)))
	        ;; Possibly append `:html-link-home' to relative file name.
	        (let ((home (and (plist-get info :html-link-home) (org-trim (plist-get info :html-link-home)))))
	          (when (and home (plist-get info :html-link-use-abs-url) (file-name-absolute-p raw-path))
		        (setq raw-path (concat (file-name-as-directory home) raw-path))))
	        ;; Maybe turn ".org" into ".html".
	        (setq raw-path (funcall link-org-files-as-html-maybe raw-path info))
	        ;; Add search option, if any.
	        (let ((option (org-element-property :search-option link)))
	          (cond ((not option) raw-path)
		            ((eq (string-to-char option) ?#) (concat raw-path option))
		            (t (concat raw-path "#" (org-publish-resolve-external-link option (org-element-property :path link)))))))
	       (t raw-path)))
	     ;; Extract attributes from parent's paragraph.
	     (attributes-plist
	      (let* ((parent (org-export-get-parent-element link))
		         (link (let ((container (org-export-get-parent link)))
			             (if (and (eq (org-element-type container) 'link)
				                  (org-html-inline-image-p link info))
			                 container
			               link))))
	        (and (eq (org-element-map parent 'link 'identity info t) link)
		         (org-export-read-attribute :attr_html parent))))
	     (attributes
	      (let ((attr (org-html--make-attribute-string attributes-plist)))
	        (if (org-string-nw-p attr) (concat " " attr) ""))))
    (cond
     ((string-match "^slide:\\([a-zA-Z0-9]+\\)" path)
      (format "<GoToAction slide={'%s'}>%s</GoToAction>" (match-string-no-properties 1 path) desc))
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'html))
     ;; Image file.
     ((and (plist-get info :html-inline-images)
	       (org-export-inline-image-p link (plist-get info :html-inline-image-rules)))
      (if (string= "svg" (file-name-extension path))
          (let ((attrs (org-html--make-attribute-string
		                (org-combine-plists '(:class "org-svg") attributes-plist '(:fallback nil)))))
            (format "\n<img src=\"%s\" alt=\"svg not support\"%s></img>\n" path (ox-spec-wa attrs)))
        (org-html-close-tag
         "Image"
         (org-html--make-attribute-string
          (org-combine-plists
           (list :src path
	             :alt (if (string-match-p "^ltxpng/" path)
		                  (org-html-encode-plain-text
		                   (org-find-text-property-in-string 'org-latex-src path))
		                (file-name-nondirectory path)))
           attributes-plist))
         info)))
     ;; Radio target: Transcode target's contents and use them as link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	    (if (not destination) desc
	      (format "<Link href=\"#%s\"%s>%s</Link>"
		          (org-export-get-reference destination info) attributes desc))))
     ;; Links pointing to a headline
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			                 (org-export-resolve-fuzzy-link link info)
			               (org-export-resolve-id-link link info))))
	    (pcase (org-element-type destination)
	      (`plain-text
	       (let ((path (funcall link-org-files-as-html-maybe destination info)))
	         (format "<Link href=\"%s#%s\"%s>%s</Link>" path (concat "ID-" path) attributes (or desc destination))))
	      (`nil (format "<i>%s</i>"
		                (or desc (org-export-data (org-element-property :raw-link link) info))))
	      (`headline
	       (let ((href (or (org-element-property :CUSTOM_ID destination) (org-export-get-reference destination info)))
		         (desc (if (and (org-export-numbered-headline-p destination info) (not desc))
		                   (mapconcat #'number-to-string (org-export-get-headline-number destination info) ".")
		                 (or desc (org-export-data (org-element-property :title destination) info)))))
	         (format "<Link href=\"#%s\"%s>%s</Link>" href attributes desc)))
	      (_
	       (let* ((ref (org-export-get-reference destination info))
		          (org-html-standalone-image-predicate #'org-html--has-caption-p)
		          (number (cond
			               (desc nil)
			               ((org-html-standalone-image-p destination info)
			                (org-export-get-ordinal
			                 (org-element-map destination 'link
			                   #'identity info t)
			                 info 'link 'org-html-standalone-image-p))
			               (t (org-export-get-ordinal
			                   destination info nil 'org-html--has-caption-p))))
		          (desc (cond (desc)
			                  ((not number) "No description for this link")
			                  ((numberp number) (number-to-string number))
			                  (t (mapconcat #'number-to-string number ".")))))
	         (format "<Link href=\"#%s\"%s>%s</Link>" ref attributes desc))))))
     ;; External link with a description part.
     ((and path desc) (format "<Link href=\"%s\"%s>%s</Link>"
			                  (org-html-encode-plain-text path) attributes desc))
     ;; External link without a description part.
     (path (let ((path (org-html-encode-plain-text path)))
	         (format "<Link href=\"%s\"%s>%s</Link>" path attributes (org-link-unescape path))))
     ;; No path, only description.  Try to do something useful.
     (t (format "<i>%s</i>" desc)))))

(defun ox-spec-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let* ((parent-type (org-element-type (org-export-get-parent paragraph)))
         (headline (org-export-get-parent-headline paragraph))
	     (style '((footnote-definition " class=\"footpara\"") (org-data " class=\"footpara\"")))
	     (attributes (org-export-read-attribute :attr_html paragraph))
         (raw-value (org-element-property :raw-value headline))
	     (extra (or (cadr (assq parent-type style)) "")))
    (if (or (and (eq parent-type 'item) ;; First paragraph in an item has no tag if it is alone or followed, at most, by a sub-list.
	             (not (org-export-get-previous-element paragraph info))
	             (let ((followers (org-export-get-next-element paragraph info 2)))
	               (and (not (cdr followers)) (memq (org-element-type (car followers)) '(nil plain-list)))))
            (eq parent-type 'quote-block)
            (string-match-p "^h[0-9]" (or raw-value ""))
            (org-html-standalone-image-p paragraph info)) ;; Standalone image.
        contents
      ;; Regular paragraph.
      (let* ((font (plist-get attributes :font))
             (gfont (plist-get attributes :gfont))
             (weight (plist-get attributes :weight))
             (type (plist-get attributes :type))
             (font-string (cond (font (concat " font='" font "'"))
                                (gfont (concat " googleFont='" gfont "'"))
                                (t nil)))
             (attributes-remain (org-html--make-attribute-string
                                 (ox-spec--plist-delete attributes :font :gfont :weight :type)))
             (text-tag (cond ((and type (string-match "^h\\([0-9]\\)\\(.*\\)$" type))
                              (cons (format "Heading size={%s}%s"
                                            (match-string 1 type)
                                            (match-string 2 type))
                                    "Heading"))
                             ((and type (string= type "Appear"))
                              (cons "Appear><div" "div></Appear"))
                             (type (save-match-data
                                     (string-match "^\\([^ ]+\\).*" type)
                                     (cons type (match-string 1 type))))
                             ((or (null raw-value) (not (org-string-nw-p raw-value)))
                              (cons "p" "p"))
                             (t (cons "Text" "Text"))))
             (text (format "<%s%s%s>\n%s</%s>"
                           (car text-tag)
                           (ox-spec-wa attributes-remain)
                           (ox-spec-wa extra)
                           contents (cdr text-tag))))
        (if font-string
            (concat "<Typeface" font-string (if weight (format " weight={%s}" weight)) ">\n" text "\n</Typeface>\n")
          text)))))

(defun ox-spec-horizontal-rule (horizontal-rule _contents info)
  "Transcode an HORIZONTAL-RULE  object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((outer-tags '(:margin :marginTop :marginBottom))
         (attributes (org-export-read-attribute :attr_html horizontal-rule))
         (divs (concat
                "style={{"
                (mapconcat
                 'identity
                 (delete nil
                         (mapcar
                          (lambda (x)
                            (if (plist-member attributes x)
                                (format "%s: '%s'" (substring (symbol-name x) 1) (plist-get attributes x))
                              )) outer-tags)) ", ") "}}")))
    (format "<div%s><hr%s /></div>"
            (ox-spec-wa divs)
            (ox-spec-wa (org-html--make-attribute-string
                         (ox-spec--plist-delete attributes outer-tags outer-tags))))))



;;; Helper functions

(defun ox-spec--normalize-css-to-json (css)
  "Convert CSS: color: #8b2252; font-weight: bold; => { json object }."
  (let* ((case-fold-search nil)
         (css-json-inner
          (mapconcat
           (lambda (css-item)
             (let* ((css-arr (split-string css-item ":"))
                    (key (org-trim (car css-arr)))
                    (value (org-trim (cadr css-arr))))
               (format "%s: '%s'"
                       (replace-regexp-in-string
                        "-" "" (replace-regexp-in-string "-[a-z]" 'upcase key)) value)))
           (split-string (replace-regexp-in-string "; *$" "" css) ";")
           ", ")))
    (concat "{" css-json-inner "}")))

(defun ox-spec--normalize-json-to-css (json)
  "Convert JSON: { fontColor: 'red', size: 23 } => common css string."
  (let* ((case-fold-search nil)
         (items (split-string (replace-regexp-in-string "^ *{\\|} *$" "" json) ",")))
    (mapconcat (lambda (item)
                 (let ((pair (split-string item ":")))
                   (concat (downcase (replace-regexp-in-string "\\([A-Z]\\)" "-\\1" (car pair) t))
                           ":"
                           (replace-regexp-in-string "['\"]" "" (cadr pair)))))
               items "; ")))

(defun ox-spec--plist-delete (plist &rest properties)
  "Delete PROPERTIES from PLIST."
  (let* (p (properties (if (listp (car properties)) (car properties) properties)))
    (while plist
      (if (not (memq (car plist) properties))
	      (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun ox-spec-wa (attr)
  "Normalize ATTR to ' xxx' style."
  (if attr
      (let ((a (org-trim attr)))
        (if (org-string-nw-p a) (concat " " a) ""))
    ""))



;;; Advice functions

(defun ox-spec--make-attribute-string (attributes)
  "Override ‘org-html--make-attribute-string’, make ATTRIBUTES a string."
  (let (output)
    (dolist (item attributes (mapconcat 'identity (nreverse output) " "))
      (cond ((null item) (pop output))
            ((symbolp item) (push (substring (symbol-name item) 1) output))
            (t (let ((key (car output))
                     (value (replace-regexp-in-string "\"" "&quot;" (org-html-encode-plain-text item) t)))
                 (setcar output (format (if (string-match-p "^{.*}$" value) "%s=%s" "%s=\"%s\"")
                                        key value))))))))

(defmacro ox-spec-advice (&rest body)
  "Add advices to BODY."
  `(cl-letf (((symbol-function 'string-trim) 'org-trim)
             ((symbol-function 'org-html--make-attribute-string) 'ox-spec--make-attribute-string))
     ,@body))



;;; End-user functions

(defun ox-export-as-spectacle (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (ox-spec-advice
   (org-export-to-buffer 'spectacle "*Org Spectacle Export*"
     async subtreep visible-only body-only ext-plist (lambda () (set-auto-mode t)))))

(defun ox-export-to-spectacle (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :html-extension) org-html-extension "html")))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system 'utf-8))
    (ox-spec-advice (org-export-to-file 'spectacle file async subtreep visible-only body-only ext-plist))))

(defun ox-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (ox-spec-advice
   (org-publish-org-to 'spectacle filename
                       (concat "." (or (plist-get plist :html-extension) org-html-extension "html"))
                       plist pub-dir)))

(defun spectacle-help ()
  "Options:

  - #+PRE_HEAD (ox-spec-pre-css, pure css)
  - #+HEAD: (embed to html)
  - #+PRED: (run before return JSX)
  - #+THEME: { primary: \"lightgreen\", secondary: \"#222222\" },
             { primary: \"Helvetica\", }
  - #+PROPS: contentWidth={1000} bgImage={xxx.jpg}
  - #+ANIM: fade/zoom 1000
  - #+CODE_THEME: { backgroundColor: 'white', fontSize: '30px' }

---------------

Ways to add style:

   - global pre css: #+PRE_CSS: ...
   - append pre css: #+CSS: ...
   - with props:     #+props: bgImage={...} style={{...}}
   - with attr_html: #+HTML_ATTR: :bgImage 'xxx' :style {{...}}
   - JS Define:      #+JS: const s1 = { color: 'red', backgroundColor: 'green' }
   -      then:      #+HTML_ATTR: :bgImage 'xxx' :style {s1}

---------------

Transition Defined Sample:

transition={[
  'fade',
  (trans, forward) => ({
    transform: `
      translate3d(0%, ${trans ? 100 : 0}%, 0)
      rotate(${trans ? (forward ? -180 : 180) : 0}deg)
    `,
    backgroundColor: trans ? '#26afff' : '#000'
  })
]}

--------------

To be continued..."
  (interactive)
  (describe-function 'spectacle-help))


(provide 'ox-spectacle)
;;; ox-spectacle ends here
