;;; ox-spectacle.el --- Spectacle.js Presentation Back-End for Org Export Engine -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; Created: 2018-11-11
;; URL: https://github.com/lorniu/ox-spectacle
;; Package-Requires: ((emacs "28.1") (org "8.3"))
;; Keywords: convenience
;; Version: 2.0

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

;; Spectacle.js is the best tool to create slides with html5, and
;; this ox-spectacle.el is the best way to create slides with Spectacle.js.
;;
;; Org-Mode + React, powerful! Have a try, you will like it :)
;;
;; First, open your Emacs.
;;
;; Then, install this and load it:
;;
;;   (require 'ox-spectacle)
;;
;; That's all.
;;
;; Create one org file, put your ideas there.
;; Then export them to a html file and open it with `C-c C-e s o`.
;;
;; The amazing presentation is in front of you. Enjoy it.
;;
;; View README.md for detail.

;;; Code:

(require 'cl-lib)
(require 'ox-html)


;;; Backend

(org-export-define-derived-backend 'spectacle 'html
  :menu-entry
  '(?s "Export to Spectacle.js Presentation"
       ((?S "As buffer" ox-spectacle-export-to-buffer)
        (?s "As html file" ox-spectacle-export-to-file)
        (?o "As html file and open" ox-spectacle-export-to-file-and-browser)))

  :options-alist
  '((:theme                  "THEME"             nil ox-spectacle-theme)
    (:template               "TEMPLATE"          nil ox-spectacle-template)
    (:transition             "TRANSITION"        nil ox-spectacle-transition)
    (:deck-opts              "DECK_OPTS"         nil ox-spectacle-deck-opts space)
    (:slide-opts             "SLIDE_OPTS"        nil ox-spectacle-slide-opts space)
    (:text-opts              "TEXT_OPTS"         nil ox-spectacle-text-opts space)
    (:export-level           "EXPORT_LEVEL"      0   ox-spectacle-export-level)
    (:extern-components      "EXTERN_COMPONENTS" nil ox-spectacle-extern-components split)
    (:scripts                "SCRIPTS"           nil ox-spectacle-scripts split)
    (:extra-scripts          "EXTRA_SCRIPTS"     nil ox-spectacle-extra-scripts split)
    (:split                  "SPLIT"             nil) ; keep for capf
    (:with-special-strings   nil                 nil) ; don't escape ...
    (:html-doctype           nil                 nil ox-spectacle--doctype))

  :translate-alist
  '((template        . ox-spectacle--template)
    (inner-template  . ox-spectacle--inner-template)
    (headline        . ox-spectacle--headline)
    (section         . ox-spectacle--section)
    (src-block       . ox-spectacle--src-block)
    (example-block   . ox-spectacle--example-block)
    (verse-block     . ox-spectacle--verse-block)
    (special-block   . ox-spectacle--special-block)
    (fixed-width     . ox-spectacle--fixed-width)
    (quote-block     . ox-spectacle--quote-block)
    (center-block    . ox-spectacle--center-block)
    (code            . ox-spectacle--code)
    (verbatim        . ox-spectacle--verbatim)
    (plain-list      . ox-spectacle--plain-list)
    (item            . ox-spectacle--item)
    (table           . ox-spectacle--table)
    (table-row       . ox-spectacle--table-row)
    (table-cell      . ox-spectacle--table-cell)
    (link            . ox-spectacle--link)
    (paragraph       . ox-spectacle--paragraph)
    (plain-text      . ox-spectacle--plain-text)
    (latex-fragment  . ox-spectacle--latex-fragment)
    (keyword         . ox-spectacle--keyword))

  :filters-alist
  '((:filter-options . ox-spectacle--init-filter)
    (:filter-parse-tree . ox-spectacle--parse-tree-filter)
    (:filter-final-output . org-html-final-function)))


;;; Variables

(defgroup ox-spectacle nil
  "Options for exporting OrgMode files to spectacle HTML pressentations."
  :tag "Org Export Spectacle"
  :prefix 'ox-spectacle-
  :group 'org-export)

(defcustom ox-spectacle-theme nil
  "Default theme."
  :type 'string)

(defcustom ox-spectacle-template "template"
  "Default template."
  :type 'string)

(defcustom ox-spectacle-transition nil
  "Default transition ."
  :type 'string)

(defcustom ox-spectacle-deck-opts nil
  "Deck tag or extra props."
  :type 'string)

(defcustom ox-spectacle-slide-opts nil
  "Slide tag or extra props."
  :type 'string)

(defcustom ox-spectacle-text-opts nil
  "Text tag or extra props, used for plain paragraph."
  :type 'string)

(defcustom ox-spectacle-export-level 0
  "Export policy.
0 for normal, 1 for embed scripts, 2 for embed images,
3 for embed all scripts and images, that is, all-in-one/self-contained."
  :type 'integer)

(defcustom ox-spectacle-scripts
  (list "https://unpkg.com/react@18.1.0/umd/react.production.min.js"
        "https://unpkg.com/react-dom@18.1.0/umd/react-dom.production.min.js"
        "https://unpkg.com/react-is@18.1.0/umd/react-is.production.min.js"
        "https://unpkg.com/prop-types@15.7.2/prop-types.min.js"
        "https://unpkg.com/spectacle@^9/dist/spectacle.min.js"
        "https://unpkg.com/htm")
  "Core scripts.
The react, react-dom, react-js, spectacle and htm are required!
You can replace them with your CDN version or local version."
  :type '(repeat string))

(defcustom ox-spectacle-extra-scripts nil
  "Other scripts you maybe used in the slides.
The your-own or third-party scripts should set here to make the embeded export
policy working. You can set this on a per-file basis using #+EXTRA_SCRIPTS:."
  :type '(repeat string))

(defcustom ox-spectacle-initial-style "
.codepane {}
.split-container {
  display: flex; flex-flow: column nowrap; justify-content: flex-start;
  align-items: stretch; height: inherit; max-height: 100vh;
}
::-webkit-scrollbar { width: 5px; height: 80%; }
::-webkit-scrollbar-track { background: rgb(179, 177, 177); border-radius: 5px; }
::-webkit-scrollbar-thumb { background: rgb(136, 136, 136); border-radius: 5px; }
::-webkit-scrollbar-thumb:hover  { background: rgb(100, 100, 100); border-radius: 5px; }
::-webkit-scrollbar-thumb:active { background: rgb(68, 68, 68); border-radius: 5px; }
"
  "CSS style add to the slides by default."
  :type 'string)

(defcustom ox-spectacle-extern-components nil
  "Declare the components you create or import to make the parser work properly."
  :type '(repeat string))

(defcustom ox-spectacle-default-template
  "({ slideNumber, numberOfSlides }) => html`
      <${FlexBox} position='absolute' bottom=${0} right=${0} opacity=${0.3}>
        <${Progress} size=${8} />
        <${Text} fontSize=${15}>${slideNumber}/${numberOfSlides}</${Text}>
      </${FlexBox}>`"
  "Template definition named as `template', as the default template of the slides."
  :type 'string)

(defvar ox-spectacle-cache-file-tpl (locate-user-emacs-file "ox-spectacle-scripts-%s.js")
  "Location of the cache file.")

(defvar ox-spectacle--doctype "xhtml")

(defvar ox-spectacle--images nil)
(defvar ox-spectacle--frags-css nil)
(defvar ox-spectacle--frags-script nil)
(defvar ox-spectacle--frags-header nil)
(defvar ox-spectacle--frags-html nil)
(defvar ox-spectacle--frags-template nil)

(defconst ox-spectacle--html-tags
  '("h1" "h2" "h3" "h4" "h5" "div" "section" "p" "span" "canvas"
    "small" "ul" "ol" "li" "hr" "a" "img" "button"))

(defconst ox-spectacle--components
  '("Deck" "Slide" "SlideContext" "SlideLayout"
    "Box" "FlexBox" "Grid"
    "Heading" "Text" "Link" "Image" "FullSizeImage" "SpectacleLogo"
    "UnorderedList"  "OrderedList" "ListItem"
    "Table" "TableCell" "TableRow" "TableHeader" "TableBody"
    "CodePane" "CodeSpan" "Quote"
    "Markdown" "MarkdownSlideSet" "MarkdownSlide" "MarkdownPreHelper"
    "Appear" "Stepper"
    "SpectacleTheme" "SpectacleThemeOverrides"
    "CommandBar" "FullScreen" "Progress" "AnimatedProgress" "Notes"))

(defconst ox-spectacle--utils
  '("defaultTheme" "fadeTransition" "slideTransition" "defaultTransition"
    "useSteps" "useMousetrap"
    "mdxComponentMap" "indentNormalizer"
    "removeNotes" "isolateNotes"))

(defconst ox-spectacle--page-html
  "<html>
<head>
  <meta charset='UTF-8' />
  <meta name='viewport' content='width=device-width, initial-scale=1' />
  <meta http-equiv='X-UA-Compatible' content='IE=edge,chrome=1' />
  <title>%s</title>

%s%s%s%s%s
</head>

<body>
%s
</body>
</html>")

(defconst ox-spectacle--page-model-script
  "  <script type='module'>

    /* imports */

    const { %s } = Spectacle;
    const { %s } = Spectacle;
    const html = htm.bind(React.createElement);

    /* other components */

    const MyLink = React.forwardRef((props, ref) => {
       // TODO: location not refresh... why?
       const id = props.id;
       if (/\\d+/.test(id)) {
           const { skipTo } = React.useContext(Spectacle.DeckContext);
           return html`<${Link} ref=${ref} onClick=${() => {event.preventDefault();skipTo({slideIndex: id})}} ...${props}></${Link}>`;
       }
       return html`<${Link} ...${props} ref=${ref}></${Link}>`;
    });

    /* template */

    let template = %s;
%s%s

    /* presentation definition begin */

    const Presentation = () => html`<${%s}%s%s%s%s>\n
%s\n
</${%s}>`;

    /* presentation definition finished */

    ReactDOM.createRoot(document.getElementById('root')).render(html`<${Presentation}/>`);

    /* presentation rendered, all finished */

  </script>")


;;; Utils

(defun ox-spectacle--export-level (info)
  "Return the export level.
Make sure the type is number. INFO is a plist holding export options."
  (let ((lv (plist-get info :export-level)))
    (if (stringp lv) (string-to-number lv) lv)))

(defun ox-spectacle--fetch-content (path)
  "Return the content of PATH, that is a file or url."
  (let ((urlp (string-match-p "^\\(http\\|ftp\\)" path)))
    (condition-case err
        (with-temp-buffer
          (if urlp
              (url-insert-file-contents-literally path)
            (insert-file-contents-literally path))
          (buffer-string))
      (error (user-error "Get content failed for %s %s" path (cdr err))))))

(defun ox-spectacle--make-scripts-section (info &optional forcenew)
  "Generate the <scripts> part of final html.
If level is 1 or 3 then embeded the contents into html. Fetch the contents
from the url or path, cache them if nessesary. FORCENEW the cache when
it's non-nil. INFO is a plist holding export options."
  (let ((lv (ox-spectacle--export-level info))
        (scripts (append
                  (plist-get info :scripts)
                  (plist-get info :extra-scripts))))
    (if (or (= lv 1) (>= lv 3))
        (let* ((name (md5 (mapconcat #'identity scripts)))
               (file (format ox-spectacle-cache-file-tpl name)))
          (when (or forcenew (not (file-exists-p file)))
            (condition-case nil
                (with-temp-file file
                  (dolist (path scripts)
                    (insert (ox-spectacle--fetch-content path))
                    (goto-char (point-max))
                    (insert "\n"))
                  (message "Caching scripts to %s" file))
              (error (delete-file file)
                     (user-error "Fetch scripts failed"))))
          (with-temp-buffer
            (insert-file-contents file)
            (format "  <script>\n%s\n</script>" (string-trim (buffer-string)))))
      (cl-loop for s in scripts
               concat (format "\n<script src='%s'></script>" s)))))

(defun ox-spectacle--make-images-section ()
  "Make a variable contain all data of embeded images.
This can improve the duplicate images."
  (cl-loop for (k . v) in ox-spectacle--images
           collect (format "\"%s\":\"%s\"" k v) into lst
           finally (return
                    (when lst
                      (format "<script>\nconst _images = {\n%s\n};
const _mkUrl = (key) => 'url(' + _images[key] + ')';\n</script>"
                              (mapconcat #'identity lst ",\n"))))))

(defun ox-spectacle--data-uri (path)
  "Generate the inline data used in html for PATH.
If PATH is a remote url, download it."
  (let* ((ext (car (split-string (file-name-extension path) "?")))
         (content (ox-spectacle--fetch-content path))
         (svgp (string-equal "svg" ext))
         (type (if svgp "svg+xml;charset=utf-8" (concat ext ";base64")))
         (data (if svgp (string-replace "#" "%23" (string-replace "\"" "'" content))
                 (base64-encode-string content 'no-line-break))))
    (format "data:image/%s,%s" type data)))

(defun ox-spectacle--props-inline-image (props info)
  "Replace all image url in PROPS to inline data.
If url is remote, download it! INFO is a plist holding contextual information."
  (if (and info
           (>= (ox-spectacle--export-level info) 2)
           (string-match-p "url(.*)" props))
      (replace-regexp-in-string
       "\\(.\\)\\(['\"]?\\)url(\\([^)]+\\))['\"]?"
       (lambda (old)
         (save-match-data
           (let ((t1 (match-string 1 old))
                 (url (match-string 3 old)))
             (if (string-prefix-p "data:" url) old
               (let ((key (md5 url)))
                 (unless (alist-get key ox-spectacle--images nil nil #'string=)
                   (setf (alist-get key ox-spectacle--images nil nil #'string=)
                         (ox-spectacle--data-uri url)))
                 (if (string= t1 "=")
                     (format "%s${_mkUrl(\"%s\")}" t1 key)
                   (format "%s_mkUrl(\"%s\")" t1 key)))))))
       props)
    props))

(defun ox-spectacle--props-compat-react-htm (props)
  "Make PROPS compat with react-htm syntax, that is, add $ if nessesary."
  (if props (replace-regexp-in-string "\\(=\\|\\.\\.\\.\\){" "\\1${" props)))

(defun ox-spectacle--filter-props (props &optional info)
  "Apply all filters to PROPS to make it compat with Spectacle.
INFO is a plist holding contextual information."
  (setq props (ox-spectacle--props-compat-react-htm props))
  (when info
    (setq props (ox-spectacle--props-inline-image props info)))
  (ox-spectacle--wa props))

(defun ox-spectacle--extract-options (opts info &optional normed)
  "Parse the value of OPTS from INFO.
OPTS is keyword like :deck-opts, its value is consists of tags and props.
Parse and return them as cons. If NORMED non-nil, apply the filters on props."
  (setq opts (org-export-data (plist-get info opts) info))
  (when (> (length opts) 0)
    (let (tag props (case-fold-search nil))
      (let ((tg (car (split-string opts))))
        (when (and tg (string-match-p "^\\([A-Z][^=]+\\|[a-z][a-z0-9]*\\)$" tg))
          (setq tag tg)))
      (setq props (string-trim (cl-subseq opts (length tag))))
      (when normed
        (setq props (ox-spectacle--filter-props props info)))
      (cons tag (if (> (length props) 0) props)))))

(defun ox-spectacle--available-components (&optional info)
  "All components available.
Get from INFO or parse and get from the buffer.
INFO is a plist holding contextual information."
  (let ((externals (if info (plist-get info :extern-components)
                     (save-excursion
                       (save-restriction
                         (let (s)
                           (widen)
                           (goto-char (point-min))
                           (while (not (eobp))
                             (let ((line (buffer-substring-no-properties
                                          (line-beginning-position) (line-end-position))))
                               (when (string-match "^#\\+EXTERN_COMPONENTS:\\(.*\\)" line)
                                 (setq s (concat s " " (match-string 1 line)))))
                             (forward-line))
                           (if s (split-string (string-trim s)))))))))
    (append ox-spectacle--components externals)))

(defun ox-spectacle--get-headlines (element &optional with-self)
  "Collect all ancestor headlines of ELEMENT.
When ELEMENT is headline and WITH-SELF is t, then add itself to the result."
  (let (hls (p (org-element-lineage element '(headline) with-self)))
    (while p
      (push p hls)
      (setq p (org-element-lineage p '(headline))))
    hls))

(defmacro ox-spectacle--pop-from-plist (plist &rest properties)
  "Pop the values with key of PROPERTIES in PLIST."
  (let ((ps (gensym)) (rs (gensym)) (pps (gensym)))
    `(let (,ps ,rs (,pps ',properties))
       (while ,plist
         (if (memq (car ,plist) ,pps)
             (setq ,rs (plist-put ,rs (car ,plist) (cadr ,plist)))
           (setq ,ps (plist-put ,ps (car ,plist) (cadr ,plist))))
         (setq ,plist (cddr ,plist)))
       (setq ,plist ,ps)
       ,rs)))

(defun ox-spectacle--wa (s &optional prefix suffix)
  "Try to trim S, and add nessesary PREFIX and SUFFIX."
  (if s
      (let ((a (org-trim s)))
        (if (org-string-nw-p a) (concat (or prefix " ") a suffix) ""))
    ""))

(defun ox-spectacle--maybe-appear (contents flags)
  "Wrap CONTENTS with <Appear> if FLAGS is A/NUM-props style."
  (if (and flags (string-match "^\\([A0-9]\\)[ \t]*\\(.*\\)$" flags))
      (let ((priority (when-let ((s (match-string 1 flags)))
                        (if (string-equal s "A") nil s)))
            (props (when-let ((s (match-string 2 flags)))
                     (ox-spectacle--props-compat-react-htm s))))
        (concat "<${Appear}"
                (ox-spectacle--wa props)
                (if priority (format " priority=${%s}" priority)) ">\n"
                contents
                "\n</${Appear}>"))
    contents))

(defun ox-spectacle--make-attribute-string (attributes &optional info)
  "Override ‘org-html--make-attribute-string’, make ATTRIBUTES a string.
INFO is a plist."
  (let (output)
    (dolist (item attributes)
      (cond ((null item) (pop output))
            ((symbolp item) (push (substring (symbol-name item) 1) output))
            (t (let ((key (car output))
                     (value (org-html-encode-plain-text item)))
                 (cond ((string-match-p "^{.*}$" value)
                        (setq value (concat "$" value)))
                       ((string-match "^['\"]\\(.*\\)['\"]$" value)
                        (setq value (match-string 1 value)))
                       (t (setq value (replace-regexp-in-string "\"" "&quot;" value t))))
                 (setcar output (format (if (string-match-p "^\\${.*}$" value) "%s=%s" "%s=\"%s\"")
                                        key value))))))
    (ox-spectacle--props-inline-image (mapconcat 'identity (nreverse output) " ") info)))

(defmacro ox-spectacle-advice (&rest body)
  "Add advices to BODY."
  `(cl-letf (((symbol-function 'string-trim) 'org-trim)
             ((symbol-function 'org-html--make-attribute-string) 'ox-spectacle--make-attribute-string))
     ,@body))


;;; Filters and Transcoders

(defun ox-spectacle--init-filter (exp-plist backend)
  "Do some initialization work.
EXP-PLIST is a plist containing export options. BACKEND is the export back-end
currently used."
  (setq ox-spectacle--images nil
        ox-spectacle--frags-css nil
        ox-spectacle--frags-script nil
        ox-spectacle--frags-header nil
        ox-spectacle--frags-html nil
        ox-spectacle--frags-template nil)
  (org-html-infojs-install-script exp-plist backend))

(defun ox-spectacle--parse-tree-filter (data _backend info)
  "Filter the buffer tree before export. DATA is a parse tree. INFO is a plist."
  (org-export-insert-image-links data info org-html-inline-image-rules))

(defun ox-spectacle--template (body info)
  "Return complete document string after HTML conversion.
BODY is the transcoded contents string. INFO is a plist
holding export options."
  (cl-flet ((mkattr (key)
              (let ((r (org-export-data (plist-get info (intern (format ":%s" key))) info)))
                (if (> (length r) 0) (ox-spectacle--wa (format "%s=${%s}" key r)) "")))
            (joinfrags (type prefix &optional suffix separator)
              (let* ((frags (symbol-value type))
                     (joined (mapconcat #'identity frags (or separator "\n"))))
                (ox-spectacle--wa joined prefix (or suffix "\n")))))
    (let* ((deck-opts (ox-spectacle--extract-options :deck-opts info t))
           (deck-tag (car deck-opts))
           (deck-props (cdr deck-opts))
           (model-script (format ox-spectacle--page-model-script
                                 (mapconcat #'identity ox-spectacle--components ", ")
                                 (mapconcat #'identity ox-spectacle--utils ", ")
                                 ox-spectacle-default-template
                                 (joinfrags 'ox-spectacle--frags-template "\n    /* user templates defined in org file */\n\n")
                                 (joinfrags 'ox-spectacle--frags-script "\n    /* user scripts defined in org file */\n\n")
                                 (or deck-tag "Deck")
                                 (ox-spectacle--wa deck-props)
                                 (mkattr 'theme)
                                 (mkattr 'template)
                                 (mkattr 'transition)
                                 (string-trim body)
                                 (or deck-tag "Deck")))
           (page-body (with-temp-buffer
                        (insert (joinfrags 'ox-spectacle--frags-html ""))
                        (goto-char (point-min))
                        (if (re-search-forward " id=['\"]root['\"]" nil t)
                            (progn
                              (end-of-line)
                              (insert "\n" model-script "\n"))
                          (insert "  <div id=\"root\"></div>\n" model-script "\n"))
                        (buffer-string))))
      (format ox-spectacle--page-html
              (org-export-data (plist-get info :title) info)
              (ox-spectacle--wa (org-html--build-mathjax-config info) "\n<!-- MathJax Setup -->\n\n" "\n")
              (ox-spectacle--wa (ox-spectacle--make-scripts-section info) "\n<!-- scripts -->\n\n" "\n")
              (ox-spectacle--wa (ox-spectacle--make-images-section) "\n<!-- images -->\n\n" "\n")
              (ox-spectacle--wa (concat ox-spectacle-initial-style
                                        (joinfrags 'ox-spectacle--frags-css
                                                   "\n/* extra css catch from the org file */\n\n" "\n"))
                                "\n<!-- styles -->\n\n<style>\n\n" "\n\n</style>\n")
              (joinfrags 'ox-spectacle--frags-header "\n<!-- extra head catch from the org file -->\n\n" "\n" "\n\n")
              page-body))))

(defun ox-spectacle--inner-template (contents _info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string."
  contents)

(defun ox-spectacle--headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline. INFO is a plist
holding contextual information."
  (let* ((title (org-export-data (org-element-property :title headline) info))
         (level (org-element-property :level headline))
         (headlines (ox-spectacle--get-headlines headline t))
         (rtitle (org-element-property :raw-value (car headlines))))
    ;; the special <config> section
    (if (string-match "^[ \t]*<config\\([ =-]?\\)>" rtitle)
        (if (= 1 (length (match-string 1 rtitle))) "" ; mute <config> when flag set
          (let ((tpl-regexp "^<t\\(?:emplate\\|\\)>[ \t]*\\([a-zA-Z][a-zA-Z0-9]+\\)"))
            (cond
             ;; take <template> section as a template definition
             ((and (= level 2) (string-match tpl-regexp title))
              (if-let ((name (match-string 1 title)))
                  (prog1 ""
                    (setq ox-spectacle--frags-template
                          (append ox-spectacle--frags-template
                                  (list (concat "\n    let " name " = ({ slideNumber, numberOfSlides }) => html`\n" contents "`;")))))
                (user-error "No name found on <Config/Template>")))
             ;; sections under <template>, return directly
             ((and (> level 2) (string-match-p tpl-regexp (org-element-property :raw-value (cadr headlines)))) contents)
             ;; others, ignore. catch src-blocks under it in `ox-spectacle--src-block'
             (t ""))))
      (let* ((layout (org-element-property :LAYOUT headline))
             (topp (lambda (&optional h)
                     (or (string-match-p "^top$" (or (if h (org-element-property :LAYOUT h) layout) ""))
                         (string-match-p "<top>$" (if h (org-export-data (org-element-property :title h) info) title)))))
             (current-top-p (funcall topp))
             (title (replace-regexp-in-string "[ \t]*<top>$" "" title)))
        ;; if type is Top, then headlines under it will be a slide page
        (if current-top-p contents
          (let* ((type (org-element-property :TYPE headline))
                 (props (ox-spectacle--wa (org-element-property :PROPS headline)))
                 (headnums (org-export-get-headline-number headline info))
                 (regexp (format "\\(?:%s\\)" (mapconcat #'identity (ox-spectacle--available-components info) "\\|")))
                 (slide-headline-p (or (= level 1) (funcall topp (org-element-lineage headline '(headline)))))
                 (tag type) prefix inline-tag inline-props inline-prefix inline-suffix slide-title)
            ;; headline with <Component.Xxx props> declaration has the highest priority
            (when (string-match (format "<\\${\\(%s\\(?:\\.[A-Z][a-zA-Z0-9]+\\)*\\)}\\( [^>]*\\|\\)>\\(\\(?:<.*>\\)?\\)$" regexp) title)
              (let ((tt (match-string 1 title)))
                ;; special case, <FlexBox/Box/Grid/Appear..> on slide headline, wrapper
                (if (and slide-headline-p (string-match-p "Box\\|Grid\\|Appear" tt))
                    (setq inline-prefix (match-string 0 title)
                          slide-title (cl-subseq title 0 (- 0 (length inline-prefix))))
                  (setq inline-tag (match-string 1 title)
                        inline-props (ox-spectacle--wa (match-string 2 title))
                        inline-prefix (match-string 3 title)))) ; deal multiple Components on headline
              (with-temp-buffer
                (insert inline-prefix)
                (goto-char (point-min))
                (while (re-search-forward "<${\\([A-Z][a-zA-Z0-9]+\\(?:\\.[A-Z][a-zA-Z0-9]+\\)*\\)}" nil t)
                  (setq inline-suffix (concat " </${" (match-string 1) "}>" inline-suffix)))))
            ;; top-most/under-top-layout, should be a Slide or SlideLayout
            (when slide-headline-p
              (let* ((slide-opts (ox-spectacle--extract-options :slide-opts info))
                     (slide-tag (car slide-opts))
                     (slide-props (ox-spectacle--wa (cdr slide-opts))))
                (cond (inline-tag (setq tag inline-tag
                                        props (concat inline-props props)))
                      (layout (setq tag (format "SlideLayout.%s" layout)))
                      (type (setq tag type)))
                (unless tag (setq tag (or slide-tag "Slide")))
                (if (= (length props) 0) (setq props slide-props))
                ;; add props title and num to user custom Slide
                (unless (string-match-p "^\\(Slide\\|SlideLayout\\..*\\)$" tag)
                  (let ((st (or slide-title title)))
                    (when (not (string-match-p " num=" props))
                      (setq props (concat (format " num=${[%s]}" (mapconcat #'number-to-string headnums ",")) props)))
                    (when (and (not (string-match-p " title=" props)) (> (length st) 0))
                      (setq props (concat (format " title=\"%s\"" (string-trim st)) props))))))
              (setq prefix (format "\n<!------ slide (%s) begin ------>\n\n" (mapconcat #'number-to-string headnums "_"))))
            ;; default Component used by headline
            (unless tag
              (if inline-tag
                  (setq tag inline-tag props (concat inline-props props))
                (setq tag "Box")))
            ;; normalize
            (if (string-match-p regexp tag) (setq tag (format "${%s}" tag)))
            (setq props (ox-spectacle--filter-props props info))
            ;; work with #+split: t, wrap every part with <Box>
            (let ((cs (if contents (split-string contents "#spectacle-splitter#" t))))
              (when (> (length cs) 1)
                (setq contents (mapconcat (lambda (c)
                                            (concat "\n<div className='split-container'>\n"
                                                    (string-trim c) "\n</div>\n")) cs))))
            ;; final output
            (concat prefix "<" tag props ">\n" inline-prefix contents inline-suffix "</" tag ">")))))))

(defun ox-spectacle--section (_section contents _info)
  "Transcode a _SECTION element from Org to HTML.
CONTENTS holds the contents of the section."
  contents)

(defun ox-spectacle--src-block (element _content info)
  "Transcode a src-block ELEMENT from Org to HTML.
INFO is a plist holding contextual information."
  (let* ((lang (org-element-property :language element))
         (code (org-element-property :value element))
         (linum (org-element-property :number-lines element))
         (props (org-export-read-attribute :attr_html element))
         (code-props (ox-spectacle--pop-from-plist props :showLineNumbers :highlightRanges :stepIndex :theme :style))
         (flags (cadr (ox-spectacle--pop-from-plist props :type)))
         (props (ox-spectacle--wa (ox-spectacle--make-attribute-string props info)))
         (code-props (ox-spectacle--wa (ox-spectacle--make-attribute-string code-props info)))
         (tag "CodePane"))
    ;; catch fragments under <config> section and others with ':type config' above
    ;; with <config-> or :type config-, then will muted it
    (if-let* ((rtitle (org-element-property :raw-value (car (ox-spectacle--get-headlines element))))
              (muteflg (or (and flags (string-match "^config\\([ =-]?\\)$" flags) (match-string 1 flags))
                           (and rtitle (string-match "^[ \t]*<config\\([ =-]?\\)>" rtitle) (match-string 1 rtitle)))))
        (prog1 ""
          (pcase (and (= (length muteflg) 0) lang)
            ("css" (setq ox-spectacle--frags-css (append ox-spectacle--frags-css (list code))))
            ((or "js" "javascript") (setq ox-spectacle--frags-script (append ox-spectacle--frags-script (list code))))
            ("html" (setq ox-spectacle--frags-header (append ox-spectacle--frags-header (list code))))
            ("mhtml" (setq ox-spectacle--frags-html (append ox-spectacle--frags-html (list code))))))
      ;; others, make it a CodePane or CustomCodePane
      (when (and flags (string-match "^\\([A-Z][[:alnum:]]+\\)\\(.*\\)" flags))
        (setq tag (match-string 1 flags)
              code-props (ox-spectacle--filter-props (concat code-props " " (match-string 2 flags)) info)
              flags nil))
      (let ((contents
             (format "%s\n<${%s}%s%s%s>\n${`\n%s\n`}\n</${%s}>%s"
                     (if (string-empty-p props) "" (format "\n<${Box}%s>" props))
                     tag
                     (if lang (concat " language='" lang "'") "")
                     (if linum "" (concat " showLineNumbers=${false}"))
                     code-props code
                     tag
                     (if (string-empty-p props) "" "\n</${Box}>"))))
        (ox-spectacle--maybe-appear contents flags)))))

(defun ox-spectacle--example-block (example-block _contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML.
INFO is a plist holding contextual information."
  (let ((attributes (org-export-read-attribute :attr_html example-block)))
    (if (plist-get attributes :textarea)
        (org-html--textarea-block example-block)
      (concat "<div className='example'>\n"
              (ox-spectacle--src-block
               example-block
               (org-html-format-code example-block info)
               info)
              "\n</div>"))))

(defun ox-spectacle--verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
CONTENTS is verse block contents. INFO is a plist holding
contextual information."
  (let* ((props (org-export-read-attribute :attr_html verse-block))
         (flags (cadr (ox-spectacle--pop-from-plist props :type)))
         (regexp-comp (format "\\(\\(%s\\)\\(\\.[A-Z][a-zA-Z0-9]+\\)*\\)" (mapconcat #'identity (ox-spectacle--available-components info) "\\|")))
         tag tag-props)
    ;; Wrap the contens with specific Component or Text. Make it Appear if nessesary
    (if (and flags (not (string-match-p "^\\([A0-9]\\)[ \t]*\\(.*\\)$" flags)))
        (setq tag (car (split-string flags nil t))
              tag-props (if (string= tag flags) nil (ox-spectacle--filter-props (cl-subseq flags (length tag)))))
      (if-let ((text-opts (ox-spectacle--extract-options :text-opts info)))
          (setq tag (car text-opts)
                tag-props (ox-spectacle--wa (cdr text-opts)))
        (setq tag "Text" tag-props nil)))
    (if (string-match-p regexp-comp (or tag "")) (setq tag (format "${%s}" tag)))
    (setq contents (format "<div className='verse'%s><%s%s>%s</%s></div>"
                           (ox-spectacle--wa (ox-spectacle--make-attribute-string props info))
                           tag (ox-spectacle--wa (ox-spectacle--filter-props tag-props info))
                           ;; Replace leading white spaces with non-breaking spaces.
                           (replace-regexp-in-string
                            "^[ \t]+" (lambda (m) (org-html--make-string (length m) "${'\u00A0'}"))
                            ;; Replace each newline character with line break. Also
                            ;; remove any trailing "br" close-tag so as to avoid duplicates.
                            (let* ((br (org-html-close-tag "br" nil info))
                                   (re (format "\\(?:%s\\)?[ \t]*\n" (regexp-quote br))))
                              (replace-regexp-in-string
                               re (concat br "\n")
                               ;; make $ < and ` work normally. see react-htm for details
                               (with-temp-buffer
                                 (insert contents)
                                 (goto-char (point-min))
                                 (while (not (eobp))
                                   (unless (looking-at "^[ \t]*<[a-zA-Z\\$/].*>")
                                     (replace-regexp-in-region "\\(`\\|\\$\\)" "\\\\\\1" (line-beginning-position) (line-end-position))
                                     (replace-regexp-in-region "<" "${'<'}" (line-beginning-position) (line-end-position)))
                                   (forward-line 1))
                                 (buffer-string)))))
                           tag))
    (ox-spectacle--maybe-appear contents flags)))

(defun ox-spectacle--special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block. INFO is a plist
holding contextual information."
  (let* ((type (org-element-property :type special-block))
         (params (org-element-property :parameters special-block))
         (props (org-export-read-attribute :attr_html special-block))
         (flags (cadr (ox-spectacle--pop-from-plist props :type)))
         (tag (if-let ((c (cl-find-if
                           (lambda (c) (string-match-p (concat "^" c "$") type))
                           (ox-spectacle--available-components))))
                  (concat "${" c "}")
                type))
         (contents (format "<%s%s>\n%s</%s>" tag
                           (concat (if params (ox-spectacle--filter-props params))
                                   (ox-spectacle--wa (ox-spectacle--make-attribute-string props info)))
                           contents tag)))
    (ox-spectacle--maybe-appear contents flags)))

(defun ox-spectacle--quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block. INFO is a plist
holding contextual information."
  (let* ((props (org-export-read-attribute :attr_html quote-block))
         (flags (cadr (ox-spectacle--pop-from-plist props :type)))
         (contents (format "<${Quote}%s>%s</${Quote}>"
                           (ox-spectacle--wa (ox-spectacle--make-attribute-string props info))
                           contents)))
    (ox-spectacle--maybe-appear contents flags)))

(defun ox-spectacle--center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block. INFO is a plist
holding contextual information."
  (let* ((props (org-export-read-attribute :attr_html center-block))
         (flags (cadr (ox-spectacle--pop-from-plist props :type)))
         (contents (concat
                    "<${FlexBox} margin='0 auto' flexDirection='column' justifyContent='flex-start' alignItems='center'"
                    (ox-spectacle--wa (ox-spectacle--make-attribute-string props info))
                    ">\n" contents "\n</${FlexBox}>")))
    (ox-spectacle--maybe-appear contents flags)))

(defun ox-spectacle--fixed-width (fixed-width _contents _info)
  "Transcode a :FIXED-WIDTH element from Org to HTML."
  (format "<div className=\"example fixed-width\"><${CodePane} showLineNumbers=${false}>\n%s</${CodePane}></div>"
          (org-html-do-format-code
           (org-remove-indentation
            (org-element-property :value fixed-width)))))

(defun ox-spectacle--code (code _contents _info)
  "Transcode ~CODE~ from Org to HTML."
  (format "<${CodeSpan}>${`%s`}</${CodeSpan}>"
          (org-element-property :value code)))

(defun ox-spectacle--verbatim (verbatim contents info)
  "Transcode =VERBATIM= from Org to HTML.
CONTENTS is the contents, INFO is a plist holding export options."
  (let ((v (org-element-property :value verbatim)))
    ;; make ` and $ literal, display normal
    (setq v (replace-regexp-in-string "\\(`\\|\\$\\)" "\\\\\\1" v))
    (org-element-put-property verbatim :value v))
  (ox-spectacle--code verbatim contents info))

(defun ox-spectacle--plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML.
CONTENTS is the contents of the list, INFO is a plist
holding export options."
  (let* ((ordered (eq (org-element-property :type plain-list) 'ordered))
         (props (org-export-read-attribute :attr_html plain-list))
         (tag (if ordered "OrderedList" "UnorderedList")))
    (format "<${%s}%s>\n%s</${%s}>\n" tag
            (ox-spectacle--wa (ox-spectacle--make-attribute-string props info))
            contents tag)))

(defun ox-spectacle--item (_item contents info)
  "Transcode an _ITEM element from Org to HTML.
CONTENTS holds the contents of the item. INFO is a plist
holding export options."
  (let (len flags props (contents (or contents "")))
    ;; <A/NUM>: make it Appear; pass proper props to ListItem or Appear
    (when (string-match "^<\\([^>$][^>]*\\)>" (car (split-string contents "\n")))
      (setq props (string-trim (match-string 1 contents)))
      (setq len (+ (length props) 2))
      (when (string-match "^\\([A-Z0-9]\\)\\([ \t]\\|$\\)+" props)
        (setq flags (match-string 1 props))
        (setq props (string-trim (cl-subseq props 1)))
        (setq props
              (with-temp-buffer
                (insert props)
                (goto-char (point-min))
                ;; parse props, some pass to Appear, others to ListItem
                (while (re-search-forward
                        (format "[ \t]+\\(%s\\)\\(=\\|[ \t]\\)"
                                (mapconcat #'identity
                                           '("priority" "alwaysVisible" "activeStyle" "inactiveStyle")
                                           "\\|"))
                        nil t)
                  (when (string-equal (match-string 2) "=")
                    (save-match-data
                      (if (re-search-forward "[ \t][a-z][a-zA-Z0-9]+\\(?:=\\|[ \t]\\)" nil t)
                          (goto-char (match-beginning 0))
                        (goto-char (point-max)))))
                  (setq flags (concat flags
                                      (ox-spectacle--wa
                                       (buffer-substring-no-properties
                                        (match-beginning 0) (point)))))
                  (delete-region (match-beginning 0) (point)))
                (ox-spectacle--wa (buffer-string)))))
      (setq contents (cl-subseq contents len)))
    (setq contents (concat "<${ListItem}"
                           (if props (ox-spectacle--filter-props props info)) ">"
                           (string-trim contents)
                           "</${ListItem}>"))
    (ox-spectacle--maybe-appear contents flags)))

(defun ox-spectacle--table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is the contents of the table, INFO is a plist holding export options."
  (let* ((props (org-export-read-attribute :attr_html table))
         (flags (cadr (ox-spectacle--pop-from-plist props :type)))
         (contents (format "<${Table}%s>\n%s</${Table}>"
                           (ox-spectacle--wa (ox-spectacle--make-attribute-string props info))
                           contents)))
    (ox-spectacle--maybe-appear contents flags)))

(defun ox-spectacle--table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to HTML.
CONTENTS is the contents of the row. INFO is a plist holding export options."
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((group (org-export-table-row-group table-row info))
           (start-group-p (org-export-table-row-starts-rowgroup-p table-row info))
           (end-group-p (org-export-table-row-ends-rowgroup-p table-row info))
           (row-open-tag "<${TableRow}>")
           (row-close-tag "</${TableRow}>")
           (group-tags (cond ((not (= 1 group))
                              '("\n<${TableBody}>" . "\n</${TableBody}>"))
                             ((org-export-table-has-header-p (org-export-get-parent-table table-row) info)
                              '("<${TableHeader}>" . "\n</${TableHeader}>"))
                             (t
                              '("\n<${TableBody}>" . "\n</${TableBody}>")))))
      (concat (and start-group-p (car group-tags))
              (concat "\n" row-open-tag "\n" (string-trim contents) "\n" row-close-tag)
              (and end-group-p (cdr group-tags))))))

(defun ox-spectacle--table-cell (_table-cell contents info)
  "Transcode a TABLE-CELL element from Org to HTML.
CONTENTS is the contents of cell. INFO is a plist holding export options."
  (setq contents (org-html-plain-text (or contents "") info))
  (format "\n<${TableCell}>%s</${TableCell}>" contents))

(defun ox-spectacle--format-image (path props info)
  "Parse image link.
PATH maybe a remote url or local file. PROPS and INFO are list."
  (let* ((lv (ox-spectacle--export-level info))
         (src (if (>= lv 2) (ox-spectacle--data-uri path) path))
         (type (cadr (ox-spectacle--pop-from-plist props :type)))
         (width (if (string-suffix-p "svg" src) "" " width=\"100%\""))
         (contents (org-html-close-tag
                    "${Image}"
                    (concat
                     (format "src=\"%s\" alt=\"%s\"%s" src (file-name-nondirectory path) width)
                     (ox-spectacle--wa (ox-spectacle--make-attribute-string props info)))
                    info)))
    (ox-spectacle--maybe-appear contents type)))

(defun ox-spectacle--link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information."
  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         (raw-link (org-element-property :raw-link link))
         (desc (org-string-nw-p desc))
         (path (cond ((string= type "file")
                      (org-export-file-uri raw-path))
                     ((member type '("http" "https" "ftp" "mailto" "news"))
                      (url-encode-url (org-link-unescape (concat type ":" raw-path))))
                     (t raw-path)))
         (props (let* ((parent (org-export-get-parent-element link))
                       (link (let ((container (org-export-get-parent link)))
                               (if (and (eq (org-element-type container) 'link)
                                        (org-html-inline-image-p link info))
                                   container
                                 link))))
                  (and (eq (org-element-map parent 'link 'identity info t) link)
                       (org-export-read-attribute :attr_html parent))))
         (sprops (ox-spectacle--wa (ox-spectacle--make-attribute-string props info))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'html))
     ;; Image file.
     ((and (plist-get info :html-inline-images)
           (org-export-inline-image-p link (plist-get info :html-inline-image-rules)))
      (ox-spectacle--format-image path props info))
     ;; Links pointing to a headline
     ((member type '("custom-id" "id"))
      (let* ((loc (org-export-resolve-id-link link info))
             (loctype (org-element-type loc))
             (destination loc)
             (id (org-element-property :CUSTOM_ID loc)))
        (when-let ((hd (and loctype
                            (not (member loctype '(headline plain-text)))
                            (car (last (org-element-lineage loc 'headline))))))
          (setq destination hd))
        (if (equal (org-element-type destination) 'headline)
            (let* ((headlines (cl-remove-if
                               ;; make sure ignore <config> section
                               (lambda (hl) (string-match-p "^[ \t]*<config.?>" (org-element-property :raw-value hl)))
                               (org-export-collect-headlines info 1)))
                   (idx (cl-position destination headlines :test #'equal))
                   (desc (if (and (org-export-numbered-headline-p loc info) (not desc))
                             (mapconcat #'number-to-string (org-export-get-headline-number loc info) ".")
                           (or desc (org-export-data (org-element-property :title loc) info)))))
              (format "<${MyLink} id=\"%s\">%s</${MyLink}>" idx desc))
          (format "<${link} href=\"#%s\"%s>%s</${Link}>" (or id path) sprops (or desc raw-link)))))
     ;; External link.
     (t (format "<${Link} href=\"%s\"%s>%s</${Link}>"
                (if path (org-html-encode-plain-text path) "#") sprops
                (or desc (org-link-unescape path)))))))

(defun ox-spectacle--paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string. INFO is
the plist used as a communication channel."
  (let* ((case-fold-search nil)
         (headline (org-element-lineage paragraph '(headline)))
         (props (org-export-read-attribute :attr_html paragraph))
         (type (cadr (ox-spectacle--pop-from-plist props :type)))
         (props (ox-spectacle--wa (ox-spectacle--make-attribute-string props info)))
         (regexp-html (format "\\(%s\\)" (mapconcat #'identity ox-spectacle--html-tags "\\|")))
         (regexp-comp (format "\\(\\(%s\\)\\(\\.[A-Z][a-zA-Z0-9]+\\)*\\)" (mapconcat #'identity (ox-spectacle--available-components info) "\\|")))
         (parent (org-export-get-parent paragraph))
         (parentype (org-element-type parent))
         (hd-raw (or (org-element-property :raw-value headline) ""))
         (notep (string-match-p "<notes.*>" hd-raw))
         (stepperp (string-match-p "<Stepper.*>" hd-raw))
         tag)
    (cond
     ((or stepperp
          (cl-member parentype '(item quote-block))
          (cl-member type '("no" "raw") :test 'string=))
      contents)
     ((org-html-standalone-image-p paragraph info)
      (let ((caption (let ((c (org-export-data (org-export-get-caption paragraph) info)))
                       (if (not (org-string-nw-p c)) "" (format "  <figcaption>%s</figcaption>\n" c))))
            (class-name nil))
        (with-temp-buffer
          (insert contents)
          (goto-char (point-min))
          (when (re-search-forward "{Image}.*\\( className=.[^'\"]+.\\)" nil t)
            (setq class-name (match-string 1))
            (delete-region (match-beginning 1) (match-end 1)))
          (goto-char (point-min))
          (re-search-forward "<\\${Image}" nil t)
          (beginning-of-line)
          (insert "<figure" (or class-name "") ">\n  ")
          (or (re-search-forward " />" nil t) (goto-char (point-max)))
          (insert "\n" caption "</figure>")
          (buffer-string))))
     ((and (member parentype '(center-block)) ;; ignore empty lines begin/end of the block
           (or (eq (org-element-property :begin paragraph)
                   (org-element-property :contents-begin parent))
               (eq (org-element-property :end paragraph)
                   (org-element-property :contents-end parent)))
           (string-match-p "^\n+$" contents))
      "")
     (t
      ;; if <Element> style, insert props into proper place directly, same as plain html paragraph
      (let ((firstline (or (car (split-string contents "\n" t)) ""))
            (reg (format "^[ \t]*</?\\(\\${%s}\\|%s\\)\\([ \t]\\|/?>\\)" regexp-comp regexp-html)))
        (when (string-match-p reg firstline)
          (setq contents (with-temp-buffer
                           (insert contents)
                           (goto-char (point-min))
                           (search-forward ">")
                           (backward-char)
                           (insert (ox-spectacle--wa props))
                           (concat (ox-spectacle--props-inline-image (buffer-substring (point-min) (point)) info)
                                   (buffer-substring (point) (point-max))))
                tag t)))
      ;; fallback tag and props to :text-opts or Text
      (let* ((text-opts (ox-spectacle--extract-options :text-opts info t))
             (text-tag (car text-opts))
             (text-props (ox-spectacle--wa (cdr text-opts))))
        (setq tag (if (eq tag t) nil (or type (if notep "p" (or text-tag "Text")))))
        (if (= (length props) 0) (setq props text-props)))
      ;; add ${} for component tag
      (if (string-match-p regexp-comp (or tag "")) (setq tag (format "${%s}" tag)))
      (if tag (format "<%s%s>%s</%s>" tag props (string-trim contents) tag)
        (ox-spectacle--wa contents "\n"))))))

(defun ox-spectacle--plain-text (text info)
  "Transcode a TEXT string from Org to HTML.
TEXT is the string to transcode. INFO is a plist holding contextual information."
  (let ((case-fold-search nil))
    ;; wrap Component with ${}, and add nessessary $ to props
    (setq text (replace-regexp-in-string
                (concat (format "<\\(/?\\(?:\\(?:%s\\)\\(?:\\.[A-Z][a-zA-Z0-9]+\\)?\\)\\)"
                                (mapconcat #'identity (ox-spectacle--available-components info) "\\|"))
                        "\\(\\(?: \\|$\\)[^>]*\\|\\)>")
                (lambda (old)
                  (save-match-data
                    (let ((tag (match-string 1 old))
                          (props (or (match-string 2 old) "")))
                      (if (string-prefix-p "/" tag)
                          (format "</${%s}>" (cl-subseq tag 1))
                        (format "<${%s}%s>" tag (ox-spectacle--props-compat-react-htm props))))))
                text t t))
    ;; add $ for props of plain html tag, make it compat with react syntax
    (when (string-match-p (format "<\\(%s\\)[ \t]" (mapconcat #'identity ox-spectacle--html-tags "\\|")) text)
      (setq text (ox-spectacle--props-compat-react-htm text)))
    (let ((org-html-protect-char-alist nil))
      (setq text (org-html-plain-text text info)))))

(defun ox-spectacle--latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT object from Org to HTML.
INFO is a plist holding contextual information."
  (let* ((latex-frag (org-element-property :value latex-fragment))
         (result (org-html-format-latex latex-frag 'mathjax info)))
    (string-replace "\\" "\\\\" result)))

(defun ox-spectacle--keyword (keyword _contents info)
  "Transcode a KEYWORD element from Org to HTML.
CONTENTS is nil. INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((string= key "HTML") value)
     ((string= key "SPLIT")
      (pcase value
        ("t" "#spectacle-splitter#")
        (_ (let ((parsed (progn
                           (string-match "^\\([0-9\\.]+\\)?\\(.*\\)" value)
                           (cons (match-string 1 value) (match-string 2 value)))))
             (format "<${Box} height=\"%s\"%s></${Box}>"
                     (if (car parsed) (format "%sem" (car parsed)) "5px")
                     (ox-spectacle--filter-props (cdr parsed) info)))))))))


;;; Mode

(defvar ox-spectacle-minor-mode-map nil)

(defun ox-spectacle-completion-at-point ()
  "Complete component name with capf as possible."
  (let* ((bds (bounds-of-thing-at-point 'symbol))
         (start (let ((p (car bds)))
                  (cond ((equal (char-after p) ?<) (+ p 1))
                        ((equal (char-before p) ?/) p))))
         (end (cdr bds))
         (cands (append '("template") (ox-spectacle--available-components))))
    (when (and start (> end start))
      (list start end (completion-table-case-fold cands) ::exclusive 'no))))

(define-minor-mode ox-spectacle-minor-mode
  "Add some features to current org buffer for better Spectacle Slides writing."
  :group 'ox-spectacle
  :lighter " Spectable"
  :keymap ox-spectacle-minor-mode-map
  (if (derived-mode-p 'org-mode)
      (let* ((kws  `((,(format "</?\\(%s\\)\\(?:>\\|[ \t\n]\\)"
                               (mapconcat #'identity
                                          (append '("template") (ox-spectacle--available-components))
                                          "\\|"))
                      (1 font-lock-function-name-face t))
                     ("\\(?:^\\|[ \t]\\)\\([_[:alpha:]][-_.[:alnum:]]*\\)=[\"'{]"
                      1 font-lock-variable-name-face t)))
             (act   (lambda () ; use closure to avoid changement
                      (font-lock-add-keywords nil kws t)
                      (font-lock-flush)
                      (add-hook 'completion-at-point-functions 'ox-spectacle-completion-at-point nil 'local)))
             (inact (lambda ()
                      (font-lock-remove-keywords nil kws)
                      (font-lock-flush)
                      (remove-hook 'completion-at-point-functions 'ox-spectacle-completion-at-point 'local))))
        (funcall (if ox-spectacle-minor-mode act inact)))
    (setq ox-spectacle-minor-mode nil)
    (user-error "Please run this under org-mode")))


;;; End-user functions

(defun ox-spectacle-export-to-buffer (&optional async subtreep visible-only body-only ext-plist)
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
  (ox-spectacle-advice
   (org-export-to-buffer 'spectacle "*Org Spectacle Export*"
     async subtreep visible-only body-only ext-plist (lambda () (set-auto-mode t)))))

(defun ox-spectacle-export-to-file (&optional async subtreep visible-only body-only ext-plist)
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
    (ox-spectacle-advice (org-export-to-file 'spectacle file async subtreep visible-only body-only ext-plist))))

(defun ox-spectacle-export-to-file-and-browser (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to file and then browser the HTML file.
Optional ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST are passed
to `ox-spectacle-export-to-file'."
  (interactive)
  (browse-url-of-file
   (expand-file-name
    (ox-spectacle-export-to-file async subtreep visible-only body-only ext-plist))))

(defun ox-spectacle-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (ox-spectacle-advice
   (org-publish-org-to 'spectacle filename
                       (concat "." (or (plist-get plist :html-extension) org-html-extension "html"))
                       plist pub-dir)))


(provide 'ox-spectacle)

;;; ox-spectacle.el ends here
