(ql:quickload '(:clip :st-json :local-time :lparallel) :silent t)
(local-time:enable-read-macros)

(defpackage :generate
  (:use :cl :local-time))
(in-package :generate)

(defparameter +base-url+ "https://www.joram.io")

;;; Utilities -----------------------------------------------------------------

(defun pandoc (&rest parameters)
  (let* ((out (make-string-output-stream))
         (err (make-string-output-stream))
         (process (sb-ext:run-program
                   "pandoc"
                   (cons "--syntax-definition=conf-syntax.xml"
                         parameters)
                   :search t
                   :output out
                   :error err)))
    (if (= 0 (sb-ext:process-exit-code process))
        (get-output-stream-string out)
        (let ((error (get-output-stream-string err)))
          (error "Pandoc failed: ~A" error)))))

(defun pandoc-markup (input-file)
  (pandoc "--to=html"
          "--email-obfuscation=none"
          input-file))

(defun pandoc-metadata (input-file)
  (st-json:read-json (pandoc "--template=templates/metadata.pandoc-template"
                             input-file)))

(defun format-date (timestamp)
  (format-timestring nil timestamp
                     :format +iso-8601-date-format+))

(defun format-timestamp (timestamp)
  (format-rfc3339-timestring nil timestamp))

(defun full-url (url)
  (format nil "~A~A" +base-url+ url))

(defun to-cdata (html)
  (plump:make-cdata (plump:make-root) :text html))

(defstruct post
  title
  description
  published
  updated
  url
  html)

;;; Templates -----------------------------------------------------------------

(defun default-template (title description body)
  (clip:process #p"templates/default.ctml"
                :title title
                :description description
                :body body))

(defun page-template (title browser-title description body)
  (default-template browser-title
                    description
                    (clip:process #p"templates/page.ctml"
                                  :title title
                                  :body body)))

(defun post-template (post)
  (default-template (post-title post)
                    (post-description post)
                    (clip:process #p"templates/post.ctml"
                                  :post post)))

;;; ATOM feed -----------------------------------------------------------------

(defun atom-feed (url posts)
  (let ((plump:*tag-dispatchers* plump:*xml-tags*))
    (clip:process #p"templates/atom.ctml"
                  :url url
                  :newest-timestamp (apply #'timestamp-maximum
                                           (map 'list #'post-published posts))
                  :posts posts)))

;;; Output --------------------------------------------------------------------

(defun content (output-file content)
  (let ((path (merge-pathnames output-file "_site/")))
    (ensure-directories-exist path)
    (if (typep content 'string)
        (with-open-file (file-output
                         path
                         :direction :output
                         :if-exists :supersede)
          (princ content file-output))
        (with-open-file (file-output
                         path
                         :element-type '(unsigned-byte 8)
                         :direction :output
                         :if-exists :supersede)
          (write-sequence content file-output)))))

(defun plump-content (output-file node)
  (content output-file (plump:serialize node nil)))

(defun plump-xml-content (output-file node)
  (content output-file
           (let ((plump:*tag-dispatchers* plump:*xml-tags*))
             (plump:serialize node nil))))

(defun file-content (output-file pathname)
  (with-open-file (in pathname
                      :element-type '(unsigned-byte 8)
                      :direction :input)
    (let ((data (make-array (file-length in)
                            :element-type '(unsigned-byte 8))))
      (read-sequence data in)
      (content output-file data))))

(defun page (out in)
  (let* ((metadata (pandoc-metadata in))
         (title (st-json:getjso "title" metadata))
         (browser-title (or (st-json:getjso "browserTitle" metadata)
                            title))
         (description (st-json:getjso "description" metadata)))
    (check-type title string)
    (check-type browser-title string)
    (check-type description string)
    (plump-content out (page-template title browser-title
                                      description
                                      (pandoc-markup in)))))

(defun relative-path (absolute-pathname base)
  (let ((pathname-directory (pathname-directory absolute-pathname))
        (current-directory (pathname-directory (merge-pathnames base))))
    (let* ((absolute-part
             (subseq pathname-directory
                     0
                     (length current-directory))))
      (assert (equal absolute-part current-directory)))
    (let ((relative-part
            (subseq pathname-directory (length current-directory))))
      (format nil "~@[~{~A~^/~}/~]~@[~A~@[.~A~]~]"
              relative-part
              (pathname-name absolute-pathname)
              (pathname-type absolute-pathname)))))

(defun directory-content (output-directory input-directory &key (exclude ()))
  (loop for subpath in (directory (format nil "~a**/*.*" input-directory))
        when (and (pathname-name subpath)
                  (not (find subpath exclude :test #'equal)))
          do (file-content (format nil "~a~a"
                                   output-directory
                                   (relative-path subpath input-directory))
                           subpath)))

(defvar *posts* ())

(defun post (path)
  (let* ((index.html (format nil "~aindex.html" path))
         (post.md (format nil "~apost.md" path))
         (html (pandoc-markup post.md))
         (metadata (pandoc-metadata post.md))
         (title (st-json:getjso "title" metadata))
         (description (st-json:getjso "description" metadata))
         (published-string (st-json:getjso "published" metadata))
         (published (when published-string (parse-timestring published-string)))
         (updated-string (st-json:getjso "updated" metadata))
         (updated (when updated-string (parse-timestring updated-string)))
         (post (make-post
                :title title
                :description description
                :published published
                :updated updated
                :url (format nil "/~a" path)
                :html html)))
    (when published
      (plump-content index.html (post-template post))
      (directory-content path path :exclude (list (merge-pathnames post.md)))
      post)))

;;; Content -------------------------------------------------------------------

;; We need the *package* binding for Clip
(let ((package `((*package* . ,(find-package :generate)))))
  (setf lparallel:*kernel* (lparallel:make-kernel 8 :bindings package)))

(let* ((paths (loop for post in (directory "blog/*")
                    collect (relative-path post "")))
       (posts (lparallel:pmapcar 'post paths)))
  (setf *posts* (sort posts #'timestamp> :key #'post-published)))

(plump-content
 "blog/index.html"
 (page-template "Blog" "Blog"
                "Sporadic blog posts about things that might be interesting."
                (clip:process #p"templates/post-list.ctml"
                              :posts *posts*)))

(page "index.html" "index.md")

(plump-xml-content "atom.xml" (atom-feed "/atom.xml" *posts*))

(file-content "style.css" "style.css")
(file-content "favicon.ico" "favicon.ico")
