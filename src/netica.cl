;;; Lisp API for Netica
;;; Use CLisp's Netica module as a reference API and implementation

(defpackage :netica
  (:use :cl :excl)
  (:import-from :netica.ffi *null*)
  (:export #:get-node-beliefs
           #:get-node-expected-utils
           #:get-node-likelihood
           #:get-node-probs
           #:get-node-levels
           #:*verbose*
           #:*env*
           #:*license*
           #:netica-condition
           #:netica-error
           #:check-errors
           #:clear-errors
           #:start-netica
           #:close-netica
           #:make-net
           #:net-info
           #:make-node
           #:node-info
           #:get-beliefs
           #:enter-finding
           #:open-dne-file
           #:with-open-dne-file
           #:save-net
           #:read-net
           ))

(in-package :netica)

;;; utils
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-unique-names (names &body forms) 
    `(let ,(mapcar (lambda (name)
                     (multiple-value-bind (symbol string)
                         (etypecase name
                           (symbol
                            (values name (symbol-name name)))
                           ((cons symbol (cons (or symbol string character) null))
                            (values (first name) (string (second name)))))
                       `(,symbol (gensym ,string))))
            names)
       ,@forms)))

;;; low-level wrapper
;;; just for reference (ported from clisp)
(defmacro make-node-wrapper (func type &rest args)
  (with-unique-names (node vec len res)
    (let* ((fun (if (consp func) (first func) func))
           (func-name (symbol-name fun)) 
           (length-form (if (consp func)
                            (subst node '<node> (second func))
                            `(netica.ffi:get-node-number-states-bn ,node)))
           (name (intern (subseq func-name 0 (position #\- func-name :from-end t))))
           (size (ff:sizeof-fobject 'netica.ffi:ff-prob-bn)))
      `(defun ,name (,node ,@args)
         ,(concatenate 'string "A low-level wrapper for netica.ffi:" (string-downcase func-name))
         (let* ((,len ,length-form)
                (,vec (,fun ,node ,@args)) 
                (,res (make-array ,len :element-type ',(first type))))
           ;; it's a hack
           (when (nullp ,vec)
             (return-from ,name (make-array 0 :element-type ',(first type))))
           (loop for i below ,len
                 for offset from 0 by ,size
                 do (setf (aref ,res i)
                          (ff:fslot-value-typed
                           ',(second type)
                           nil
                           (+ (ff:foreign-pointer-address ,vec) offset))))
           ,res)))))

(defun adjust-number-of-states (num-states type)
  (case type
    (#.netica.ffi:*continuous-type* (1+ num-states))
    (#.netica.ffi:*discrete-type* num-states)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (make-node-wrapper netica.ffi:get-node-beliefs-bn (single-float netica.ffi:ff-prob-bn))
  (make-node-wrapper netica.ffi:get-node-expected-utils-bn (single-float netica.ffi:ff-util-bn))
  (make-node-wrapper netica.ffi:get-node-likelihood-bn (single-float netica.ffi:ff-prob-bn))
  (make-node-wrapper netica.ffi:get-node-probs-bn (single-float netica.ffi:ff-prob-bn) parent-states)
  (make-node-wrapper (netica.ffi:get-node-levels-bn
                      (adjust-number-of-states
                       (netica.ffi:get-node-number-states-bn <node>)
                       (netica.ffi:get-node-type-bn <node>)))
                     (double-float netica.ffi:ff-level-bn)))

;;; user interface variables
;;; TODO: *license* and *verbose*
(defvar *verbose* t "log stream for netica")
(defvar *env* nil "default/current netica environment")
(defvar *license* "" "license key for netica")

;;; helpers
(defun truep (bool)
  (if (eq bool netica.ffi:*false*)
      nil
      t))

(defun nullp (p)
  (etypecase p
    (integer (zerop p))
    (ff:foreign-pointer (zerop (ff:foreign-pointer-address p)))
    (ff:foreign-address (zerop (ff:fslot-address p)))))

(defparameter *error-category-map*
  `(("OUT_OF_MEMORY_CND" . ,netica.ffi:*out-of-memory-cnd*)
    ("USER_ABORTED_CND" . ,netica.ffi:*user-aborted-cnd*)
    ("FROM_WRAPPER_CND" . ,netica.ffi:*from-wrapper-cnd*)
    ("FROM_DEVELOPER_CND" . ,netica.ffi:*from-developer-cnd*)
    ("INCONS_FINDING_CND" . ,netica.ffi:*incons-finding-cnd*)))

(defparameter *error-severity-map*
  `(("XXX_ERR" . ,netica.ffi:*xxx-err*)
    ("ERROR_ERR" . ,netica.ffi:*error-err*)
    ("WARNING_ERR" . ,netica.ffi:*warning-err*)
    ("NOTICE_ERR" . ,netica.ffi:*notice-err*)
    ("REPORT_ERR" . ,netica.ffi:*report-err*)
    ("NOTHING_ERR" . ,netica.ffi:*nothing-err*)))

(defparameter *argument-checking-map*
  `(("NO_CHECK" . ,netica.ffi:*no-check*)
    ("QUICK_CHECK" . ,netica.ffi:*quick-check*)
    ("REGULAR_CHECK" . ,netica.ffi:*regular-check*)
    ("COMPLETE_CHECK" . ,netica.ffi:*complete-check*)
    ("QUERY_CHECK" . ,netica.ffi:*query-check*)))

(defparameter *node-type-map*
  `(("DISCRETE_TYPE" . ,netica.ffi:*discrete-type*)
    ("CONTINUOUS_TYPE" . ,netica.ffi:*continuous-type*)))

(defparameter *node-kind-map*
  `(("NATURE_NODE" . ,netica.ffi:*nature-node*)
    ("DECISION_NODE" . ,netica.ffi:*decision-node*)
    ("UTILITY_NODE" . ,netica.ffi:*utility-node*)
    ("CONSTANT_NODE" . ,netica.ffi:*constant-node*)
    ("DISCONNECTED_NODE" . ,netica.ffi:*disconnected-node*)))

(defun error-categories (err)
  "Return all categories the error belongs"
  (loop for (name . code) in *error-category-map* 
        when (truep (netica.ffi:error-category-ns code err))
          collect name))

(defun error-message (err)
  "Format error message"
  (format nil "~s(~s)~@[ ~s~]: ~s~%"
          (car (rassoc (netica.ffi:error-severity-ns err) *error-severity-map*)) 
          (netica.ffi:error-number-ns err)
          (error-categories err)
          (netica.ffi:error-message-ns err)))

(defun print-verbose (fmt-string &rest args)
  (when *verbose*
    (apply #'format *verbose*
           (concatenate 'string "~&;; " fmt-string)
           args)))

(defmacro defapi (name varlist &body body)
  `(defun ,name ,(append varlist '(((:env *env*) *env*) ((:verbose *verbose*) *verbose*)))
     ,@body))

(defun convert-to-foreign-array (value type size)
  (let* ((vec (ff:allocate-fobject type :foreign-static-gc size))
         (address (ff:fslot-address vec)))
    (typecase value
      (vector
       (loop for v across value
             for offset from 0 by (ff:sizeof-fobject type)
             do (setf (ff:fslot-value-typed type :c (+ address offset)) v)))
      (list
       (loop for v in value
             for offset from 0 by (ff:sizeof-fobject type)
             do (setf (ff:fslot-value-typed type :c (+ address offset)) v))))
    vec))

;;; APIs 
(define-condition netica-condition (simple-condition)
  ()
  (:documentation "Base condition class for Netica error reports"))

(define-condition netica-error (netica-condition simple-error)
  ()
  (:documentation "Error class for Netica error reports"))

(defun check-errors (&key ((:env *env*) *env*) (clear t) (signalp t))
  "Check and report all errors, clear them by default.
If signalp is true, will (error) for XXX_ERR and ERROR_ERR and (warn) for WARNING_ERR and NOTICE_ERR,
otherwise will only print verbosely.
For REPORT_ERR and NOTHING_ERR, message will be printed verbosely."
  (labels ((clear-error (err)
             (when clear
               (netica.ffi:clear-error-ns err))))
    (let ((err *null*))
      (loop for e in *error-severity-map*
            for severity = (cdr e)
            do (loop
                 (setf err (netica.ffi:get-error-ns *env* severity err))
                 (when (nullp err)
                   (return))
                 (case severity
                   ((#.netica.ffi:*xxx-err* #.netica.ffi:*error-err*)
                    (unwind-protect
                         (if signalp
                             (cerror "Ignore error"
                                     'netica-error
                                     :format-control (error-message err))
                             (print-verbose (error-message err)))
                      (clear-error err)))
                   ((#.netica.ffi:*warning-err* #.netica.ffi:*notice-err*)
                    (unwind-protect
                         (if signalp
                             (warn 'netica-condition :format-control (error-message err))
                             (print-verbose (error-message err)))
                      (clear-error err)))
                   ((#.netica.ffi:*report-err* #.netica.ffi:*nothing-err*)
                    (unwind-protect
                         (print-verbose (error-message err))
                      (clear-error err)))
                   (t
                    (warn "Unknown error type ~s, no errors reported" severity))))))))

(defun clear-errors (&key ((:env *env*) *env*) severity)
  "Clear errors below severity level, if severity is not specified, it will clear all errors.
Severity is a string."
  (let ((severity (or (cdr (assoc severity *error-severity-map* :test 'equal))
                      netica.ffi:*xxx-err*)))
    (netica.ffi:clear-errors-ns *env* severity)))

(defun start-netica (&key ((:license *license*) *license*) checking maxmem
                       ((:verbose *verbose*) *verbose*))
  "Start netica, initialize it, and return the initialized environment.
Checking level and maxmem can be specified, by default, checking is \"REGULAR_CHECK\" and maxmem is 2d9.
Checking is a string, maxmem is a number.
Special variable *env* will be set on success."
  ;; make sure *env* is not initialized
  (when *env*
    (warn "Netica already running, use stop-netica to terminal current Netica session.")
    (return-from start-netica))
  (let ((env (netica.ffi:new-netica-environ-ns *license* *null* *null*)))
    (when (nullp env)
      (error 'netica-error
             :format-control "Netica License Key not accepted.~%Make sure key starts with a + and ends with five digit security code."))
    ;; initialize netica
    (multiple-value-bind (res mesg)
        (netica.ffi:init-netica2-bn env)
      (when (< res 0)
        (error 'netica-error
               :format-control "Cannot initialize Netica.~%Retval: ~a~%Message:~a"
               :format-arguments (list res mesg)))
      (print-verbose mesg))
    ;; setup argument checking and memory limit
    (let* ((checking (cdr (assoc (or checking "REGULAR_CHECK") *argument-checking-map*
                                 :test 'equal)))
           (maxmem (max (float (or maxmem 2d9) 1d0) 2d5)))
      (netica.ffi:argument-checking-ns checking env)
      (netica.ffi:limit-memory-usage-ns maxmem env))
    ;;
    (print-verbose "Netica successfully started.")
    ;; update *env*
    (setf *env* env)))

(defun close-netica (&key (env *env*)
                       ((:verbose *verbose*) *verbose*))
  "Terminate the netica session.
*env* will be set to NIL."
  ;; check env
  (unless env
    (warn "Netica not running, nothing to do."))
  ;; report errors if exists
  (check-errors :signalp nil :env *env*)
  ;;
  (print-verbose "Shut down any remaining nets.")
  
  ;;
  (print-verbose "Shut down Netica session.")
  (multiple-value-bind (res mesg)
      (netica.ffi:close-netica-bn env)
    (if (< res 0)
        (cerror "Continue shutting down Netica"
                'netica-error
                :format-control "Cannot shut down Netica session.~%Retval: ~a~%Message: ~a"
                :format-arguments (list res mesg))
        (print-verbose mesg))
    (setf *env* nil)))

(defapi make-net (&key (name (symbol-name (gensym "NET")))
                       (comment nil)
                       (title nil))
  "Make a network with a given name/comment/title and return it."
  (let ((net (netica.ffi:new-net-bn name *env*)))
    (print-verbose "new net ~s: ~s~%" name net) 
    (check-errors)
    (when comment
      (netica.ffi:set-net-comment-bn net comment))
    (check-errors)
    (when title
      (netica.ffi:set-net-title-bn net title))
    (check-errors)
    net))

(defun net-info (net &key (out *standard-output*))
  "Print information about the net."
  (format out "~&net: ~s~%name: ~s~%" net (netica.ffi:get-net-name-bn net))
  (let ((title (netica.ffi:get-net-title-bn net)))
    (unless (zerop (length title))
      (format out "title: ~s~%" title)))
  (let ((comment (netica.ffi:get-net-comment-bn net)))
    (unless (zerop (length comment))
      (format out "comment: ~s~%" comment)))
  (let ((file-name (netica.ffi:get-net-file-name-bn net)))
    (unless (zerop (length file-name))
      (format out "file-name: ~s~%" file-name)))
  (let* ((nodes (netica.ffi:get-net-nodes-bn net))
         (count (netica.ffi:length-node-list-bn nodes)))
    (dotimes (ii count)
      (node-info (netica.ffi:nth-node-bn nodes ii) :header ii)))
  (check-errors))

(defapi make-node (&key (name (symbol-name (gensym "NODE")))
                        (net (error 'program-error :format-control ":NET must be specified for MAKE-NODE"))
                        (kind .ffi:*nature-node*)
                        levels
                        states 
                        title
                        comment
                        parents
                        cpt
                        x
                        y)
  "Make a network node with the given parameters and return it.
The parameters are: name, net, kind, states (state name list),
levels (vector), parents list, cpt.
CPT (conditional probability table) is a list of conses:
 ((parent-state-vector . node-state-probability-vector) ...)
one cons for each combination of possible parent states,
where parent-state-vector is a vector of parent states,
 its length being (length parents);
and node-state-probability-vector is a vector of corresponding node state
 probabilities, its length being (length states).
When LEVELS is supplied, the node is continuous.
X & Y are coordinates; both or neither must be supplied."
  (let* ((node (netica.ffi:new-node-bn name (if levels 0 (length states)) net))
         (num-states (if levels (1- (length levels)) (length states))))
    (print-verbose "new node ~s: ~s~%" name node)
    (check-errors)
    (when (/= kind netica.ffi:*nature-node*)
      (netica.ffi:set-node-kind-bn node kind)
      (check-errors))
    (when levels
      (netica.ffi:set-node-levels-bn node num-states levels)
      (check-errors))
    (loop for state in states
          for idx upfrom 0
          do (if (consp state)
                 (progn
                   (netica.ffi:set-node-state-name-bn node idx (car state))
                   (netica.ffi:set-node-state-title-bn node idx (cdr state)))
                 (netica.ffi:set-node-state-name-bn node idx state))
             (check-errors))
    (when title
      (netica.ffi:set-node-title-bn node title)
      (check-errors))
    (when comment
      (netica.ffi:set-node-comment-bn node comment)
      (check-errors))
    (dolist (parent parents)
      (netica.ffi:add-link-bn parent node)
      (check-errors))
    (dolist (probs cpt)
      (let* ((parent-state-vec (convert-to-foreign-array
                                (map 'vector #'netica.ffi:get-state-named-bn (car probs) parents)
                                'netica.ffi:ff-state-bn
                                (length (car probs))))
             (prob-vec (convert-to-foreign-array
                        (cdr probs)
                        'netica.ffi:ff-prob-bn
                        (length (cdr probs)))))
        (netica.ffi:set-node-probs-bn node parent-state-vec prob-vec))
      (check-errors))
    (when (or x y)
      (if (and x y)
          (netica.ffi:set-node-vis-position-bn node *null* x y)
          (cerror "ignore the supplied argument"
                  'program-error
                  :format-control "If one of X (~S) and Y (~S) is supplied, both must be"
                  :format-arguments (list x y))))
    (check-errors)
    node))

(defun node-info (node &key header (out *standard-output*))
  "Print information about the node."
  (format out "~&~@[ * [~s] ~]node: ~s (net: ~s)~%name: ~s   (~s ~s)~%"
          header node (netica.ffi:get-node-net-bn node)
          (netica.ffi:get-node-name-bn node)
          (car (rassoc (netica.ffi:get-node-type-bn node) *node-type-map*))
          (car (rassoc (netica.ffi:get-node-kind-bn node) *node-kind-map*)))
  (let ((title (netica.ffi:get-node-title-bn node)))
    (unless (zerop (length title))
      (format out "title: ~s~%" title)))
  (multiple-value-bind (x y) (netica.ffi:get-node-vis-position-bn node *null*)
    (format out "position: (~s ~s)~%" x y))
  (let ((count (netica.ffi:get-node-number-states-bn node)))
    (format out "state count: ~:d~%" count)
    (dotimes (state count)
      (let ((title (netica.ffi:get-node-state-title-bn node state)))
        (format out "[~:d] name: ~s~[~:;  title: ~s~]~%" state
                (netica.ffi:get-node-state-name-bn node state)
                (length title) title))))
  (let* ((nodes (netica.ffi:get-node-children-bn node))
         (count (netica.ffi:length-node-list-bn nodes)))
    (if (zerop count) (format out "no children~%")
        (loop initially (format out "~:d ~:*~[~;child~:;children~]:~%" count)
              for ii from 0 to (1- count)
              for child = (netica.ffi:nth-node-bn nodes ii)
              do (format out "[~:d] ~s (~s)~%" ii
                         (netica.ffi:get-node-name-bn child) child))))
  (let* ((nodes (netica.ffi:get-node-parents-bn node))
         (count (netica.ffi:length-node-list-bn nodes)))
    (if (zerop count) (format out "no parents~%")
        (loop initially (format out "~:d parent~:p:~%" count)
              for ii from 0 to (1- count)
              for parent = (netica.ffi:nth-node-bn nodes ii)
              do (format out "[~:d] ~s (~s)~%" ii
                         (netica.ffi:get-node-name-bn parent) parent)))) 
  (let ((levels (get-node-levels node))) 
    (dotimes (ii (length levels))
      (format out "[~:d] level: ~s~%" ii (aref levels ii))))
  (check-errors))

(defapi get-beliefs (node &key)
  "Get the belief vector for the node."
  (let ((beliefs (get-node-beliefs node))
        (name (netica.ffi:get-node-name-bn node))) 
    (check-errors)
    (loop :for belief :across beliefs :and index :upfrom 0 :do
      (print-verbose "~a: P(~s)=~f~%" name
                     (netica.ffi:get-node-state-name-bn node index) belief))
    (check-errors)
    beliefs))

(defapi enter-finding (net node state &key)
  "Enter a finding by node and state names"
  (let* ((nd (netica.ffi:get-node-named-bn node net))
         (st (netica.ffi:get-state-named-bn state nd)))
    (netica.ffi:enter-finding-bn nd st)
    (check-errors)
    (print-verbose "~s: set to ~s~%" node state)))

(defapi open-dne-file (file &key)
  (let ((out (netica.ffi:new-file-stream-ns
              (namestring (translate-logical-pathname
                           (merge-pathnames
                            file #.(make-pathname :type "dne"))))
              *env*
              *null*)))
    (print-verbose "new stream: ~s~&" out)
    (check-errors)
    out))

(defmacro with-open-dne-file ((var file &rest opts &key &allow-other-keys)
                              &body body)
  `(let ((,var (open-dne-file ,file ,@opts)))
     (unwind-protect (progn ,@body)
       (netica.ffi:delete-stream-ns ,var)
       (check-errors))))

(defapi save-net (net &key (file (netica.ffi:get-net-file-name-bn net)))
  "Save the network to the file."
  (with-open-dne-file (out file)
    (netica.ffi:write-net-bn net out)
    (check-errors)
    (print-verbose "saved ~s to ~s~%" net
                   (netica.ffi:get-net-file-name-bn net))))

(defapi read-net (file &key)
  (with-open-dne-file (in file)
    (let ((net (netica.ffi:read-net-bn in netica.ffi:*no-window*)))
      (check-errors)
      net)))

(pushnew :netica *features*)


