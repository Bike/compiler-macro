;;;; Modified by Bike for use in sandalphon.compiler-macro.
;;;; macro SORT is renamed INLINE-SORT
;;;; package renamed
;;;; all sbcl hooks replaced
;;;; documentation changed slightly appropriately
;;;; package exports INLINE-SORT, SORT, STABLE-SORT rather than just SORT
;;;; add requirement for alexandria (to avoid sb-kernel:%coerce-callable-to-fun, and because with-gensyms is darn convenient)

;;;; unmodified at http://discontinuity.info/~pkhuong/tabasco-sort.lisp

;;;; Tabasco Sort v 1.0.1
;;;;
;;;; Generates relatively-small unrolled and inlined stable merge
;;;; sorts.
;;;;
;;;; Usage:
;;;; (tabasco:inline-sort (#'< :key #'-)
;;;;   (aref array 0) (aref array 1) (aref array 2))
;;;; will sort the values in ARRAY's first, second and third slots,
;;;; write the values back in order, and return them as multiple
;;;; values.
;;;;
;;;; The predicate and key function are interpreted exactly as in
;;;; CL:STABLE-SORT.
;;;;
;;;; (tabasco:inline-sort (#'< :overwrite nil) ...) will sort the values,
;;;; without writing them back to the places.  If the overwrite
;;;; argument is a literal NIL, the overwriting code is never
;;;; even emitted, and the macro will work without any issue on
;;;; non-place values (e.g. function calls).
;;;;
;;;; See http://pvk.ca/Blog/2012/08/27/tabasco-sort-super-optimal-merge-sort/
;;;; for more in-depth explanations.
;;;;
;;;; Release history
;;;;
;;;; * 2012-08-28
;;;;  Only enable the transform when (> speed compilation-speed) or
;;;;  when SBCL's generic sort would be inlined anyway.
;;;;  Suggested by Lutz Euler on #sbcl.
;;;;
;;;; * 2012-08-26
;;;;  Initial release: stable merge sort generator and SBCL hooks.

;;;; License: Modified BSD
;;;; License Copyright (c) 2012, Paul-Virak Khuong
;;;;  All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;; Redistributions of source code must retain the above copyright
;;;; notice, this list of conditions and the following disclaimer.
;;;; Redistributions in binary form must reproduce the above copyright
;;;; notice, this list of conditions and the following disclaimer in the
;;;; documentation and/or other materials provided with the distribution.
;;;;
;;;; Neither the name of the Million Monkey Enterprises nor the names of
;;;; its contributors may be used to endorse or promote products derived
;;;; from this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defpackage "SANDALPHON.COMPILER-MACRO-TEST/TABASCO-SORT"
  (:use "CL" "SANDALPHON.COMPILER-MACRO")
  (:shadow "SORT" "STABLE-SORT")
  (:export "SORT" "STABLE-SORT" "INLINE-SORT")
  (:nicknames "TABASCO"))

(in-package "SANDALPHON.COMPILER-MACRO-TEST/TABASCO-SORT")

;;;; Implement permutations with a cycle decomposition
;;;;
;;;; A mapping like a b c d
;;;;                b a c d
;;;;
;;;; is re-expressed as (a b) (c) (d), and each cycle is executed
;;;; as a rotatef -- a clever native-code compiler could handle it
;;;; all by itself (any SSA-based backend should, for instance), but
;;;; I don't know any CL compiler that's up to it.
(defun find-cycle (mapping)
  "Extract an arbitrary cycle from a non-empty mapping,
   returning both the cycle and the rest of the mapping."
  (assert mapping)
  (let* ((head  (pop mapping))
         (cycle (list (cdr head))))
    (loop
     (let* ((next-source (first cycle))
            (pair        (assoc next-source mapping)))
       (cond (pair
              (push (cdr pair) cycle)
              ;; if this sucks enough to matter, the output
              ;; will be humongous anyway
              (setf mapping (remove pair mapping)))
             (t
              (assert (eql next-source (first head)))
              (return (values cycle mapping))))))))

(defun emit-permute (destinations sources)
  "Emit a [destinations <- sources] permutation via its
   cycle decomposition"
  ;; source -> destination alist, minus trivial pairs
  (let ((mapping (remove-if (lambda (pair)
                              (eql (car pair) (cdr pair)))
                            (pairlis sources destinations))))
    `(progn
       ,@(loop
           while mapping
           collect
           (multiple-value-bind (cycle new-mapping)
               (find-cycle mapping)
             (setf mapping new-mapping)
             `(rotatef ,@cycle))))))

;;;; * COMPARATOR and KEY are forms to splice in as functions;
;;;; * DESTINATIONS holds the variables that'll hold the sorted
;;;;   merged values;
;;;; * {LEFT,RIGHT}-HEAD are used to cache the result from calling
;;;;   KEY on the first element in the left or right sequence of
;;;;   values.
(defvar *inline-sort-comparator*)
(defvar *inline-sort-key*)
(defvar *inline-sort-destinations*)
(defvar *inline-sort-left-head*)
(defvar *inline-sort-right-head*)

(defun emit-merge-1 (left right acc)
  "Build a search tree to determine the right permutation to
   merge LEFT and RIGHT, given that each is pre-sorted."
  ;; stability trickery
  `(if (funcall ,*inline-sort-comparator* ,*inline-sort-right-head*
                                          ,*inline-sort-left-head*)
       ,(let* ((acc        (cons (first right) acc))
               (right      (rest right)))
          ;; pop from RIGHT, and recurse if RIGHT isn't empty.
          (if right
              `(let ((,*inline-sort-right-head*
                       (funcall ,*inline-sort-key* ,(first right))))
                 ,(emit-merge-1 left right acc))
              (emit-permute *inline-sort-destinations*
                            (append (reverse acc) left))))
       ;; same
       ,(let* ((acc  (cons (first left) acc))
               (left (rest left)))
          (if left
              `(let ((,*inline-sort-left-head*
                       (funcall ,*inline-sort-key* ,(first left))))
                 ,(emit-merge-1 left right acc))
              (emit-permute *inline-sort-destinations*
                            (append (reverse acc) right))))))

(defun emit-merge (left right)
  "Caching calls to KEY means we have to special-case empty lists
   (which doesn't happen when we sort, anyway)"
  (cond ((null left)
         (emit-permute right right))
        ((null right)
         (emit-permute left left))
        (t
         (let ((*inline-sort-destinations* (append left right))
               (*inline-sort-left-head*  (gensym "LEFT-HEAD-KEY"))
               (*inline-sort-right-head* (gensym "RIGHT-HEAD-KEY")))
           `(let ((,*inline-sort-left-head*  (funcall ,*inline-sort-key*
                                                      ,(first left)))
                  (,*inline-sort-right-head* (funcall ,*inline-sort-key*
                                                      ,(first right))))
              ,(emit-merge-1 left right nil))))))

(defun emit-sort-1 (values length)
  "Unrolled and inlined recursive merge sort generator.
   Lists of length 1 or less are trivially sorted; recurse
   on the rest."
  (when (> length 1)
    (let* ((split (truncate length 2))
           (left  (subseq values 0 split))
           (right (subseq values split)))
      `(progn
         ,(emit-sort-1 left  split)
         ,(emit-sort-1 right (- length split))
         ,(emit-merge left right)))))

(defun emit-sort (values *inline-sort-comparator* *inline-sort-key*)
  (emit-sort-1 values (length values)))

;;;; Hide all that machinery behind a friendly macro.
(defmacro inline-sort ((comparator &key key (overwrite t))
		       &body values
		       &environment env)
  "Sorts all VALUES in ascending order with respect to COMPARATOR and
   KEY.  COMPARATOR should be a strict order, like CL:<, and KEY defaults
   to NIL (which is interpreted as the identity).  By default, the result
   is written back to the places; that's skipped if OVERWRITE is NIL. A
   literal NIL value for overwrite will avoid generating any write.
   The SORT form always evaluates to the sorted values, in order."
  (let (vars vals
        store-vars writer-forms
        reader-forms
        temps
        (_comparator (gensym "COMPARATOR"))
        (_key        (gensym "KEY"))
        (_overwrite  (gensym "OVERWRITE")))
    (loop for value in (reverse values) do
      (push (gensym "TEMP") temps)
      ;; only use the setf expansion if we might write to the place.
      (if (not overwrite)
          (push value reader-forms)
          (multiple-value-bind (var val store-var writer reader)
              (get-setf-expansion value env)
            (setf vars (append var vars)
                  vals (append val vals))
            (push store-var store-vars)
            (push writer writer-forms)
            (push reader reader-forms))))
    `(let* ((,_comparator ,comparator)
            (,_key        ,(or key '#'identity))
            (,_overwrite  ,overwrite)
            ,@(mapcar 'list vars vals)
            ,@(mapcar 'list temps reader-forms))
       (declare (ignorable ,_overwrite))
       ,(emit-sort temps _comparator _key)
       ,(and overwrite
             `(when ,_overwrite
                ,@(loop
                    for value in values
                    for store-var-list in store-vars
                    for writer in writer-forms
                    for temp in temps
                    collect
                    (progn
                      (unless (= 1 (length store-var-list))
                        (error "Can't sort multiple-value place ~S" value))
                      `(let ((,(first store-var-list) ,temp))
                         ,writer)))))
       (values ,@temps))))


;;;; compiler macro things start here

(declaim (inline sort))
(defun sort (seq pred &key key)
  (cl:sort seq pred :key key))

(declaim (inline stable-sort))
(defun stable-sort (seq pred &key key)
  (cl:stable-sort seq pred :key key))

(define-compiler-hinter sort (seq pred &key key))
(define-compiler-hinter stable-sort (seq pred &key key))

;; (and (array * *) (array * 4)) => (array * 4)
;; (or (array * *) (array * 4)) => (array * *)
;; (and (array * 4) (array * (1 2 3 4))) => (array * (1 2 3 4))
;; (or (array * 4) (array * (1 2 3 4))) => (array * 4)

;;; gave up, copied from sb-c (ranked arrays are useless for the expansion anyway)

(defun array-type-dimensions-or-give-up (type)
  (labels ((maybe-array-type-dimensions (type)
             (kindcase type
               (array-type
                (array-type-dimensions type))
               (union-type
                (let* ((types (remove nil (mapcar #'maybe-array-type-dimensions
                                                  (union-type-types type))))
                       (result (car types)))
                  (dolist (other (cdr types) result)
                    (unless (equal result other)
                      (decline-expansion
                       "~@<dimensions of arrays in union type ~S do not match~:@>"
                       #+(or)(type-specifier type) type)))))
               (intersection-type
                (let* ((types (remove nil (mapcar #'maybe-array-type-dimensions
                                                  (intersection-type-types type))))
                       (result (car types)))
                  (dolist (other (cdr types) result)
                    (unless (equal result other)
                      (abort-expansion
                       "~@<dimensions of arrays in intersection type ~S do not match~:@>"
                       #+(or)(type-specifier type) type))))))))
    (or (maybe-array-type-dimensions type)
        (decline-expansion
         "~@<don't know how to extract array dimensions from type ~S~:@>"
         #+(or)(type-specifier type) type))))

(defvar *unrolled-vector-sort-max-length* 8)

(defun maybe-emit-unrolled-merge-sort (fun sequence predicate key env)
  (unless (policy (and (> speed space)
		       (or (zerop space)
			   (> speed compilation-speed)))
		  env)
    (decline-expansion))
  (let* ((sequence-type (form-type sequence env))
	 (dimensions (array-type-dimensions-or-give-up
		      sequence-type)))
    (unless (typep dimensions '(cons integer null))
      (decline-expansion
       "~@<sequence argument isn't a vector of known length~:@>" #+ccl nil))
    (let ((length (first dimensions)))
       ;; TODO: change max length to be declaration-controlled rather than dynamically-controlled?
      (when (> length *unrolled-vector-sort-max-length*)
	(decline-expansion
	 "~@<sequence argument too long for unrolled sort ~
              (length ~S greater than ~S)~:@>"
	 length *unrolled-vector-sort-max-length*))
      (if (<= length 1)
	  `(prog1 ,sequence ,predicate ,key) ; gotta evaluate those args
	  (alexandria:with-gensyms (seqvar)
	    ;; remember evaluation order - sequence, then predicate, then key.
	    ;; (tabasco:inline-sort covers predicate and key)
	    `(let* ((,seqvar ,sequence))
	       (declare (type ,sequence-type ,seqvar))
	       (if (array-has-fill-pointer-p ,seqvar)
		   ;; only the elements up to the fill pointer should be sorted.
		   ;; since that varies, just give up (at runtime...) if necessary
		   (locally (declare (notinline ,fun)) ; don't expand recursively (kind of a big hammer...)
		     (,fun ,seqvar ,predicate :key ,key))
		   (locally (declare #+sbcl (optimize (sb-c::insert-array-bounds-checks 0)))
		     (tabasco:inline-sort ((alexandria:ensure-function ,predicate)
					   ,@(when key `(:key (alexandria:ensure-function ,key))))
					  ,@(loop for i below length collect
						 `(aref ,seqvar i)))))
	       ,seqvar))))))

(define-compiler-hint sort (sequence predicate &key key &environment env)
  "Unroll sort of short vectors." 
  (maybe-emit-unrolled-merge-sort 'sort sequence predicate key env))

(define-compiler-hint stable-sort (sequence predicate &key key &environment env)
  "Unroll sort of short vectors."
  (maybe-emit-unrolled-merge-sort 'stable-sort sequence predicate key env))
