;;; purged from condition.lisp

;;; so, some things.
;;; we don't want optimization notes to be ERRORs, of course, or even WARNINGs exactly.

;;; According to CLHS 3.2.5
;;; Conditions of type ERROR might be signaled by the compiler in situations
;;;   where the compilation cannot proceed without intervention.
;;; warnings might be signaled in situations where the compiler can determine
;;;   that the consequences are undefined or that a run-time error will be signaled.
;;; The compiler is permitted to issue warnings about matters of programming style as conditions of type style-warning.

;;; COMPILE and COMPILE-FILE both return multiple values indicating whether any errors (conditions of type ERROR)
;;;   or warnings (conditions of type (OR ERROR (AND WARNING (NOT STYLE-WARNING)))).

;;; Optimization notes obviously aren't run-time or undefinedness issues, and they're not really a programming style
;;;   thing or "substandard" (to quote the entry on STYLE-WARNING).

;;; Actually, that page does mention "inefficient" code as possibly causing style warnings.  But still, I think there's
;;;   a difference between e.g. leaving a variable unused, and a thousand notes about (* a a) not being as efficient
;;;   as it would be if a was declare'd fixnum.

;;; I guess that should be enough for me to cut this short and just subclass style-warning.

;;; actually my above description of compile and compile-file's values is wrong: the first value is false if no
;;;   WARNINGs or ERRORs were signaled, the third if no (OR ERROR (AND WARNING (NOT STYLE-WARNING)))s were signaled.
;;; so, style-warns will show up in the third value and all.  but i think that's ok, at least for now.
;;; targeted suppresion mechanisms would be nice.

;;; guess what i'm not at all sure about expansion-abortion either.  the functional mechanism makes sense
;;;   (telling the macroexpander that this form will not expand) but i dunno about the conditions

;;; oh i had an idea.  use slots

;;; nope.  abortion gone for now

;;; rejiggering so that not all optimization notes require expansion to stop

;;; type hierarchy's getting messy.
