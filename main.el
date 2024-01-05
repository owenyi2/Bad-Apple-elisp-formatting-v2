(require 'cl-lib)

(setq gc-cons-threshold (* 511 1024 1024))
(defun lisp--match-hidden-arg (limit) nil) 
(defalias 'mv-setq 'cl-multiple-value-setq)

(defun split-input (text-string)
  (setq text-list nil)
  (setq current-word "")
  (setq inside-string nil)

  (cl-loop for c across text-string do
	   (progn
	     (if inside-string
		 (progn
		   (setq current-word (concat current-word (string c)))
		   (if (= c inside-string)
		       (progn
			 (setq text-list (cons current-word text-list))
			 (setq current-word "")
			 (setq inside-string nil)
			 )
		     )
		   )
	       (if (member c '(9 32 10 40 41))
		   (progn
		     (if (not (string= current-word ""))
			 (progn
			   (setq text-list (cons current-word text-list))
			   (setq current-word "")
			   )
		       )
		     (if (member c '(40 41))
			 (setq text-list (cons (string c) text-list))
		       )
		     )
		 (if (= c 34)
		     (progn
		       (if (not (string= current-word ""))
			   (progn
			     (setq text-list (cons current-word text-list))
			     (setq current-word "")
			     )
			 )
		       (setq current-word (concat current-word (string c)))
		       (setq inside-string c)
		       )
		   (setq current-word (concat current-word (string c)))
		   )
		 )
	       )
	     )
	   )
  (if current-word
      (setq text-list (cons current-word text-list))
    )
  (reverse text-list)
  )





(defun handle-new-line (width prev-end this-row prev-row wrapped-text)
    (setq wrapped-text (concat wrapped-text (make-string (- width prev-end) ? )))
    (setq wrapped-text (concat wrapped-text (make-string (- this-row prev-row) ?\n))) 
    )

(defun add-space (lisp-list num-space)
  (let (output)
  (setq list-length (length lisp-list))
  (if (< list-length 3)
      (progn
	(setq output (car lisp-list))
	(setq output (concat output (make-string num-space ? )))
	(dolist (elt (cdr lisp-list) output)
	  (setq output (concat output elt))
	  )
	)
    
    (setq divisor (- (length lisp-list) 2))
    (setq quotient (/ num-space divisor))
    (setq remainder (% num-space divisor))

    (setq q-space (make-string quotient (cl-coerce " " 'character)))
    (setq r-space (make-string remainder (cl-coerce " " 'character)))

    (setq output (car lisp-list))  
	(setq r-count 0)
	(dolist (elt (cdr lisp-list) output)
	  (setq output (concat output elt))
	  (setq output (concat output q-space))
	  (if (< r-count remainder)
	      (progn
		(setq output (concat output " "))
		(setq r-count (1+ r-count)) 
		)
	    )
	  )
	(if (equal quotient 0)
	    output
	  (substring output 0 (- 0 quotient))
	  )
	)
  )
  )


(defun process-slice (text-list slice-length word-count)
  (setq text-length 0)
  (setq short-list nil)

  (setq word (nth word-count text-list))

  
  (while (< (+ text-length (length word) 1) slice-length)
	   (setq short-list (cons " " short-list))
	   (setq short-list (cons word short-list))
	   (setq text-length (+ text-length (length word) 1))
	   (setq word-count (% (1+ word-count) (length text-list)))
 

	   (setq word (nth word-count text-list))

	  
	   )
    

  (setq short-list (reverse short-list))
  (setq short-list (cdr short-list))
  (setq text-length (- text-length 1))

  
  (if short-list
      	(setq processed-slice (add-space short-list (- slice-length text-length)))	
    (setq processed-slice (make-string slice-length (cl-coerce " " 'character)))
    )
  (list processed-slice word-count)
  )

(defun wrap-text (slice-list text-list width height)
  (setq wrapped-text "")
  (setq word-count 0)
  (setq prev-slice '(0 0 0))

  (if (not slice-list)
      (progn
	(setq wrapped-text (concat wrapped-text (make-string height ?\n)))
	(setq this-row height)
	)
      )
  
  (while slice-list
      (setq wrapped-line "")
      (setq this-slice (car slice-list))
      (mv-setq (prev-row prev-start prev-end) prev-slice)
      (mv-setq (this-row this-start this-end) this-slice)

      (if ( > this-row prev-row)
	  (progn
	    (setq wrapped-text (handle-new-line width prev-end this-row prev-row wrapped-text))
	    (setq prev-end 0)
	    )
	)
       
      (setq wrapped-line (concat wrapped-line (make-string (- this-start prev-end) (cl-coerce " " 'character))))
      (setq slice-length (- this-end this-start))

 
      (mv-setq (processed-slice word-count) (process-slice text-list slice-length word-count))
      
      (setq wrapped-line (concat wrapped-line processed-slice))
      (setq wrapped-text (concat wrapped-text wrapped-line))
      
      (setq slice-list (cdr slice-list))
      (setq prev-slice this-slice)
      )

    (setq wrapped-text (concat wrapped-text (make-string (- height this-row) ?\n)))
    (setq wrapped-text (concat wrapped-text "\n\n"))
    (if (< word-count (length text-list))
	(setq wrapped-text (concat wrapped-text "\n" (mapconcat 'identity (cl-subseq text-list word-count (length text-list)) " ")))
	)

    wrapped-text
    )


(defun main (text-list)
  (setq frame-number 0)
  
  (switch-to-buffer "*scratch*")
  (erase-buffer)
  (text-scale-set -3)

  (switch-to-buffer "*temporary*")
  (buffer-disable-undo)
  (erase-buffer)
  (insert-file-contents "./slice-list.el")
  (goto-char (point-min))

  (setq frame-start-time (float-time))
  (setq long-pause-time 0.06)
  (setq short-pause-time 0.02)

  (garbage-collect)
  (while (not (eobp))
      (setq slice-list (thing-at-point 'line))
      (setq slice-list (cl-subseq slice-list 0 (- (length slice-list) 1)))
      (setq slice-list (eval (car (read-from-string slice-list))))

      (setq output (wrap-text slice-list text-list 180 54))

      (switch-to-buffer "*scratch*")
      (erase-buffer)
      (insert output)
      (goto-char (point-min))
     
      (setq frame-number (+ 2 frame-number))

      (if (equal (% frame-number 3) 0) 
	  (progn
	    (garbage-collect)
	    (message "time: %.3f | frame %s \n"  (- (float-time) frame-start-time) frame-number)
	    (sit-for (- long-pause-time (- (float-time) frame-start-time)))
	    
	    )

	(message "time: %.3f | frame %s \n"  (- (float-time) frame-start-time) frame-number)
	(sit-for (- short-pause-time (- (float-time) frame-start-time)))
	)

      (setq frame-start-time (float-time))
      
      (switch-to-buffer "*temporary*")
      (forward-line 1)
    
 

    )
    (switch-to-buffer "*scratch*")
  )

(with-temp-buffer
  (insert-file-contents "./main.el")
  (setq text-string (buffer-substring-no-properties (point-min) (point-max)))
  (setq text-list (split-input text-string))
  )

(while-no-input (main text-list))
