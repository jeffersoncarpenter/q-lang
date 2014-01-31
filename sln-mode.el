(setq sln-mode nil)

(defun sln-mode-on (sln-file-path)
  "Turns on sln-mode!"
  (setq sln-directory (file-name-directory sln-file-path))
  (setq sln-projects ())
  (setq sln-project-paths ())
  (setq sln-files ())
  (sln-process-solution-file sln-file-path))

(defun sln-process-solution-file (solution-file-path)
  "Parses the solution file and takes all due actions"
  (with-temp-buffer
    (insert-file-contents solution-file-path)

    ;; find all of the projects in the solution
    (while (search-forward "Project(")

      ;; search forward to obtain project name
      (setq sln-project (buffer-substring
			 (search-forward "= \"")
			 (- (search-forward "\"") 1)))

      ;; search forward to obtain project path
      (setq sln-project-rel-path (buffer-substring
				    (search-forward ", \"")
				    (- (search-forward "\"") 1)))
      (setq sln-project-path (concat sln-directory sln-project-rel-path))

      (if (string-match "[a-zA-Z/]*c[cs]proj" sln-project-path)
	  (progn
	    (add-to-list 'sln-projects sln-project)
	    (add-to-list 'sln-project-paths `(,sln-project . ,sln-project-path))
      
	    ;; process project file
	    (sln-process-project-file sln-project)
	    )
	)
      )
    )
  )


(setq project-buffer-sizes ())

(defun sln-process-project-file (project-file)
  "Parses a project file and takes all due actions"
  (setq sln-project-path (cdr (assoc project-file sln-project-paths)))
  (with-temp-buffer
    (insert-file-contents sln-project-path)
    (add-to-list 'project-buffer-sizes (buffer-size))
    )
  )
      

(sln-mode-on "c:/tfs/geo.cqrs/Solomo.Geo.CQRS.sln")


(defun sln-browse-mode-on ()
  "turns on some stuff to use with sln-browse"
  (message "sln browse mode on")
  )

(defun sln-browse ()
  "Opens a buffer to browse the solution"
  (get-buffer-create "test")
  (switch-to-buffer-other-window "test")
  (insert "aoeu")
  (sln-browse-mode-on)
  )

(sln-browse)


(length project-buffer-sizes)
(car project-buffer-sizes)
(last project-buffer-sizes)

(car sln-projects)
(last sln-projects)
(length sln-projects)

(car sln-project-paths)
(last sln-project-paths)
(length sln-project-paths)
