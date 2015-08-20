(require 'request)
(require 'json)

(defun ungulate-eval-buffer ()
  (interactive)
  (let* ((temp-file (make-temp-file "rhinoscript" nil ".py")))
    (write-region nil nil temp-file)
    (request "http://localhost:8080/runpythonscriptfile"
             :type "POST"
             :data (json-encode `(("FileName" . ,temp-file)))
             :success (function*
                       (lambda (&key data &allow-other-keys)
                         (message "Buffer sent to Rhino successfully."))))))

(define-minor-mode ungulate-mode
  "Why shave yaks when you can pet rhinos?"
  :lighter " rhino"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'ungulate-eval-buffer)
            map))

(provide 'ungulate-mode)
