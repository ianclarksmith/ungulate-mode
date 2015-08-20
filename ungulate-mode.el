(require 'request)
(require 'json)

(defvar ungulate-http-port 8080
  "HTTP port which Rhino is listening on.")

(defvar ungulate-http-host "localhost"
  "HTTP host which Rhino is listening on.")

(defvar ungulate-rhino-path nil
  "Path to Rhinoceros.
Set whenever `ungulate-rhino-is-listening' is run.")

(defun ungulate-eval-buffer ()
  (interactive)
  (let* ((temp-file (make-temp-file "rhinoscript" nil ".py")))
    (write-region nil nil temp-file)
    (request (format "http://%s:%s/runpythonscriptfile"
                     ungulate-http-host
                     ungulate-http-port)
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

(defun ungulate-rhino-is-listening ()
  (request (format "http://%s:%s/ping" ungulate-http-host ungulate-http-port)
           :parser 'json-read
           :success (function*
                     (lambda (&key data &allow-other-keys)
                       (setq ungulate-rhino-path (assoc-default 'msg data))
                       (message "%s" data)))
           :error (message "Rhino is not listening.")))

(defun ungulate-bring-rhino-to-front ()
  "Bring Rhinoceros to front (OS X only)."
  (interactive)
  (start-process "" nil "open" ungulate-rhino-path))

(provide 'ungulate-mode)
