;;; ungulate-mode.el --- Minor mode for RhinoPython scripting

;; Copyright 2015 Ian Clark Smith

;; Author: Ian Clark Smith <ian@sumisu.co>
;; URL: http://github.com/ianclarksmith/ungulate-mode
;; Keywords: tools
;; Package-Requires: ((request "0.2.0"))

;;; Commentary:

;; A minor mode for editing RhinoPython scripts
;;
;; Usage:
;; - To use, enter the command StartAtomEditorListener from within Rhinoceros
;; - Activate ungulate-mode in a buffer containing RhinoPython code
;; - Run `ungulate-eval-buffer' or C-c C-c in an ungulate-mode buffer
;;
;; Troubleshooting:
;; - Note that on some systems the loopback address (localhost) defaults to IPv6
;;   although the Rhino editor server only listens on IPv4. To ensure that
;;   requests are dispatched properly, create or edit ~/.curlrc and add the line
;;   "ipv4" without quotes.

;;; Code:

(require 'request)
(require 'json)

(defvar ungulate-http-port 8080
  "HTTP port which Rhino is listening on.")

(defvar ungulate-http-host "localhost"
  "HTTP host which Rhino is listening on.")

(defvar ungulate-rhino-path nil
  "Path to Rhinoceros.
Set whenever `ungulate-rhino-is-listening' is run.")

(defvar ungulate-foreground-on-eval-p t
  "Bring Rhino to foreground unless nil.")

(defun ungulate--rhino-endpoint (method)
  "Convenience function for assembling a request endpoint."
  (format "http://%s:%s/%s" ungulate-http-host ungulate-http-port method))

(defun ungulate-eval-buffer ()
  "Evaluate the contents of the current buffer in Rhino.

To do this, the buffer is saved as a temporary file with a .py extension and
sent to Rhino to run as a Python script."
  (interactive)
  (let* ((temp-file (make-temp-file "rhinoscript" nil ".py")))
    (write-region nil nil temp-file)
    (request (ungulate--rhino-endpoint "runpythonscriptfile")
             :type "POST"
             :data (json-encode `(("FileName" . ,temp-file)))
             :success (function*
                       (lambda (&key data &allow-other-keys)
                         (message "Buffer sent to Rhino successfully.")
                         (if ungulate-foreground-on-eval-p
                             (ungulate-bring-rhino-to-front)))))))

(define-minor-mode ungulate-mode
  "Why shave yaks when you can pet rhinos?"
  :lighter " rhino"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'ungulate-eval-buffer)
            map))

(defun ungulate-rhino-is-listening ()
  (interactive)
  (request (ungulate--rhino-endpoint "ping")
           :parser 'json-read
           :success (function*
                     (lambda (&key data &allow-other-keys)
                       (setq ungulate-rhino-path (assoc-default 'msg data))
                       (message "%s" data)))
           :error (message "Rhino is not listening.")))

(defun ungulate-bring-rhino-to-front ()
  "Bring Rhinoceros to front (OS X only)."
  (interactive)
  (if ungulate-rhino-path
      (start-process "" nil "open" ungulate-rhino-path))
  (message "Please set ungulate-rhino-path or run `ungulate-rhino-is-listening'"))

(provide 'ungulate-mode)
;;; ungulate-mode.el ends here
