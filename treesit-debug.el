;;; treesit-debug.el --- Treesit.el debugger port -*- lexical-binding: t; coding: utf-8 -*-

;;; Commentary:

;; This file contains debug utilities for tree-sitter.
;;
;; (treesit-debug-mode)

;;; Code:

(require 'treesit)

(defvar-local treesit-debug--tree-buffer nil
  "Buffer used to display the syntax tree of this buffer.")

(defvar-local treesit-debug--source-code-buffer nil
  "Source buffer of the syntax tree displayed in this `treesit-debug' buffer.")

(defgroup treesit-debug nil
  "Tree sitter debug and display features."
  :group 'treesit)

(defcustom treesit-debug-jump-buttons nil
  "Whether to enable jump-to-node buttons in `treesit-debug' views.
This can have a performance penalty in large buffers."
  :type 'boolean
  :group 'treesit-debug)

(defcustom treesit-debug-highlight-jump-region nil
  "Whether to highlight the node jumped to.
This only takes effect if `treesit-debug-jump-buttons' is non-nil."
  :type 'boolean
  :group 'treesit-debug)

(defun treesit-dfs-with-depth (node pred &optional step depth)
  "Traverse the subtree of NODE depth-first.

Traverse starting from NODE (i.e., NODE is passed to PRED).  For
each node traversed, call PRED with the node, stop and return the
node if PRED returns non-nil.  If STEP >= 0 or nil, go forward,
if STEP < 0, go backward.  If no node satisfies PRED, return
nil.

DEPTH can be a positive integer or 0, meaning go DEPTH deep
counting from NODE; or nil, meaning there is no limit."
  (let ((real-depth (or depth 0)))
    (if (funcall pred node real-depth)
        node
      (cl-loop for child in (if (or (null step) (>= step 0))
                                (treesit-node-children node)
                              (nreverse (treesit-node-children node)))
               if (treesit-dfs-with-depth
                   child pred step (1+ real-depth))
               return child))))

(defun treesit-debug--goto-node (buffer range)
  "Switch to BUFFER, centering on the region defined by NODE."
  (switch-to-buffer-other-window buffer)
  (goto-char (car range))
  (push-mark (cdr range) "Jumped" treesit-debug-highlight-jump-region))

(defun treesit-debug--button-node-lookup (button)
  "The function to call when a `treesit-debug' BUTTON is clicked."
  (unless treesit-debug--source-code-buffer
    (error "No source code buffer set"))
  (unless (buffer-live-p treesit-debug--source-code-buffer)
    (user-error "Source code buffer has been killed"))
  (unless button
    (user-error "This function must be called on a button"))
  (treesit-debug--goto-node treesit-debug--source-code-buffer
                            (button-get button 'points-to)))

(defun treesit-debug--display-node (node depth)
  "Function (predicate) to run at every node."
  (insert (make-string (* 2 depth) ?\ ))
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node))
        (node-text (format "%s:" (treesit-node-type node))))
    (if treesit-debug-jump-buttons
        (insert-button node-text
                       'action 'treesit-debug--button-node-lookup
                       'follow-link t
                       'points-to `(,start . ,end))
      (insert node-text)))
  (insert "\n"))

(defun treesit-debug--display-tree ()
  "Display the primary tree `treesit-tree'."
  (with-current-buffer treesit-debug--tree-buffer
    (with-current-buffer treesit-debug--source-code-buffer
      (setq treesit-debug--source-code-root (treesit-buffer-root-node)))
    (let* ((parsers (treesit-parser-list treesit-debug--source-code-buffer))
           (root (treesit-parser-root-node (car parsers)))
           buffer-read-only)
      (erase-buffer)
      (treesit-dfs-with-depth root #'treesit-debug--display-node))))

(defun treesit-debug--setup ()
  "Set up syntax tree debugging in the current buffer."
  (unless (buffer-live-p treesit-debug--tree-buffer)
    (setq treesit-debug--tree-buffer
          (get-buffer-create (format "*treesit-tree: %s*" (buffer-name)))))
  (let ((source-buffer (current-buffer)))
    (with-current-buffer treesit-debug--tree-buffer
      (buffer-disable-undo)
      (setq treesit-debug--source-code-buffer source-buffer
            buffer-read-only t)))
  (add-hook 'kill-buffer-hook #'treesit-debug--teardown nil :local)
  (add-hook 'after-save-hook #'treesit-debug--display-tree nil :local)

  (display-buffer treesit-debug--tree-buffer)
  (treesit-debug--display-tree))

(defun treesit-debug--teardown ()
  "Tear down syntax tree debugging in the current buffer."
  (remove-hook 'after-save-hook #'treesit-debug--display-tree :local)
  (when (buffer-live-p treesit-debug--tree-buffer)
    (kill-buffer treesit-debug--tree-buffer)
    (setq treesit-debug--tree-buffer nil)))

;;;###autoload
(define-minor-mode treesit-debug-mode
  "Toggle syntax tree debugging for the current buffer.
This mode displays the syntax tree in another buffer, and keeps it up-to-date."
  :init-value nil
  :group 'treesit
  (treesit-debug--setup))

(provide 'treesit-debug)
;;; treesit-debug.el ends here
