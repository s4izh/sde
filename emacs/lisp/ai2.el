(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)
;; you can utilize :map :hook and :config to customize copilot

(defun my/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))

(with-eval-after-load 'copilot
  (evil-define-key 'insert copilot-mode-map
    (kbd "<tab>") #'my/copilot-tab))

(add-hook 'prog-mode-hook 'copilot-mode)

(global-copilot-mode)

(dolist (mode '(org-mode-hook
                compilation-mode-hook
                debugger-mode-hook
                minibuffer-mode-hook
                vterm-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (copilot-mode 0))))

(use-package ellama
  :init
  (setopt ellama-language "english")
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "mistral:latest" :embedding-model "mistral:latest")))


(use-package gptel
  :config
  (setq gptel-default-mode "markdown-mode")
  (gptel-make-ollama
    "Ollama"                               ;Any name of your choosing
    :host "localhost:11434"                ;Where it's running
    :models '("mistral")            ;Installed models
    :stream t)                             ;Stream responses
  )
(setq-default gptel-model "mistral" ;Pick your default model
              gptel-backend (gptel-make-ollama "Ollama" :host "localhost:11434"))
