(use-package cider
  :after (paredit eldoc)
  :ensure t
  :commands (cider-jack-in)
  :config
  (add-hook 'cider-mode-hook #'paredit-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode))

(use-package 4clojure
  :commands (4clojure-open-question)
  :after cider
  :ensure t
  :defer t
  :init (progn
	  (defadvice 4clojure-open-question (around 4clojure-open-question-around)
	    "Start a cider/nREPL connection if one hasn't already been started when
opening 4clojure questions"
	    ad-do-it
	    (unless cider-current-clojure-buffer
	      (cider-jack-in)))))

(provide 'ffe-clojure)
