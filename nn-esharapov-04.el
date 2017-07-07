;;; In setting PATH order is important
(setenv "PATH" (concat "C:\\PF\\Git\\usr\\bin;" (getenv "PATH")))
;;; Docker configuration
(setenv (or (getenv "DOCKER_HOST") "tcp://192.168.99.100:2376"))
(setenv (or (getenv "DOCKER_TLS_VERIFY") "1"))
(setenv (or (getenv "DOCKER_MACHINE_NAME") "default"))
(setenv (or (getenv "DOCKER_CERT_PATH") "C:\\Users\\esharapov\\.docker\\machine\\machines\\default"))

