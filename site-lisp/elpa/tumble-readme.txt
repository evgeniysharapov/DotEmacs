;;; Commentary:

;; Tumble is a mode for interacting with Tumblr inside Emacs. It currently
;; provides the following functions:
;;
;; tumble-text-from-region 
;;     Posts the selected region as a "Text".
;; tumble-text-from-buffer 
;;     Posts the current buffer as a "Text".

;; tumble-quote-from-region 
;;     Posts the current region as a "Quote". Prompts
;;     for an optional "source" parameter.

;; tumble-link 
;;     Prompts for a title and a URL for a new "Link".
;; tumble-link-with-description 
;;     Prompts for a title and a URL for a new "Link" and
;;     uses the selected region as the link's description.

;; tumble-chat-from-region 
;;     Posts the selected region as a "Chat".
;; tumble-chat-from-buffer 
;;     Posts the current buffer as a "Chat".

;; tumble-photo-from-url 
;;     Prompts for a file URL, a caption and a clickthrough and
;;     posts the result as a "Photo".
;; tumble-photo-from-file 
;;     Prompts for a local file, a caption and a clickthrough and
;;     posts the result as a Photo.
;; tumble-audio 
;;     Prompts for a local file and an optional caption to 
;;     upload a MP3 file.

;; tumble-video-from-url 
;;     Prompts for an embed code and an optional caption to post a video
;;     to Tumblr.

;; A word of caution: Audio files can take a while to upload and will 
;; probably freeze your Emacs until it finishes uploading.

;; You can always find the latest version of Tumble at: http://github.com/febuiles/tumble

;; Installation: 

;;
;; Download Tumble to some directory:
;; $ git clone git://github.com/febuiles/tumble.git
;;
;; Add it to your load list and require it:
;;
;; (add-to-list 'load-path "~/some_directory/tumble.el")
;; (require 'tumble)
;;
;; Open tumble.el (this file) and modify the following variables:
;; (setq tumble-email "your_email@something.com")
;; (setq tumble-password "your_password")
;; (setq tumble-url "your_tumblelog.tumblr.com")
;; 
;; Tumble uses no group for posting and Markdown as the default 
;; format but you can change these:
;; (setq tumble-group "your_group.tumblr.com")
;; (setq tumble-format "html")

