(require 'org-publish)
(setq org-publish-project-alist
      '(

        ;; .. add all components here
        ("org-notes"
         :base-directory "~/Documents/projects/harmony/doc"
         :publishing-directory "~/Documents/projects/harmony/public"
         :base-extension "org"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4
         :auto-preamble t
         )

        ("org-static"
         :base-directory "~/Documents/projects/harmony/doc"
         :publishing-directory "~/Documents/projects/harmony/public"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("harmony" :components ("org-notes" "org-static"))

        ))