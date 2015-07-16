(let ((snow (format "%s/../snow.el" (file-name-directory load-file-name))))

  (when (require 'undercover nil t)
    (undercover snow))

  (load-file snow)

  (ert-deftest test-snow ()
    ;; hooray -- everything loaded and nothing broke
    (should t)))

(provide 'snow-test)
;;; snow-test.el ends here
