((when (require 'undercover nil t)
   (undercover "tiny.el"))

 require 'snow)

(ert-deftest test-snow ()
  ;; hooray -- everything loaded and nothing broke
  (should t))

(provide 'snow-test)
;;; snow-test.el ends here
