(require 'base64)
(require 'hex-util)
(require 'md5)


(defun oplop:replace-char-in-string (chr rep str)
  (let ((replaced str))
    (while (string-match (concat "\\(" (regexp-quote chr) "\\)") replaced)
      (setq replaced (replace-match rep nil nil replaced)))
    replaced))


(defun oplop:base64-encode-string-urlsafe (str)
  (let* ((encoded (base64-encode-string str 't))
         (safer (oplop:replace-char-in-string "+" "-" encoded))
         (safest (oplop:replace-char-in-string "/" "_" safer)))
    safest))


(defun oplop:subsequence-of-digits (str)
  (if (string-match "\\([0-9]+\\)" str)
      (match-string 0 str)
    nil))


(defun oplop:substring-with-digits (str nchars)
  (let* ((first-nchars (substring str 0 nchars))
         (some-digits (oplop:subsequence-of-digits first-nchars))
	 (string-with-digits
	  (if some-digits encoded
	    (concat (or (oplop:subsequence-of-digits encoded) "1") encoded))))
    (substring string-with-digits 0 nchars)))
  

(defun oplop:account-password (nickname master-password)
  (let* ((encoding 'utf-8)
	 (master-password (encode-coding-string master-password encoding))
         (nickname (encode-coding-string nickname encoding))
         (plain-text (concat master-password nickname))
         (digest (decode-hex-string (md5 plain-text)))
         (encoded (oplop:base64-encode-string-urlsafe digest)))
    (oplop:substring-with-digits encoded 8)))


(defun oplop ()
  "Oplop is a password hashing algorithm. See
http://code.google.com/p/oplop/ for details. When invoked, this
interactive function prompts the user for their nickname and
master password, and then copies the account password to the
emacs kill-ring."
  (interactive "")
  (let ((nickname (read-string "nickname: "))
        (master-password (read-passwd "master password: ")))
    (kill-new (oplop:account-password nickname master-password))))


(provide 'oplop)
