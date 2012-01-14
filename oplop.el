(require 'base64)
(require 'hex-util)
(require 'md5)


(defun oplop:subsequence-of-digits (str)
  (if (string-match "\\([0-9]+\\)" str)
      (match-string 0 str)
    nil))


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


(defun oplop:substring-with-digits (str nchars)
  (let* ((first-nchars (substring str 0 nchars))
         (some-digits (oplop:subsequence-of-digits first-nchars))
	 (string-with-digits
	  (if some-digits encoded
	    (concat (or (oplop:subsequence-of-digits encoded) "1") encoded))))
    (substring string-with-digits 0 nchars)))
  

(defun oplop:account-password (nickname master-password)
  ;; The steps it takes to generate an account password is:
  (let* ((master-password (encode-coding-string master-password 'utf-8))
         (nickname (encode-coding-string nickname 'utf-8))
         ;; Concatenate the master password with the nickname (in that
         ;; order!).
         (plain-text (concat master-password nickname))
         ;; Generate the MD5 hash of the concatenated string.
         (digest (decode-hex-string (md5 plain-text)))
         ;; Convert the MD5 hash to URL-safe Base64.
         (encoded (oplop:base64-encode-string-urlsafe digest)))
    ;; Return a subset of the hash containing digits
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
