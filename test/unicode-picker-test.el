;;; Test for `unicode-picker'

;;; Commentary:
;; These are the tests for `unicode-picker'

;;; Code:

(ert-deftest unicode-picker ()
  (with-temp-buffer
    (unicode-picker "card")
    (should (equal (buffer-string) "ℵℶℷℸ🂠🂡🂢🂣🂤🂥🂦🂧🂨🂩🂪🂫🂬🂭🂮🂱
🂲🂳🂴🂵🂶🂷🂸🂹🂺🂻🂼🂽🂾🂿🃁🃂🃃🃄🃅
🃆🃇🃈🃉🃊🃋🃌🃍🃎🃏🃑🃒🃓🃔🃕🃖🃗🃘🃙
🃚🃛🃜🃝🃞🃟🃠🃡🃢🃣🃤🃥🃦🃧🃨🃩🃪🃫🃬
🃭🃮🃯🃰🃱🃲🃳🃴🃵🎴💳📇🗂🗃")))
  (with-temp-buffer
    (unicode-picker "arrows")
    (should (equal (buffer-string) "⇇⇇⇈⇈⇉⇉⇊⇊⇶⬱⮄⮅⮆⮇⮔🔀🔁🔂🔃🔄
🗘")))
  )

(ert-deftest unicode-picker--get-unicode-detail-at-point ()
  (with-temp-buffer)
  (unicode-picker "card")
  (should (equal (unicode-picker--get-unicode-detail-at-point) '("FIRST TRANSFINITE CARDINAL" . 8501)))
  (right-char 1)
  (should (equal (unicode-picker--get-unicode-detail-at-point) '("SECOND TRANSFINITE CARDINAL" . 8502)))
  (forward-line)
  (should (equal (unicode-picker--get-unicode-detail-at-point) '("PLAYING CARD TWO OF HEARTS" . 127154)))
  )
;;
