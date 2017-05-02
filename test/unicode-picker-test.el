;;; Test for `unicode-picker'

;;; Commentary:
;; These are the tests for `unicode-picker'

;;; Code:

(ert-deftest unicode-picker ()
  (with-temp-buffer
    (unicode-picker "card")
    (should (equal (buffer-string) "ℵℶℷℸ🂠🂡🂢🂣🂤🂥🂦🂧🂨🂩🂪🂫🂬🂭🂮🂱
🂲🂳🂴🂵🂶🂷🂸🂹🂺🂻🂼🂽🂾🂿🃁🃂🃃🃄🃅🃆
🃇🃈🃉🃊🃋🃌🃍🃎🃏🃑🃒🃓🃔🃕🃖🃗🃘🃙🃚🃛
🃜🃝🃞🃟🃠🃡🃢🃣🃤🃥🃦🃧🃨🃩🃪🃫🃬🃭🃮🃯
🃰🃱🃲🃳🃴🃵🎴💳📇🗂🗃")))
  (with-temp-buffer
    (unicode-picker "arrows")
    (should (equal (buffer-string) "⇇⇇⇈⇈⇉⇉⇊⇊⇶⬱⮄⮅⮆⮇⮔🔀🔁🔂🔃🔄
🗘")))
  )

;;
