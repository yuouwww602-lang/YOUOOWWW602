;;; ============================================================
;;; GTOL_REBUILD.lsp (AutoCAD/Visual LISP compatible)
;;; Command: GTOL
;;; - No Express Tools
;;; - Safe for LT (COM unavailable -> fallback)
;;; - Avoids DXF group (7)
;;; ============================================================

(setq *GTOL_DEBUG* nil)
(setq *GTOL_LAYER* "GTOL")
(setq *GTOL_COLOR* 3)

(defun gtol:debug (msg)
  (if *GTOL_DEBUG* (prompt (strcat "\n[GTOL] " msg)))
)

(defun gtol:enamep (e) (and e (= (type e) 'ENAME)))

(defun gtol:com-available ()
  (and (fboundp 'vl-load-com)
       (fboundp 'vlax-get-acad-object)
       (fboundp 'vlax-ename->vla-object)
       (fboundp 'vlax-objectp))
)

(defun gtol:objectp (obj)
  (if (fboundp 'vlax-objectp)
    (vlax-objectp obj)
    (and obj (= (type obj) 'VLA-OBJECT))
  )
)

(defun gtol:ensure-layer (lname col)
  (if (not (tblsearch "LAYER" lname))
    (command "_.-LAYER" "_M" lname "_C" (itoa col) lname "")
  )
  lname
)

(defun gtol:pt3 (p) (list (car p) (cadr p) 0.0))
(defun gtol:vadd (a b) (mapcar '+ a b))
(defun gtol:vscale (v s) (mapcar '(lambda (x) (* x s)) v))
(defun gtol:rightvec (rot / u v)
  (setq u (list (cos rot) (sin rot) 0.0))
  (setq v (list (- (sin rot)) (cos rot) 0.0))
  (if (> (abs (sin rot)) (abs (cos rot))) (gtol:vscale v -1.0) u)
)

(defun gtol:safe-string (s) (if (= (type s) 'STR) s ""))

(defun gtol:dimscale () (if (> (getvar "DIMSCALE") 0.0) (getvar "DIMSCALE") 1.0))

(defun gtol:dimdec () (getvar "DIMDEC"))

(defun gtol:format-measure (m) (rtos m 2 (gtol:dimdec)))

(defun gtol:dim-displayed-text (ed / over meas s)
  (setq over (cdr (assoc 1 ed)))
  (setq meas (cdr (assoc 42 ed)))
  (cond
    ((and over (/= over ""))
      (setq s over)
      (if (and meas (vl-string-search "<>" s))
        (setq s (vl-string-subst (gtol:format-measure meas) "<>" s))
      )
      s
    )
    (meas (gtol:format-measure meas))
    (T "0")
  )
)

;; ---------- Text/Style (entget fallback only) ----------
(defun gtol:get-dim-textheight-entget ()
  (* (getvar "DIMTXT") (gtol:dimscale))
)

(defun gtol:get-dim-textrotation-entget (ed)
  (cond ((cdr (assoc 50 ed))) (T 0.0))
)

(defun gtol:get-dim-textstyle-entget () (getvar "DIMTXSTY"))

(defun gtol:get-dim-textpos-entget (ed)
  (gtol:pt3 (or (cdr (assoc 11 ed)) (cdr (assoc 10 ed)) '(0.0 0.0 0.0)))
)

;; ---------- COM path (annotated) ----------
;; COM required
(defun gtol:get-dim-textheight-com (ent / o th)
  (setq o (vlax-ename->vla-object ent))
  (setq th (vlax-get o 'TextHeight))
  (if (and th (numberp th) (> th 0.0)) th (gtol:get-dim-textheight-entget))
)

;; COM required
(defun gtol:get-dim-textrotation-com (ent ed / o r)
  (setq o (vlax-ename->vla-object ent))
  (setq r (vlax-get o 'TextRotation))
  (if (and r (numberp r)) r (gtol:get-dim-textrotation-entget ed))
)

;; COM required
(defun gtol:get-dim-textstyle-com (ent)
  (setq o (vlax-ename->vla-object ent))
  (setq s (vlax-get o 'TextStyleName))
  (if (and s (= (type s) 'STR) (/= s "")) s (gtol:get-dim-textstyle-entget))
)

;; COM required
(defun gtol:get-dim-textpos-com (ent ed)
  (setq o (vlax-ename->vla-object ent))
  (setq tp (vlax-get o 'TextPosition))
  (if (and tp (= (type tp) 'LIST))
    (gtol:pt3 tp)
    (gtol:get-dim-textpos-entget ed)
  )
)

(defun gtol:calc-tol-textheight (dimValue dimTextHeight)
  ;; Reference rule (tunable): smaller dimensions get proportionally smaller tolerance text.
  ;; - dimValue <= 25: 0.55x dimTextHeight
  ;; - 25 < dimValue <= 100: 0.60x dimTextHeight
  ;; - > 100: 0.65x dimTextHeight
  (cond
    ((<= dimValue 25.0) (* dimTextHeight 0.55))
    ((<= dimValue 100.0) (* dimTextHeight 0.60))
    (T (* dimTextHeight 0.65))
  )
)

(defun gtol:mk-mtext (ins h txt layer col rot sty / oldsty e)
  (setq oldsty (getvar "TEXTSTYLE"))
  (if (and sty (/= sty "")) (setvar "TEXTSTYLE" sty))
  (setq e
    (entmakex
      (list
        '(0 . "MTEXT")
        (cons 8 layer)
        (cons 62 col)
        (cons 10 (gtol:pt3 ins))
        (cons 40 h)
        (cons 50 rot)
        (cons 71 4)
        (cons 1 txt)
      )
    )
  )
  (setvar "TEXTSTYLE" oldsty)
  e
)

(defun gtol:parse-fit (s / ss letter grade)
  (setq ss (vl-string-trim " " (gtol:safe-string s)))
  (if (>= (strlen ss) 2)
    (progn
      (setq letter (substr ss 1 1))
      (setq grade (atoi (substr ss 2)))
      (if (> grade 0) (list letter grade) nil)
    )
    nil
  )
)

(defun gtol:it (D grade / i mult)
  (setq i (+ (* 0.45 (expt D (/ 1.0 3.0))) (* 0.001 D)))
  (setq mult
    (cond
      ((= grade 5) 7)  ((= grade 6) 10) ((= grade 7) 16)
      ((= grade 8) 25) ((= grade 9) 40) ((= grade 10) 64)
      ((= grade 11) 100)
      (T 16)
    )
  )
  (/ (* mult i) 1000.0)
)

(defun gtol:ask-hole-shaft (/ k)
  (initget "H S")
  (setq k (getkword "\n공차 종류 선택 [H=구멍공차/S=축공차] <H>: "))
  (if (or (null k) (= k "")) "H" k)
)

(defun gtol:make-tolerance-text (spec dia)
  ;; Returns final MTEXT content.
  (setq spec (vl-string-trim " " (gtol:safe-string spec)))
  (setq fit (gtol:parse-fit spec))
  (if fit
    (progn
      (setq letter (car fit))
      (setq grade (cadr fit))
      (setq it (gtol:it dia grade))
      (setq hs (gtol:ask-hole-shaft))
      (cond
        ((= hs "H") (setq up (strcat "+" (rtos it 2 3)) dn "0"))
        (T (setq up "0" dn (strcat "-" (rtos it 2 3))))
      )
      (strcat up "\\P" dn)
    )
    spec
  )
)

(defun gtol:select-dimension ( / sel ent ed)
  (setq sel nil)
  (while (not sel)
    (setq sel (entsel "\n공차를 넣을 치수(DIMENSION)를 클릭하세요: "))
    (setq ent (car sel))
    (if (not (gtol:enamep ent))
      (progn (prompt "\n선택 실패. 다시 선택하세요.") (setq sel nil))
      (progn
        (setq ed (entget ent))
        (if (not (= (cdr (assoc 0 ed)) "DIMENSION"))
          (progn (prompt "\nDIMENSION만 선택 가능합니다.") (setq sel nil))
        )
      )
    )
  )
  (list ent ed)
)

(defun c:GTOL ( / ent ed dimText dimH rot sty tpt dia tolH axis gap ins spec txt comok sel)
  (if (fboundp 'vl-load-com)
    (vl-load-com)
    (prompt "\nAutoCAD LT에서는 COM 함수 미지원. 제한 모드로 실행됩니다.")
  )

  (setq comok (gtol:com-available))
  (if (not comok)
    (prompt "\nCOM 사용 불가: entget 기반 경로로 진행합니다.")
  )

  (gtol:ensure-layer *GTOL_LAYER* *GTOL_COLOR*)

  (setq sel (gtol:select-dimension))
  (setq ent (car sel))
  (setq ed (cadr sel))

  (setq dimText (gtol:dim-displayed-text ed))
  (if comok
    (progn
      (setq dimH (gtol:get-dim-textheight-com ent))
      (setq rot  (gtol:get-dim-textrotation-com ent ed))
      (setq sty  (gtol:get-dim-textstyle-com ent))
      (setq tpt  (gtol:get-dim-textpos-com ent ed))
      (setq dia  (cdr (assoc 42 ed)))
    )
    (progn
      (setq dimH (gtol:get-dim-textheight-entget))
      (setq rot  (gtol:get-dim-textrotation-entget ed))
      (setq sty  (gtol:get-dim-textstyle-entget))
      (setq tpt  (gtol:get-dim-textpos-entget ed))
      (setq dia  (cdr (assoc 42 ed)))
    )
  )

  (if (or (null dia) (not (numberp dia)) (<= dia 0.0)) (setq dia 20.0))

  (setq tolH (gtol:calc-tol-textheight dia dimH))
  (setq axis (gtol:rightvec rot))
  (setq gap (* dimH 0.22))
  (setq ins (gtol:vadd tpt (gtol:vscale axis gap)))

  (setq spec (getstring T "\n공차 입력 (예: h7 / H7 / ±0.01 / +0.02/-0.00): "))
  (setq spec (vl-string-trim " " (gtol:safe-string spec)))
  (if (= spec "")
    (progn (prompt "\n취소됨.") (princ))
    (progn
      (setq txt (gtol:make-tolerance-text spec dia))

      (gtol:debug (strcat "ent=" (if ent "OK" "NIL")))
      (gtol:debug (strcat "dimText=" dimText))
      (gtol:debug (strcat "dimH=" (rtos dimH 2 3)))
      (gtol:debug (strcat "tolH=" (rtos tolH 2 3)))
      (gtol:debug (strcat "txt=" txt))

      (gtol:mk-mtext ins tolH txt *GTOL_LAYER* *GTOL_COLOR* rot sty)
      (prompt "\n공차 입력 완료.")
      (princ)
    )
  )

  (princ)
)

(prompt "\n[GTOL_REBUILD] Loaded. Command: GTOL")
(princ)
