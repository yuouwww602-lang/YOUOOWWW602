;;; ============================================================
;;; GTOL_REBUILD_v9.lsp (GstarCAD/AutoCAD safe)
;;; Command: GTOL
;;; - NO vlax-safearray-p
;;; - NO MTEXT DXF group (7)
;;; - NO entmakex->vlax-ename->vla-object binding (fix "bind object: T")
;;; - BoundingBox uses VLA AddText object (always valid VLA object)
;;; ============================================================

(if (fboundp 'vl-load-com) (vl-load-com))

(setq *gtol_layer* "GTOL")
(setq *gtol_color* 3)

(defun gtol:ensure-layer (lname col)
  (if (not (tblsearch "LAYER" lname))
    (command "_.-LAYER" "_M" lname "_C" (itoa col) lname "")
  )
  lname
)

(defun gtol:pt3 (p) (list (car p) (cadr p) 0.0))
(defun gtol:dimscale () (if (> (getvar "DIMSCALE") 0.0) (getvar "DIMSCALE") 1.0))

(defun gtol:vadd (a b) (mapcar '+ a b))
(defun gtol:vscale (v s) (mapcar '(lambda (x) (* x s)) v))
(defun gtol:dot (a b) (+ (* (car a) (car b)) (* (cadr a) (cadr b)) (* (caddr a) (caddr b))))

(defun gtol:safe-get (obj prop / r)
  (setq r (vl-catch-all-apply 'vlax-get (list obj prop)))
  (if (vl-catch-all-error-p r) nil r)
)

(defun gtol:is-safearray (x) (= (type x) 'SAFEARRAY))

(defun gtol:anypoint->list (p / vv)
  (cond
    ((= (type p) 'LIST) p)
    ((= (type p) 'VARIANT)
      (setq vv (vlax-variant-value p))
      (cond
        ((= (type vv) 'LIST) vv)
        ((gtol:is-safearray vv) (vlax-safearray->list vv))
        (T nil)
      )
    )
    ((gtol:is-safearray p) (vlax-safearray->list p))
    (T nil)
  )
)

;; --------- COM helpers (VLA) ----------
;; COM required
(defun gtol:doc () (vla-get-ActiveDocument (vlax-get-acad-object)))
;; COM required
(defun gtol:ms  () (vla-get-ModelSpace (gtol:doc)))
;; COM required
(defun gtol:3dpt (p) (vlax-3d-point (gtol:pt3 p)))

(defun gtol:com-available ()
  (and (fboundp 'vl-load-com)
       (fboundp 'vlax-get-acad-object)
       (fboundp 'vlax-ename->vla-object))
)

(defun gtol:objectp (obj)
  (if (fboundp 'vlax-objectp)
    (vlax-objectp obj)
    (and obj (= (type obj) 'VLA-OBJECT))
  )
)

;; ✅ 임시 TEXT를 VLA로 생성 (항상 VLA object 반환) -> BoundingBox 안정
;; COM required
(defun gtol:vla-temp-text (txt h rot sty / oldsty o)
  (setq oldsty (getvar "TEXTSTYLE"))
  (if (and sty (/= sty "")) (setvar "TEXTSTYLE" sty))

  (if (gtol:com-available)
    (progn
      (setq o (vl-catch-all-apply 'vla-AddText (list (gtol:ms) txt (gtol:3dpt '(0 0 0)) h)))
      (if (vl-catch-all-error-p o)
        (setq o nil)
        (vla-put-Rotation o rot)
      )
    )
    (setq o nil)
  )

  (setvar "TEXTSTYLE" oldsty)
  o
)

;; -------- DIM info ----------
;; COM required
(defun gtol:dim-measure (ent / o m)
  (setq o (vlax-ename->vla-object ent))
  (setq m (gtol:safe-get o 'Measurement))
  (if (and m (numberp m)) m nil)
)

(defun gtol:dimdec () (getvar "DIMDEC"))
(defun gtol:format-measure (m) (rtos m 2 (gtol:dimdec)))

(defun gtol:dim-displayed-text (ent ed / over m s)
  (setq over (cdr (assoc 1 ed)))
  (setq m (if (gtol:com-available)
            (gtol:dim-measure ent)
            (gtol:dim-measure-entget ed)
          )
  )
  (cond
    ((and over (/= over ""))
      (setq s over)
      (if (and m (vl-string-search "<>" s))
        (setq s (vl-string-subst (gtol:format-measure m) "<>" s))
      )
      s
    )
    (m (gtol:format-measure m))
    (T "0")
  )
)

;; COM required
(defun gtol:get-dim-textheight (ent / o th)
  (setq o (vlax-ename->vla-object ent))
  (setq th (gtol:safe-get o 'TextHeight))
  (cond
    ((and th (numberp th) (> th 0.0)) th)
    (T (* (getvar "DIMTXT") (gtol:dimscale)))
  )
)

;; COM required
(defun gtol:get-dim-textrotation (ent ed / o r)
  (setq o (vlax-ename->vla-object ent))
  (setq r (gtol:safe-get o 'TextRotation))
  (cond
    ((and r (numberp r)) r)
    ((cdr (assoc 50 ed)))
    (T 0.0)
  )
)

;; COM required
(defun gtol:get-dim-textstyle (ent / o s)
  (setq o (vlax-ename->vla-object ent))
  (setq s (gtol:safe-get o 'TextStyleName))
  (cond
    ((and s (= (type s) 'STR) (/= s "")) s)
    (T (getvar "DIMTXSTY"))
  )
)

;; COM required
(defun gtol:get-dim-textpos (ent ed / o tp lst)
  (setq o (vlax-ename->vla-object ent))
  (setq tp (gtol:safe-get o 'TextPosition))
  (setq lst (gtol:anypoint->list tp))
  (if (and lst (>= (length lst) 2))
    (gtol:pt3 lst)
    (gtol:pt3 (or (cdr (assoc 11 ed)) (cdr (assoc 10 ed)) '(0.0 0.0 0.0)))
  )
)

(defun gtol:clean-dimtext (s)
  (if (null s) ""
    (progn
      (setq s (vl-string-translate "{" "" s))
      (setq s (vl-string-translate "}" "" s))
      s
    )
  )
)

(defun gtol:safe-string (s)
  (if (= (type s) 'STR) s "")
)

;; -------- entget fallback helpers (COM not required) ----------
(defun gtol:dim-measure-entget (ed / m)
  (setq m (cdr (assoc 42 ed)))
  (if (and m (numberp m)) m nil)
)

(defun gtol:get-dim-textheight-entget ()
  (* (getvar "DIMTXT") (gtol:dimscale))
)

(defun gtol:get-dim-textrotation-entget (ed)
  (cond
    ((cdr (assoc 50 ed)))
    (T 0.0)
  )
)

(defun gtol:get-dim-textstyle-entget ()
  (getvar "DIMTXSTY")
)

(defun gtol:get-dim-textpos-entget (ed)
  (gtol:pt3 (or (cdr (assoc 11 ed)) (cdr (assoc 10 ed)) '(0.0 0.0 0.0)))
)

;; -------- placement axis ("visual right") ----------
(defun gtol:rightvec (rot / u v)
  (setq u (list (cos rot) (sin rot) 0.0))
  (setq v (list (- (sin rot)) (cos rot) 0.0))
  (if (> (abs (sin rot)) (abs (cos rot)))
    (gtol:vscale v -1.0)
    u
  )
)

;; ✅ bbox halfextent by projecting bounding box corners onto axis
;; COM required
(defun gtol:text-halfextent-along (txt h rot axis sty / o mn mx mnL mxL projMin projMax vv)
  (setq o (gtol:vla-temp-text txt h rot sty))

  (if (not (and o (gtol:objectp o)))
    0.0
    (progn
      (setq mn (vlax-make-variant (vlax-make-safearray vlax-vbDouble '(0 . 2))))
      (setq mx (vlax-make-variant (vlax-make-safearray vlax-vbDouble '(0 . 2))))
      (vl-catch-all-apply 'vla-getBoundingBox (list o 'mn 'mx))

      (setq mnL (gtol:anypoint->list mn))
      (setq mxL (gtol:anypoint->list mx))

      ;; delete temp text safely
      (vl-catch-all-apply 'vla-Delete (list o))

      (setq projMin 1e99 projMax -1e99)
      (foreach p
        (list
          (list (nth 0 mnL) (nth 1 mnL) 0.0)
          (list (nth 0 mnL) (nth 1 mxL) 0.0)
          (list (nth 0 mxL) (nth 1 mnL) 0.0)
          (list (nth 0 mxL) (nth 1 mxL) 0.0)
        )
        (setq vv (gtol:dot p axis))
        (if (< vv projMin) (setq projMin vv))
        (if (> vv projMax) (setq projMax vv))
      )
      (/ (- projMax projMin) 2.0)
    )
  )
)

;; -------- formatting helpers ----------
(defun gtol:split (str delim / pos out dlen)
  (setq out '())
  (setq dlen (strlen delim))
  (while (setq pos (vl-string-search delim str))
    (setq out (cons (substr str 1 pos) out))
    (setq str (substr str (+ pos dlen 1)))
  )
  (reverse (cons str out))
)

(defun gtol:normalize-plusminus (s)
  (setq s (gtol:safe-string s))
  (setq s (vl-string-translate " " "" s))
  (if (wcmatch s "+-*") (strcat "±" (substr s 3)) s)
)

(defun gtol:norm-line (s / t)
  (setq t (vl-string-trim " " s))
  (cond
    ((= t "0") "0")
    ((= t "+0") "+0")
    ((or (= (substr t 1 1) "+") (= (substr t 1 1) "-")) t)
    (T (strcat "+" t))
  )
)

(defun gtol:norm-pair (up dn / u d)
  (setq u (gtol:norm-line up))
  (setq d (gtol:norm-line dn))
  (if (and (= u "0") (= (substr d 1 1) "-")) (setq u "+0"))
  (list u d)
)

(defun gtol:indent-secondline (up dn tolH sty / plusW)
  (setq up (gtol:safe-string up))
  (setq dn (gtol:safe-string dn))
  (if (and (/= up "")
           (= (substr up 1 1) "+")
           (wcmatch (vl-string-translate " " "" dn) "0,0.*,+0,+0.*"))
    (progn
      (setq plusW (* 2.0 (gtol:text-halfextent-along "+" tolH 0.0 '(1 0 0) sty)))
      (list up (strcat "{\\pxi" (rtos plusW 2 4) ";" dn "}"))
    )
    (list up dn)
  )
)

;; ISO IT width (simplified width)
(defun gtol:it (D grade / i um mult)
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

(defun gtol:parse-fit (s / ss letter grade)
  (setq ss (vl-string-trim " " s))
  (if (>= (strlen ss) 2)
    (progn
      (setq letter (substr ss 1 1))
      (setq grade (atoi (substr ss 2)))
      (if (> grade 0) (list letter grade) nil)
    )
    nil
  )
)

(defun gtol:ask-hole-shaft (/ k)
  (initget "H S")
  (setq k (getkword "\n공차 종류 선택 [H=구멍공차/S=축공차] <H>: "))
  (if (or (null k) (= k "")) "H" k)
)

;; ✅ MTEXT 생성: DXF (7) 안 넣음. TEXTSTYLE만 잠깐 바꿔서 폰트 일치
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

;; ===================== MAIN =====================
(defun c:GTOL ( / layer col sel ent ed tpt rot axis dimH tolH sty showTxt cleanTxt halfExt gap ins
                 dia spec fit letter grade IT hs up dn pair lines txt done )

  (if (not (fboundp 'vl-load-com))
    (prompt "\nAutoCAD LT에서는 COM 함수 미지원. 제한 모드로 실행됩니다.")
    (vl-load-com)
  )
  (if (not (gtol:com-available))
    (prompt "\nCOM 사용 불가: entget 기반 경로로 진행합니다.")
  )

  (setq done nil)
  (setq layer (gtol:ensure-layer *gtol_layer* *gtol_color*))
  (setq col *gtol_color*)

  (setq sel (entsel "\n공차를 넣을 치수(DIMENSION)를 클릭하세요: "))
  (setq ent (car sel))
  (if (null ent) (progn (princ "\n취소됨.") (setq done T)))

  (if (not done)
    (progn
      (setq ed (entget ent))
      (if (not (= (cdr (assoc 0 ed)) "DIMENSION"))
        (progn (princ "\nDIMENSION만 선택 가능합니다.") (setq done T))
      )
    )
  )

  (if (not done)
    (progn
      (setq showTxt (gtol:dim-displayed-text ent ed))
      (setq cleanTxt (gtol:clean-dimtext showTxt))

      (if (gtol:com-available)
        (progn
          (setq dimH (gtol:get-dim-textheight ent))
          (setq rot  (gtol:get-dim-textrotation ent ed))
          (setq sty  (gtol:get-dim-textstyle ent))
          (setq tpt (gtol:get-dim-textpos ent ed))
          (setq halfExt (gtol:text-halfextent-along cleanTxt dimH rot (gtol:rightvec rot) sty))
          (setq dia (gtol:dim-measure ent))
        )
        (progn
          (setq dimH (gtol:get-dim-textheight-entget))
          (setq rot  (gtol:get-dim-textrotation-entget ed))
          (setq sty  (gtol:get-dim-textstyle-entget))
          (setq tpt (gtol:get-dim-textpos-entget ed))
          (setq halfExt 0.0)
          (setq dia (gtol:dim-measure-entget ed))
        )
      )

      (setq tolH (* dimH 0.60))
      (setq axis (gtol:rightvec rot))

      (setq gap (* dimH 0.22))
      (setq ins (gtol:vadd tpt (gtol:vscale axis (+ halfExt gap))))

      (if (or (null dia) (<= dia 0.0)) (setq dia 20.0))

      (setq spec (getstring T "\n공차 입력 (예: H7 / h7 / Js7 / ±0.1 / 0,-0.025 / +0.025,0 / 17.701/17.680): "))
      (setq spec (vl-string-trim " " (gtol:safe-string spec)))
      (if (= spec "") (progn (princ "\n취소됨.") (setq done T)))
    )
  )

  (if (not done)
    (progn
      ;; 1) limit up/dn by "/"
      (if (vl-string-search "/" spec)
        (progn
          (setq lines (gtol:split spec "/"))
          (if (>= (length lines) 2)
            (progn
              (setq up (vl-string-trim " " (car lines)))
              (setq dn (vl-string-trim " " (cadr lines)))
              (setq pair (gtol:norm-pair up dn))
              (setq pair (gtol:indent-secondline (car pair) (cadr pair) tolH sty))
              (setq txt (strcat (car pair) "\\P" (cadr pair)))
              (gtol:mk-mtext ins tolH txt layer col rot sty)
              (princ "\n공차 입력 완료.")
              (setq done T)
            )
          )
        )
      )

      ;; 2) ISO fit (H/h/Js; else ask)
      (if (not done)
        (progn
          (setq fit (gtol:parse-fit spec))
          (if fit
            (progn
              (setq letter (car fit))
              (setq grade (cadr fit))
              (setq IT (gtol:it dia grade))

              (cond
                ((= letter "H") (setq up (strcat "+" (rtos IT 2 3)) dn "0"))
                ((= letter "h") (setq up "0" dn (strcat "-" (rtos IT 2 3))))
                ((or (= letter "J") (= letter "j"))
                  (setq up (strcat "+" (rtos (/ IT 2.0) 2 3))
                        dn (strcat "-" (rtos (/ IT 2.0) 2 3))))
                (T
                  (setq hs (gtol:ask-hole-shaft))
                  (if (= hs "H")
                    (setq up (strcat "+" (rtos IT 2 3)) dn "0")
                    (setq up "0" dn (strcat "-" (rtos IT 2 3))))
                )
              )

              (setq pair (gtol:norm-pair up dn))
              (setq pair (gtol:indent-secondline (car pair) (cadr pair) tolH sty))
              (setq txt (strcat (car pair) "\\P" (cadr pair)))
              (gtol:mk-mtext ins tolH txt layer col rot sty)
              (princ "\n공차 입력 완료.")
              (setq done T)
            )
          )
        )
      )

      ;; 3) ±
      (if (not done)
        (progn
          (setq spec (gtol:normalize-plusminus spec))
          (if (wcmatch spec "±*")
            (progn
              (gtol:mk-mtext ins tolH spec layer col rot sty)
              (princ "\n공차 입력 완료.")
              (setq done T)
            )
          )
        )
      )

      ;; 4) stacked by "," or space
      (if (not done)
        (progn
          (setq lines nil)
          (if (vl-string-search "," spec)
            (setq lines (gtol:split spec ","))
            (if (vl-string-search " " spec)
              (setq lines (gtol:split spec " "))
            )
          )
          (if (and lines (>= (length lines) 2))
            (progn
              (setq up (vl-string-trim " " (car lines)))
              (setq dn (vl-string-trim " " (cadr lines)))
              (setq pair (gtol:norm-pair up dn))
              (setq pair (gtol:indent-secondline (car pair) (cadr pair) tolH sty))
              (setq txt (strcat (car pair) "\\P" (cadr pair)))
              (gtol:mk-mtext ins tolH txt layer col rot sty)
              (princ "\n공차 입력 완료.")
              (setq done T)
            )
          )
        )
      )

      ;; fallback
      (if (not done)
        (progn
          (gtol:mk-mtext ins tolH spec layer col rot sty)
          (princ "\n공차 입력 완료.")
          (setq done T)
        )
      )
    )
  )

  (princ)
)

(princ "\n[GTOL_REBUILD_v9] Loaded. Command: GTOL")
(princ)
