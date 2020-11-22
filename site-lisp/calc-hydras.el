(require 'pretty-hydra)

(defun calc-store-show (&optional var)
  (interactive)
  (or var (setq var (calc-read-var-name "Store: " t)))
  (calc-store-into var)
  (calc-push-list `((calcFunc-evalto (var ,(intern (calc-var-name var)) ,var)
                                     ,(calc-var-value var)))))

(defun calc-show-all-vars ()
  (interactive)
  (let (cvars)
    (mapatoms (function
	       (lambda (x)
		 (and (string-match "\\`var-" (symbol-name x))
		      (not (memq x calc-dont-insert-variables))
		      (calc-var-value x)
		      (not (eq (car-safe (symbol-value x)) 'special-const))
		      (or (not (eq x 'var-Decls))
			  (not (equal var-Decls '(vec))))
		      (or (not (eq x 'var-Holidays))
			  (not (equal var-Holidays '(vec (var sat var-sat)
							 (var sun var-sun)))))
                      (push
                       `(calcFunc-evalto (var ,(intern (calc-var-name x)) ,x)
                                         ,(calc-var-value x))
                       
                       cvars)))))
    (calc-push-list cvars)))

(defvar calc-function-hydra-topline
  "
Calc: ~i(INV,^)~h( HYP,^) %`calc-angle-mode [_md_ _mr_] [,] _dg_roup %`calc-float-format [_df_ix _ds_ci _dn_orm] _K_eep args  _SPC_ more _q_uit
")

(defvar calc-function-hydra-topline-heads
  '(("md" calc-degrees-mode)
    ("K" calc-keep-args)
    ("mr" calc-radians-mode)
    ("dg" calc-group-digits)
    ("ds" calc-sci-notation)
    ("df" calc-fix-notation)
    ("dn" calc-normal-notation)
    ("dl" calc-line-numbering)
    ("td" calc-trail-display)
    ("I" calc-inverse)
    ("H" calc-hyperbolic)
    ("q" nil :exit t)))

(defun calc-function-hydra-docstring (str)
  (let (match
        (face-rx (rx "~F(" (group (+? any)) "," (* blank)
                     (group (*? any))
                     ")")))

    (while (setq match (string-match face-rx str match))
      (let ((ms (match-beginning 0))
            (me (match-end 0))
            (mv (match-string 1 str))
            (mf (match-string 2 str)))
        (setq str (concat
                   (substring str 0 ms)
                   (propertize mv 'face (intern mf))
                   (substring str me )
                   ))
        
        )
      )

    (replace-regexp-in-string
     (rx "~i(" (group (+? any))  "," (* blank) (group (* (not (any ")")))) ")")
     "%s(if calc-inverse-flag \"\\1\" \"\\2\")"
     (replace-regexp-in-string
      (rx "~h(" (group (+? any))  "," (* blank) (group (* (not (any ")")))) ")")
      "%s(if calc-hyperbolic-flag \"\\1\" \"\\2\")"
      
      (replace-regexp-in-string
       (rx bos "\n")
       calc-function-hydra-topline
       str)))))

(defun hydra-add-style-properties (hint)
  (let (match)
     (while (setq match (string-match
                         (rx (group (any "*~/"))
                             word
                             (? (| (*? nonl) word))
                             (backref 1))
                         hint
                         match))
       (let ((style (match-string 1 hint)))
         (aset hint (match-beginning 0) ? )
         (aset hint (- (match-end 0) 1) ? )
         (add-face-text-property
          (+ (match-beginning 0) 1)
          (- (match-end 0) 1)
          (cond
           ((string= style "*") 'bold)
           ((string= style "/") 'italic)
           ((string= style "~") 'underline)
           )
          t hint)))
     hint))

(defmacro hydra-style-hint (hydra-name)
  (let ((hydra-hint (intern (format "%s/hint" hydra-name))))
    `(setq ,hydra-hint
           (list 'hydra-add-style-properties ,hydra-hint))))

(progn (eval
        `(defhydra calc-function-hydra
           (:quit-key "q"
                      :base-map (make-sparse-keymap)
                      :foreign-keys run
                      :pre (setq which-key-inhibit t)
                      :hint none
                      :post (setq which-key-inhibit nil))
           
           ,(calc-function-hydra-docstring
             "
 *Basic* ^^^^^^^^^^^^^^   *Variables* ^^  *Trig*  ^^^^^         *Vectors*          ^^^^^^^          *Algebra*
ᵢ ^^_Q_ ~i(2ˣ  ,sqrt)    _ss_ store =>   ᵢₕ _C_os      _vx_ [1..x] ^^ _vt_ transpose ^^^^^   _as_implify   _aF_ fit curve
ᵢ _\\^_ ~i(ˣ√y,yˣ )  ^   _sS_ store      ᵢₕ _S_in      _vb_ [x,x,] ^^ _VH_ histogram ^^^^^   _an_ormalize 
  ^^_&_ 1/x  ^^^^^^^^^   _st_ store pop  ᵢₕ _T_an      _|_  concat ^^ _VM_ map       ^^^^^   _ad_erivative 
ₕ ^^_L_ log~h(₁₀,ₑ )^^   _sr_ recall    *Units*    ^^  _vu_n/_vp_ack  _VR_ reduce    ^^^^^   _ai_ntegral / _aI_ approx
ₕ ^^_E_ ~h(10,e )ˣ  ^^   _su_ clear      _uv_iew       _vl_ len  ^^   _u+_ sum       ^^^^^   _aS_olve 1  / _aP_ all  / _aR_oot
  ^^_B_ logₓ(y) ^^^^^^^^^_se_ edit       _uc_onv       _vv_ reV  ^^   _u*_ prod      ^^^^^   _aN_ min    / _aX_ max
  ^^_=_/_N_ eval ^^^^^^^ _sa_ show       _us_simpl     _VS_ sort ^^ ₕ _uM_ ~H(median,mean  )
")

           ,@calc-function-hydra-topline-heads
           
           ("Q" calc-sqrt)
           ("^" calc-power)
           ("&" calc-inv)
           
           ("L" calc-ln)
           ("E" calc-exp)
           ("B" calc-log)

           ("ss" calc-store-show )
           ("sS" calc-store )
           ("st" calc-store-into )
           ("sr" calc-recall )
           ("su" calc-unstore )
           ("se" calc-edit-variable)
           ("sa" calc-show-all-vars)

           ("C" calc-cos)
           ("S" calc-sin)
           ("T" calc-tan)
           

           ("vx" calc-index "range")
           ("vb" calc-build-vector "ncopies")
           ("|" calc-concat "concat")
           ("vu" calc-unpack "to stack")
           ("vp" calc-pack "to stack")
           ("vl" calc-vlength "length")
           ("vv" calc-reverse-vector "reverse")
           ("VS" calc-sort "sort")
           ("VH" calc-histogram "histogram")
           ("vt" calc-transpose "transpose")

           ("VM" calc-map)
           ("VR" calc-reduce)
           
           ("u+" calc-vector-sum "ditto")
           ("u*" calc-vector-product "ditto")

           ("uc" calc-convert-units "convert")
           ("ub" calc-convert-units "-> base")
           ("us" calc-simplify-units "simplify")
           ("uv" calc-enter-units-table "view table")
           
           ("uM" calc-vector-mean "mean")


           ("as" calc-simplify "simplify")
           ("an" calc-normalize-rat "rationalize")
           ("ad" calc-derivative "d/dx")
           ("ai" calc-integral "integrate")
           ("aS" calc-solve-for "solve")
           ("aP" calc-poly-roots "solve all")
           ("=" calc-evaluate "evaluate")
           
           ("aI" calc-num-integral "integrate num")
           ("aR" calc-find-root "find root")
           ("aN" calc-find-minimum "find min")
           ("aX" calc-find-maximum "find max")
           ("aF" calc-curve-fit "fit curve [[xs ys]]")
           ("N" calc-eval-num "evaluate numeric")
           
           ("SPC" calc-function-hydra-2/body :exit t)
           ))

       (hydra-style-hint calc-function-hydra))


(progn
  (eval
   `(defhydra calc-function-hydra-2
      (:quit-key "q"
       :hint none
       :base-map (make-sparse-keymap)
       :foreign-keys run
       :pre (setq which-key-inhibit t)
       :post (setq which-key-inhibit nil))
      ,(calc-function-hydra-docstring
        "
     *Graphs* ^^        *Binary*
_gf_ graph 2d ^^        
_gF_ graph 3d ^^        
_ga_ add curv ^^        
_gp_ replot   ^^        
scale _gl_ x _gL_ y     
range _gr_ x _gR_ y     
style _gs_ l _gS_ p     
")

      ,@calc-function-hydra-topline-heads
      ("SPC" calc-function-hydra/body :exit t)

      ("F" calc-floor)
      
      ("i" calc-info)
      ("hk" calc-help-prefix)
      ("hc" calc-describe-key-briefly)
      ("hf" calc-describe-function)
      ("hv" calc-describe-variable)
      
      ("bP" calc-fin-pv "PV [r, n, x]")
      ("bF" calc-fin-fv "FV [r, n, x]")
      ("bN" calc-fin-npv "PV [r, xs]")
      ("ba" calc-logical-and)
      ("bo" calc-logical-or)
      ("bx" calc-logical-xor)
      ("bn" calc-logical-not)
      ("bl" calc-lshift-binary)
      ("br" calc-rshift-binary)

      ("d0" calc-decimal-radix)
      ("d2" calc-binary-radix)
      ("d8" calc-octal-radix)
      ("d6" calc-hex-radix)
      ("dr" calc-radix)
      
      ("!" calc-factorial "factorial")
      ("kc" calc-choose "n choose m")
      ("kf" calc-prime-factors "factors")
      ("kg" calc-gcd "GCD")
      ("kl" calc-lcm "LCM")

      ("gf" calc-graph-fast "graph func")
      ("gF" calc-graph-fast-3d "graph 3d")
      ("gp" calc-graph-plot "replot")
      ("gP" calc-graph-print "print")
      ("ga" calc-graph-add "add curve")
      ("gN" calc-graph-num-points "set #")
      ("gs" calc-graph-line-style "line style")
      ("gS" calc-graph-point-style "point style")

      ("gl" calc-graph-log-x "log/lin x")
      ("gL" calc-graph-log-y "log/lin y")
      ("gr" calc-graph-range-x "range x")
      ("gR" calc-graph-range-y "range y")
      ("gq" calc-graph-quit "close")
      
      ))
  (hydra-style-hint calc-function-hydra-2))


(bind-key "SPC" 'calc-function-hydra/body calc-mode-map)
;(bind-key "s s" 'calc-store-show calc-mode-map)
;(bind-key "s S" 'calc-store calc-mode-map)
(provide 'calc-hydras)
