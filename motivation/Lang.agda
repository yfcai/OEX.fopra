module Lang where

data Exp : Set where
  f       : Exp
  g       : Exp
  h       : Exp
  x       : Exp
  y       : Exp
  z       : Exp
  _+_     : Exp → Exp → Exp
  _*_     : Exp → Exp → Exp
  fun_=>_ : Exp → Exp → Exp
  _[_]    : Exp → Exp → Exp

infixr 1 fun_=>_
infixl 6 _+_
infixl 7 _*_
infix  9 _[_]

e1 : Exp
e2 : Exp
e3 : Exp
e4 : Exp
e5 : Exp

e1 = x * y + x * z
e2 = fun x => x
e3 = fun x => x + x
e4 = fun f => fun x => f [ x ]
e5 = (fun x => x [ x ]) [ fun x => x [ x ] ]

-- C^c C^n to normalize
