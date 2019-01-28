type  exptree =  N of int 
  |  Plus of exptree *  exptree 
  | Minus of exptree *  exptree 
  |  Mult of exptree *  exptree 
  | Div of exptree *  exptree 
  | Rem of exptree *  exptree 
  | Neg of  exptree 
  | Abs of  exptree 
;;
