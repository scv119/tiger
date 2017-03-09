type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
       | OpExp of exp * binop * exp
       | EseqExp of stm * exp

val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

fun max a b = if a < b then b else a

fun maxargs (s:stm) = 
    let 
      fun meargs (e:exp) (n:int): int = 
        case e of 
          OpExp (e1, _, e2) => meargs e1 (meargs e2 n)
        | EseqExp (s1, e1) => msargs s1 (meargs e1 n)
        | _ => n
      and msargs (s:stm) (n:int): int =
        case s of 
          CompoundStm (st1, st2) => msargs st1 (msargs st2 n)
        | AssignStm (_, e) => meargs e n
        | PrintStm l => mlargs l (max n (length l)) 
      and mlargs (l: exp list) (n: int): int =
        case l of 
          [] => n
        | x::xs => mlargs xs (meargs x n)
    in 
      msargs s 0
    end
