type var = string
type symbol = string
type signature = (symbol * int) list
type term =
    V of var
  | N of int
  | Node of symbol * (term list)
  | Underscore
type atom = Atom of symbol * term list
type goal = Goal of atom list
type head = Head of atom
type body = Body of atom list
type clause = Fact of head | Rule of head * body
type program = clause list
type substitution = (var * term) list

exception NotUnifiable
exception NotFound
exception InvalidProgram
exception NotPossible

let rec print_term_list tl = match tl with
    [] -> ()
  | [t] -> print_term t
  | t::tls ->
      print_term t;
      Printf.printf ",";
      print_term_list tls

and print_list_body = function
  | Node("_empty_list", []) -> ()
  | Node("_list", [t1; Node("_empty_list", [])]) -> print_term t1
  | Node("_list", [t1; t2]) -> 
    print_term t1;
    Printf.printf ",";
    print_list_body t2
  | _ -> raise NotPossible

and print_term = function
  | V(v) -> Printf.printf " %s " v
  | Node("_empty_list", []) -> Printf.printf " [] "
  | Node(s, []) -> Printf.printf " %s " s
  | Node("_list", _) as t -> 
    Printf.printf " [";
    print_list_body t;
    Printf.printf "] "
  | Node(s, l) -> 
    Printf.printf " %s ( " s;
    print_term_list l;
    Printf.printf " ) "
  | N(n) -> Printf.printf " %d " n
  | Underscore -> Printf.printf " _ "
;;

let union l1 l2 = 
  let combined = l1 @ l2 in
  List.sort_uniq compare combined
;;

let rec modifyTerm i t = match t with
    V(v) -> V((string_of_int i) ^ v)
  | Node(s, l) -> Node(s, List.map (modifyTerm i) l)
  | _ -> t
;;

let rec modifyAtom i a = match a with
  | Atom(s, l) -> Atom(s, List.map (modifyTerm i) l)
;;

let rec modifyClause cl i = match cl with
    Fact(Head(a)) -> Fact(Head(modifyAtom i a))
  | Rule(Head(a), Body(l)) -> Rule(Head(modifyAtom i a), Body(List.map (modifyAtom i) l))
;;

let rec modifyInitialProg prog i = match prog with
    [] -> []
  | cl::ps -> (modifyClause cl i)::modifyInitialProg ps (i+1)
;;

let rec modifyProg2 prog a = match prog, a with
    [], _ -> []
  | cl::ps, Atom(s, _) -> match cl with Fact(Head(Atom(s', _))) | Rule(Head(Atom(s', _)), _) ->
                if s = s' then (modifyClause cl 0)::modifyProg2 ps (Atom(s, []))
                else cl::modifyProg2 ps (Atom(s, []))
;;

let rec vars_term t =
  match t with
      V(v) -> [v]
    | Node(s, l) -> List.fold_left union [] (List.map vars_term l)
    | _ -> []
;;

let vars_atom a = match a with Atom(s, l) -> vars_term (Node(s, l))
;;

let rec vars_goal goal = match goal with Goal(g) -> List.fold_left union [] (List.map vars_atom g)
;;

let rec subst s t =
  match t with
      Node(s', l) -> Node(s', List.map (subst s) l)
    | N(_) -> t
    | Underscore -> t
    | V(x) -> match s with
                  [] -> t
                | s'::xs -> if fst s' = x then snd s' else subst xs t
;;

let rec subst_atom s a = match a with Atom(s', l) -> Atom(s', List.map (subst s) l)
;;

let rec varInTerm v t =
  match t with
      V(x) -> x = v
    | Node(s, l) -> List.fold_left (||) false (List.map (varInTerm v) l)
    | _ -> false
;;

let compose s1 s2 =
  let f s x = (fst x, subst s (snd x)) in (List.map (f s2) s1) @ s2
;;

let rec mgu_term t1 t2= match (t1, t2) with
  | V x, V y when x = y -> []
  | (N(n1), N(n2)) when n1 = n2 -> []
  | (Underscore, _) | (_, Underscore) -> []
  | V x, Node(_, _) | V x, N _ when not (varInTerm x t2) -> [(x, t2)]
  | Node(_, _), V y | N _, V y when not (varInTerm y t1) -> [(y, t1)]  
  | (Node(s1, l1), Node(s2, l2)) ->
      if s1 <> s2 || (List.length l1 <> List.length l2) then raise NotUnifiable
      else
        let f s tt = compose s (mgu_term (subst s (fst tt)) (subst s (snd tt))) in
        List.fold_left f [] (List.combine l1 l2)
  | _, _ -> raise NotUnifiable
;;

let mgu_atom a1 a2 = match a1, a2 with Atom(s1, l1), Atom(s2, l2) -> mgu_term (Node(s1, l1)) (Node(s2, l2))
;;

let rec getSolution unif vars = match vars with
    [] -> []
  | v::vs ->
      let rec occurs l = match l with
          [] -> raise NotFound
        | x::xs -> if (fst x) = v then x
                    else occurs xs
      in
      try (occurs unif)::getSolution unif vs
      with NotFound -> getSolution unif vs
;;

let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
          { termio with Unix.c_icanon = false } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res

let rec printSolution unif = match unif with
    [] -> Printf.printf "true. "
  | [(v, t)] -> (
      Printf.printf "%s =" v;
      print_term t;
    )
  | (v, t)::xs -> (
      Printf.printf "%s =" v;
      print_term t;
      Printf.printf ", ";
      printSolution xs;
    )
;;

let solve_atom_atom a1 a2 unif =
  compose unif (mgu_atom (subst_atom unif a1) (subst_atom unif a2))
;;

let solve_term_term t1 t2 unif =
  compose unif (mgu_term (subst unif t1) (subst unif t2))
;;

let rec simplify_term t= match t with
    N(_) -> t
  | Node("+", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (N(n1), N(n2)) -> N(n1 + n2)
        | _ -> raise NotUnifiable
    )
  | Node("-", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (N(n1), N(n2)) -> N(n1 - n2)
        | _ -> raise NotUnifiable
    )
  | Node("*", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (N(n1), N(n2)) -> N(n1 * n2)
        | _ -> raise NotUnifiable
    )
  | Node("/", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (N(n1), N(n2)) -> N(n1 / n2)
        | _ -> raise NotUnifiable
      )
  | _ -> t
;;

let eval a unif = match a with
    Atom("_equal", [t1; t2])
  | Atom("_not_equal", [t1; t2]) -> compose unif (mgu_term (simplify_term (subst unif t1)) (simplify_term (subst unif t2)))
  | Atom(">", [t1; t2]) -> (
        match (simplify_term (subst unif t1), simplify_term (subst unif t2)) with
            (N(n1), N(n2)) -> if n1 > n2 then unif else raise NotUnifiable
          | _ -> raise NotUnifiable
    )
  | Atom("<", [t1; t2]) -> (
      match (simplify_term (subst unif t1), simplify_term (subst unif t2)) with
          (N(n1), N(n2)) -> if n1 < n2 then unif else raise NotUnifiable
        | _ -> raise NotUnifiable
    )
  | _ -> unif
;;

let rec solve_goal prog g unif vars=
  match g with
      Goal([]) -> (
        printSolution (getSolution unif vars);
        flush stdout;
        let choice = ref (get1char()) in
        while(!choice <> '.' && !choice <> ';') do
          Printf.printf "\nUnknown Action: %c \nAction? " (!choice);
          flush stdout;
          choice := get1char();
        done;
        Printf.printf "\n";
        if !choice = '.' then (true, [])
        else (false, [])
      )
    | Goal(a::gs) -> match a with
          Atom("_equal", _) | Atom(">", _) | Atom("<", _) -> (
            try solve_goal prog (Goal(gs)) (eval a unif) vars
            with NotUnifiable -> (false, [])
          )
        | Atom("_not_equal", _) -> (
            try (false, eval a unif)
            with NotUnifiable -> solve_goal prog (Goal(gs)) unif vars
          )
        | Atom("_ofcourse", _) -> let _ = solve_goal prog (Goal(gs)) unif vars in (true, [])
        | Atom("integer", [t]) -> (
            match (simplify_term (subst unif t)) with
                N(_) -> solve_goal prog (Goal(gs)) unif vars
              | _ -> (false, [])
          )
        | _ ->
          let new_prog = modifyProg2 prog a in
          let rec iter prog' = match prog' with
              [] -> (false, [])
            | cl::ps -> match cl with
                Fact(Head(a')) -> (
                  try
                    let u = (solve_atom_atom a' a unif) in
                    match (solve_goal new_prog (Goal(gs)) u vars) with
                        (true, u') -> (true, u')
                      | _ -> iter ps
                  with NotUnifiable -> iter ps
                )
              | Rule(Head(a'), Body(al)) -> (
                  try
                    let u = (solve_atom_atom a' a unif) in
                    match (solve_goal new_prog (Goal(al @ gs)) u vars) with
                        (true, u') -> (true, u')
                      | _ -> iter ps
                  with NotUnifiable -> iter ps
                )
        in iter new_prog
;;

let interpret_goal g prog = solve_goal prog g [] (vars_goal g)
;;
