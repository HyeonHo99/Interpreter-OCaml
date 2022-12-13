module F = Format

let rec interp_expr (e: Ast.expr) (g: FStore.t) (s: Store.t) : Value.t = 
 (* Implement this function *)
    match e with
    | Ast.Num n -> NumV n
    | Ast.Add (expr1, expr2) -> let add_z n1 n2 = 
                                    (match n1,n2 with
                                    | Value.NumV z1, Value.NumV z2 -> let added = z1 + z2
                                                                      in
                                                                      Value.NumV (added))
                                in
                                add_z (interp_expr expr1 g s) (interp_expr expr2 g s)
    | Ast.Sub (expr1, expr2) -> let sub_z n1 n2 =
                                    (match n1,n2 with
                                    | Value.NumV z1, Value.NumV z2 -> let subtracted = z1 - z2
                                                                      in
                                                                      Value.NumV (subtracted))
                                in
                                sub_z (interp_expr expr1 g s) (interp_expr expr2 g s)
    | Ast.Id x -> (match Store.mem x s with
                    | true -> Store.find x s
                    | false -> failwith (F.sprintf "Free identifier: %s" x))
    | Ast.Call (x, el) -> (match FStore.mem x g with
                            | true -> let pl, e = FStore.find x g
                                      in
                                      (match (List.length pl = List.length el) with
                                                    | true -> let rec rec_up l1 l2 s1 s2 = 
                                                                        match l1 with
                                                                        | [] -> s2
                                                                        | h :: t -> (if List.length t = 0 then Store.add h (interp_expr (List.hd l2) s1 s2) s2
                                                                                    else let ss = Store.add h (interp_expr (List.hd l2) s1 s2) s2
                                                                                         in
                                                                                         rec_up t (List.tl l2) s1 ss)
                                                                    in
                                                                    interp_expr e g (rec_up pl el g s)
                                                    | false -> failwith (F.sprintf "The number of arguments of %s mismatched: Required: %d, Actual: %d" x (List.length pl) (List.length el)))
                            | false -> failwith (F.sprintf "Undefined function: %s" x))
    | Ast.LetIn (x, expr1, expr2) -> interp_expr expr2 g (Store.add x (interp_expr expr1 g s) s)

let interp_fundef (d: Ast.fundef) (g: FStore.t) : FStore.t = 
 (* Implement this function *)
    match d with
    | Ast.FunDef (func_name, param_list, expr1) -> (match FStore.mem func_name g with
                                                    | true
                                                    | false -> FStore.add func_name (param_list, expr1) g)

let interp (p: Ast.prog) : Value.t = 
 (* Implement this function *)
    let var_store = Store.empty
    in
    let rec update_fun_store fundef_list fun_store =
        match fundef_list with
        | [] -> fun_store
        | h :: t -> (match List.length t with
                    | 0 -> interp_fundef h fun_store
                    | _ -> update_fun_store t (interp_fundef h fun_store))
    in
    match p with
    | Ast.Prog (fundef_list, expr1) -> interp_expr expr1 (update_fun_store fundef_list FStore.empty) var_store