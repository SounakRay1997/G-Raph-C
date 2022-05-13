(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)
module F = Functions
module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "GraphC" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and void_t     = L.void_type   context 
  in 
  let string_t   = L.pointer_type i8_t in 
  let obj_t      = L.pointer_type i8_t in 
  let node_t     = L.pointer_type i8_t in
  let edge_t     = L.pointer_type i8_t in
  (* Return the LLVM type for a G-Raph-C type *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.String -> string_t  
    | A.Void   -> void_t
    | A.Node -> node_t
    | A.Edge -> edge_t
    | A.Graph | A.EList | A.NList-> obj_t
  in
  (* let rec type_str t = match t with
          A.Int -> "int"
        | A.Bool -> "bool"
        | A.String -> "string"
        | A.Void   -> "void"
        | A.Node -> "node"
        | A.Edge -> "edge"
        | A.Graph -> "graph"
        | A.NList -> "nlist"
        | A.EList -> "elist"
        (* | _ -> raise (Failure "Invalid string map key type") *)
    in *)

  

  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let default_val = match t with
        | A.String | A.Node | A.Edge | A.Void | A.NList | A.EList -> L.const_pointer_null (ltype_of_typ t)
        | A.Int | A.Bool  -> L.const_int (ltype_of_typ t) 0
        | _ -> L.const_pointer_null i32_t
      in StringMap.add n (L.define_global n default_val the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in
  
  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls   in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
      
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
      in

      (* finds local variable declarations in function body*)
      let local_vars = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
          let rec get_local_vars final_locals body = (match body with 
            [] -> final_locals
            | h::t -> get_local_vars (match h with      
              SDec(t, id, _ ) -> (t,id) :: final_locals
              | _ -> final_locals) t
          ) in
          let full_loc_vars = get_local_vars [] fdecl.sbody in 

      List.fold_left add_local local_vars full_loc_vars
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
      with Not_found ->  StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec build_expr builder ((_, e) : sexpr) = match e with
        SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SStringLit s -> L.build_global_stringptr s "string" builder
      | SPrint e    -> (match e with
          (A.Int,_) | (A.Bool, _) -> L.build_call printf_func [| int_format_str ; (build_expr builder e) |] "printf" builder
          | (A.String, _) -> L.build_call printf_func [| str_format_str ; (build_expr builder e) |] "printf" builder
          | _ -> raise (Failure ("printing not supported yet for expr: " ^ string_of_sexpr e ^ ".")))
      | SId s       -> L.build_load (lookup s) s builder
      | SNoexpr     -> L.const_int i32_t 0 
      | SAssign (s, e) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.Multi   -> L.build_mul
         | A.Divide  -> L.build_udiv
         | A.Modulus -> L.build_urem
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Equal   -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Less    -> L.build_icmp L.Icmp.Slt
         | A.Lte     -> L.build_icmp L.Icmp.Sle
         | A.Gt      -> L.build_icmp L.Icmp.Sgt
         | A.Gte     -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" builder

      (* for builtin functions, get name from functions.ml and declare and call that function *)
      | SCall(builtin_name, args) when StringMap.mem builtin_name F.builtin_fns -> 
        let fdecl = StringMap.find builtin_name F.builtin_fns in 
        let rettype = function A.Void -> i32_t | _ as rtyp -> ltype_of_typ rtyp in  (* Deals with void function calls *)
        let func_t : L.lltype = L.function_type (rettype fdecl.A.rtyp) (Array.of_list (List.map ltype_of_typ (List.map fst fdecl.A.formals))) in
        let func_value : L.llvalue = L.declare_function builtin_name func_t the_module 
        and llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        L.build_call func_value (Array.of_list llargs) (builtin_name ^ "_result") builder
      
      (* user defined functions *)
      | SCall (f, args) ->
        let (fdef, fdecl) =  StringMap.find f function_decls in
        let llargs =  List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = (match fdecl.srtyp with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list llargs) result builder
    in

    
  
    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt builder = function
        SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e -> ignore(build_expr builder e); builder
      | SReturn e -> ignore(match fdecl.srtyp with
                      A.Void -> L.build_ret_void builder
                      | _ -> L.build_ret (build_expr builder e) builder); builder
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb

      (* convert to while loop *)
      | SFor (dec, pred, inc, body) -> build_stmt builder
        (SBlock [ SExpr dec; (SWhile (pred, SBlock [ body; SExpr inc]))])

      | SDec (_, _, e) -> ignore(build_expr builder e); builder
      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb

    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (match fdecl.srtyp with 
      A.Void -> L.build_ret_void
      | _ -> L.build_ret (L.const_int i32_t 0))

  in

  List.iter build_function_body functions;
  the_module
