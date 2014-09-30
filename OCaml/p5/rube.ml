open Ast

type value =
    VInt of int
  | VNil
  | VString of string
  | VObject of string * (string * value) list
    (* Invariant:  no field name appears twice in a VObject *)
  | VLoc of int

(*********************************************************************)

let rec output_expr o = function
  | EInt i -> Printf.fprintf o "%d" i
  | ENil -> Printf.fprintf o "nil"
  | ESelf -> Printf.fprintf o "self"
  | EString s -> Printf.fprintf o "\"%s\"" s
  | ELocal x -> output_string o x
  | EField x -> output_string o x
  | EIf (e1, e2, e3) ->
      Printf.fprintf o "if %a then %a else %a end" output_expr e1
	output_expr e2 output_expr e3
  | ESeq (e1, e2) -> Printf.fprintf o "%a; %a" output_expr e1 output_expr e2
  | EWrite (x, e) ->
      Printf.fprintf o "%s = (%a)" x output_expr e
  | EWriteField (x, e) ->
      Printf.fprintf o "%s = (%a)" x output_expr e
  | ENew x ->
      Printf.fprintf o "new %s" x
  | EInvoke (e, m, es) ->
      Printf.fprintf o "%a.%s(%a)" output_expr e m output_exprs es

and output_exprs o = function
    [] -> ()
  | [e] -> output_expr o e
  | e::es -> Printf.fprintf o "%a, %a" output_expr e output_exprs es

and output_strs o = function
    [] -> ()
  | [s] -> output_string o s
  | s::ss -> Printf.fprintf o "%a, %a" output_string s output_strs ss

and output_meth o ((name, args, body):meth) =
  Printf.fprintf o "  def %s(%a)\n    %a\n  end\n" name output_strs args output_expr body

and output_meths o = function
    [] -> ()
  | [m] -> Printf.fprintf o "%a" output_meth m
  | m::ms -> Printf.fprintf o "%a\n%a" output_meth m output_meths ms

and output_cls o ((name, super, meths):cls) =
  Printf.fprintf o "class %s < %s\n  %a\nend\n" name super output_meths meths

and output_clss o = function
    [] -> ()
  | [c] -> Printf.fprintf o "%a" output_cls c
  | c::cs -> Printf.fprintf o "%a\n%a" output_cls c output_clss cs

and print_program ((cs,e):prog) = match cs with
  | [] -> Printf.printf "%a\n" output_expr e
  | _ -> Printf.printf "%a\n%a\n" output_clss cs output_expr e

(*********************************************************************)

let rec print_fields o = function
  | [] -> ()
  | [f,v] -> Printf.fprintf o "%s=%a" f print_value v
  | (f,v)::fs -> Printf.fprintf o "%s=%a, %a" f print_value v print_fields fs

and print_value o = function
  | VInt n -> Printf.fprintf o "%d" n
  | VNil -> Printf.fprintf o "nil"
  | VString s -> Printf.fprintf o "%s" s
  | VObject (c, []) -> Printf.fprintf o "[%s]" c
  | VObject (c, fs) -> Printf.fprintf o "[%s %a]" c print_fields fs
  | VLoc n -> Printf.fprintf o "l%d" n

(* Local variable environment *)
type environment = (string * value) list

(* Heap *)
type heap = (int * value) list

let rec print_env o = function
    [] -> ()
  | [x,v] -> Printf.fprintf o "%s=%a" x print_value v
  | (x,v)::xs -> Printf.fprintf o "%s=%a\n%a" x print_value v print_env xs

let rec print_heap o = function
    [] -> ()
  | [l,v] -> Printf.fprintf o "l%d=%a" l print_value v
  | (l,v)::xs -> Printf.fprintf o "l%d=%a\n%a" l print_value v print_heap xs

(*********************************************************************)

(* PART 1 *)

let rec contains curr comp = match curr with
	[] -> false
	|(h::t) -> if h = comp then true else contains t comp
;;

let rec cdp4 (sl: string list):bool = match sl with
	[] -> false
	|(h::t) -> if contains t h then true else cdp4 t
;;

let cdp3 (m: meth):bool = match m with
	(s,sl,e) -> cdp4(sl)
;;

let rec cdp1 (c:cls):bool = match c with
	(s1,s2,h::t) -> if cdp3 h then true else cdp1(s1,s2,t)
	|_-> false
;;

	
let rec check_dup_params (p:prog):bool = match p with
	(h::t, e) -> if cdp1 h then true else check_dup_params (t,e)
	|_ -> false
	
;;

(*************************)
let rec fold f a l = match l with
    [] -> a
  | (h::t) -> fold f (f a h) t
;;


let rec u4 (h:string)(e:expr):bool = match e with
	(*true =  was used (SHORT CIRCUIT CASE), false = wasn't used*)
	
	EInt e -> false
	| ENil -> false
	| ESelf -> false
	| EString s -> false
	| ELocal e -> if e = h then true else false(* Read a local variable *)
	| EField e -> if e = h then true else false (* Read a field *)
	| EIf (e1,e2,e3) -> u4 h e1 || u4 h e2 || u4 h e3
	| ESeq (e1,e2) -> u4 h e1 || u4 h e2
	| EWrite (s,e) -> if s = h then true else u4 h e (* Write a local variable *)
	| EWriteField (s,e) -> if s = h then true else u4 h e  (* Write a field *)
	| ENew e -> false
	| EInvoke (e,s,[]) -> u4 h e 
	| EInvoke (e,s,(e2::t)) -> if u4 h e2 then true else u4 h (EInvoke(e,s,t))
;;

let rec u3 (sl: string list)(m:string)(e: expr)(c: string):((string * string * string) list) = match sl with
	(*iterates through the method args (sl) and calls u4 to check e for each*)
	[] -> []
	|(h::t)-> if u4 h e then (u3 t m e c) else [(c,m,h)]@(u3 t m e c)
;;

let rec u2 (ml: meth list)(c: string):((string * string * string) list) = match ml with
	[] -> []
	|((s,sl,e)::t) -> u3 sl s e c @ u2 t c
;;

let rec u1 (cl: cls list): ((string * string * string) list) = match cl with
	[] -> []
	|((s1,s2,ml)::t) -> (u2 ml s1)@u1 t 
;;

let rec unused (p:prog):((string * string * string) list) = match p with
	([],e) -> []@[]
	|(cl,e) -> u1 cl
;;

(******************)

exception Rename_error;;

let rec r_sl2 (str:string)(x:string)(y:string):string = match str with
	s -> if s = x then y else s
;;

let rec r_sl (sl: string list)(x:string)(y:string):string list = match sl with
	[] -> []
	|(h::t) -> (r_sl2 h x y)::(r_sl t x y)
;;


let rec r_el (f:(expr->string->string->expr))(el: expr list)(x:string)(y:string):expr list = match el with
	[] -> []
	|(h::t) -> (f h x y)::(r_el f t x y)
;;

let rec r_e (e: expr)(x:string)(y:string):expr = match e with
	|EInt e -> EInt e
	| ENil -> ENil
	| ESelf -> ESelf
	| EString e -> EString e
	| ELocal e -> if e = x then ELocal y else ELocal e(* Read a local variable *)
	| EField e -> if e = x then EField y else EField e(* Read a field *)
	| EIf (e1,e2,e3) -> EIf(r_e e1 x y, r_e e2 x y, r_e e3 x y)
	| ESeq (e1,e2) -> ESeq(r_e e1 x y, r_e e2 x y)
	| EWrite (s,e) -> if s = x then EWrite(y, r_e e x y) else EWrite(s, r_e e x y) (* Write a local variable *)
	| EWriteField (s,e) -> if s = x then EWriteField(y, r_e e x y) else EWriteField(s, r_e e x y)  (* Write a field *)
	| ENew e -> if e = x then ENew y else ENew e
	| EInvoke (e,s,el) -> EInvoke(r_e e x y,s,r_el r_e el x y)
;;

let rh4 (m1: meth)(x:string)(y:string):meth = match m1 with
	(s,sl,e) -> (s, r_sl sl x y, r_e e x y)
;;

let rec rh3 (ml:meth list) (m:string)(x:string)(y:string):meth list = match ml with
	[] -> []
	|((s,sl,e)::t) -> if s = m then (rh4 (s,sl,e) x y)::(rh3 t m x y) else (s,sl,e)::(rh3 t m x y)
;;


let rh2 (c1:cls) (m:string)(x:string)(y:string):cls = match c1 with (*found class*)
	(s1,s2,h::t) ->(s1,s2,rh3 (h::t) m x y)
	|(s1,s2,[]) -> (s1,s2,[])
;;

let rec rh1 (cl:cls list) (c:string) (m:string)(x:string)(y:string):cls list = match cl with
	[] -> []
	|((s1,s2,ml)::t) -> if s1 = c then (rh2 (s1,s2,ml) m x y)::(rh1 t c m x y) (*return modified class*)
						else (s1,s2,ml)::(rh1 t c m x y)
;;

let rename (p:prog) (c:string) (m:string) (x:string) (y:string):prog = match p with
	([],e) -> p
	|(cl,e) -> ((rh1 cl c m x y), e)
;;

(*********************************************************************)

(* PART 2 *)

let rec contains_c (cl:cls list)(c:string):bool = match cl with
	[] -> false
	|(s1,s2,ml)::t -> if s1 = c then true else contains_c t c
;;

let rec cdch (cl:cls list):bool = match cl with
	[] -> false
	|((s1,s2,ml)::t) -> if contains_c t s1 then true else cdch t
;;

let check_dup_classes (p:prog):bool = match p with
	([],e) -> false
	|(cl,e) -> cdch cl
;;

(************)

exception Temp_error;;

let rec soh1 (cl:cls list)(c: string):string = match cl with
	[] -> raise Not_found
	|((s1,s2,ml)::t) -> if s1 = c then s2 else soh1 t c
;;

let superclass_of (p:prog) (c:string):string = match p with
	(cl,e) -> if c = "Object" then "Object" else soh1 cl c
;;

(*************)

let rec contains_sc (cl:cls list) (c:string):bool = 
	if c = "Object" then true else contains_c cl c
;;

let rec check_sc_def (pcl:cls list)(cl:cls list):bool = match cl with
	[] -> true
	|((s1,s2,ml)::t) -> if contains_sc pcl s2 then check_sc_def pcl t else false
;;

let rec cc2 (orig:string) (curr_sc:string)(p:prog)(pcl: cls list):bool = match curr_sc with
	(*focuses on checking for a single class thru the entire pcl list*)
	(*true if well formed, false if bad*)
	"Object" -> true
	|s -> if curr_sc = orig then false else cc2 orig (superclass_of p curr_sc) p pcl
;;


let rec cc1 (p:prog)(cl:cls list):bool = match cl with
	(*cycles thru cls list and calls cc2 on each cls in the list*)
	((s1,s2,ml)::t) -> if cc2 s1 s2 p cl then cc1 p t else false
	|[] -> true
;;

let cyc_check (p:prog):bool = match p with
	(cl,e) -> cc1 p cl
;;

let wf_inheritance (p:prog):bool = match p with
	(*checks each class's sc existence and non-cyclicity separately:*)
	(cl,e) -> check_sc_def cl cl && cyc_check p 
;;

(************)

exception Lookup_error1;;
exception Lookup_error2;;
exception Lookup_error3;;

let rec sc_cls (cl: cls list)(c:string):cls = match cl with
	[] -> raise Not_found
	|h::t -> (match h with 
		(s1,s2,ml) -> if s1 = c then (s1,s2,ml) else sc_cls t c
	)
;;


let rec l2 (cl0: cls list)(cl: cls list)(c:string)(ml: meth list)(m:string):meth = match ml with
(*DON'T NEED C*)
[] -> (match (sc_cls cl0 c) with
	(cln,sc,ml2) -> if cln = "Object" then raise Not_found else l2 cl0 cl0 sc ml2 m
)
|((str,strl,ex)::t) -> if str = m then (str,strl,ex) else l2 cl0 cl0 c t m


and

l1 (cl0: cls list)(cl: cls list)(c:string)(ml2: meth list)(m:string):meth = match cl with
	(*match on class and give associated meth list to l2 to search thru meths*)
	(*DON'T NEED ML2*)
	[] -> raise Not_found (*if you can't find the class, it's wrong*)
	|((cln,sc,ml)::t) -> if cln = c then l2 cl0 cl0 c ml m  else l1 cl0 t c [] m
;;


let lookup (p:prog) (c:string) (m:string):meth = match p with
	(cl,e) -> l1 cl cl c [] m
;;




(* 
let rec l2 (cl0: cls list)(cl: cls list)(c:string)(ml: meth list)(m:string):meth = (match ml with
		[] -> 	if contains_c cl0 (soh1 cl0 c) 
					then (match (sc_str cl c) with 
						(s1,s2,methl) -> l1 cl0 cl0  s1 methl m (*meth not found in this cls*)
				)
				else raise Not_found
		|((s,sl,e)::t) ->	(if s = m 
									then (s,sl,e) 
									else l2 cl0 cl0 c t m))
and

l1 (cl0: cls list)(cl: cls list)(c:string)(ml2: meth list)(m:string):meth = match cl with
	(*match on class and give associated meth list to l2 to search thru meths*)
	[] -> raise Not_found
	|((s1,s2,ml)::t) -> (if s1 = c then l2 cl0 cl c ml m else l1 cl0 t c [] m)
;; *)

(*********************************************************************)

(* PART 3 *)

exception Eval_error


let rec env_contains (env: environment)(c:string):bool = match env with
	[] -> false
	|(s,v)::t -> if s = c then true else env_contains t c
;;

let rec env_get (env: environment)(c:string):int = match env with
	[] -> raise Eval_error
	|(s,v)::t -> if s = c then (match v with (VLoc l) -> l |_->raise Eval_error) else env_get t c
;;

let rec env_update (env: environment)(s:string)(v:value):environment = match env with
	[] -> []
	|(s1,v1)::t -> if s1 = s then (s1,v)::t else (s1,v1)::(env_update t s v)
;;

let env_main (env: environment)(s:string)(v:value):environment = 
	if not (env_contains env s) then ((s,v)::env) 
	else env_update env s v
;;

let rec vloc_contained (h:heap)(vl: int):bool = match h with
	[] -> false
	|(i,v)::t -> if i = vl then true else vloc_contained t vl
;;


let rec fl_update(fl:(string*value)list)(c:string)(v:value):((string*value)list) = match fl with
	[] -> [(c,v)]
	|(s,v2)::t -> if s = c then (s,v)::t else (s,v2)::(fl_update t c v)
;;


let rec heap_update (h:heap)(loc:value)(v:value)(s:string):heap = match h with
	[] -> []
	|((i,v2)::t) -> (match loc with 
		(VLoc l) -> 
			if i = l then (match v2 with 
				VObject (c,fl) -> (i,VObject(c,fl_update fl s v))::t(*(heap_update t loc v s) *)(*could just be ::t*)
				| _ -> raise Eval_error
			)
			else ((i,v2)::heap_update t loc v s) 
		|_-> raise Eval_error
		)
;;

let heap_main (h:heap)(env:environment)(s:string)(v:value):heap =
	if (vloc_contained h (env_get env "self")) then (heap_update h (List.assoc "self" env) v s) 
	else (((env_get env "self"), VObject ("self",[(s,v)]))::h)
;;


let rec fl_lookup (sl:((string*value)list))(s:string):value = match sl with
	[] -> VNil
	|((s1,v1)::t) -> if s1 = s then v1 else fl_lookup t s
;;

let field_main (v:value)(s:string):value = match v with
	VObject (c,fl) -> fl_lookup fl s
	|_ -> raise Eval_error
;;

let rec field_helper (h:heap)(v:value):value = match h with
	[] -> raise Eval_error
	|((i,v2)::t) -> (match v with (VLoc l) ->
		if i = l then v2
		else field_helper t v
		| _ -> raise Eval_error)
;;

let get_val (p:environment*heap*value):value = match p with
	(e,h,v) -> v
;;

(* EINVOKE STUFF: *)
let add (v1:value)(v2:value):value = match v1,v2 with
	VInt v1, VInt v2 -> VInt(v1+v2)
	| _ -> raise Eval_error
;;

let subtract (v1:value)(v2:value):value = match v1,v2 with
	VInt v1, VInt v2 -> VInt(v1-v2)
	| _ -> raise Eval_error
;;

let multiply (v1:value)(v2:value):value = match v1,v2 with
	VInt v1, VInt v2 -> VInt(v1*v2)
	| _ -> raise Eval_error
;;

let divide (v1:value)(v2:value):value = match v1,v2 with
	VInt v1, VInt v2 -> VInt(v1/v2)
	| _ -> raise Eval_error
;;

let eq (v1:value)(v2:value):value = match v1,v2 with
	VInt v1, VInt v2 -> if v1 = v2 then VInt 1 else VNil
	| _ -> raise Eval_error
;;

let to_str (v1:value):value = match v1 with
	VInt v1 -> VString(string_of_int v1)
	|VNil -> VString("nil")
	| _ -> raise Eval_error
;;

let print_nil (v1:value):value = match v1 with
	VString v1 -> print_string v1; VNil
	|_->raise Eval_error
;;


let rec heap_val_getter (loc:value)(h:heap):value = match loc with
	VLoc l -> (match h with 
		[] -> raise Temp_error
		|(i,v)::t -> if i = l then v 
					else heap_val_getter loc t
	)
	|_ -> raise Eval_error
;;

let rec class_name_getter (loc:value)(h:heap):(string) = match loc with
	VLoc l -> (match h with 
		[] -> raise Eval_error
		|(i,v)::t -> if i = l then (match v with
			VObject (s,svl) -> s
			|_ -> raise Eval_error
		) else class_name_getter loc t
	)
	|_ -> raise Eval_error
;;

let rec env_maker (env:environment)(args:string list)(vl:value list):environment = match args,vl with
	[],[] -> env
	|(sh::st),(vh::vt) -> (env_maker ([(sh,vh)]@env) st vt)
	|_,_ -> raise Eval_error
;;

let rec h_accumulator (f:((environment*heap*(cls list*expr))->(environment*heap*value)))(el:expr list)(cl:cls list)(input:(environment*heap*(value list))):(environment*heap*(value list)) = match input with
	(env,h,vl) ->
		(match el with
			[] -> (env,h,vl)
			|e::t -> (match f(env,h,(cl,e)) with
				(env2,h2,v2) -> h_accumulator f t cl (env2,h2,vl@[v2])
			)
		)
;;

exception Test_error1;;
exception Test_error2;;
	
let rec eph (p:environment*heap*(cls list*expr)):(environment*heap*value) = match p with
	(env,h,(cl,ex)) -> match ex with
		  EInt e -> (env,h,VInt e)
		  | ENil -> (env,h,VNil)
		  | ESelf -> (env,h,List.assoc "self" env)
		  | EString e -> (env,h,VString e)
		  | EIf (e1,e2,e3) -> (match eph(env,h,(cl,e1)) with
				(a,b,VNil) -> eph(a,b,(cl,e3))
				|(a,b,_) -> eph(a,b,(cl,e2)))
		  | ELocal e -> (env,h,List.assoc e env)
		  | EField s -> (env,h,field_main(field_helper h (List.assoc "self" env)) s)
		  | ESeq (e1,e2) -> (match eph(env,h,(cl,e1)) with
				(a,b,v1) -> eph(a,b,(cl,e2))
		  )
		  | EWrite (s,e)  -> if s = "self" then raise Eval_error else (match eph(env,h,(cl,e)) with 
				(a1,h1,v) -> (env_main a1 s v, h1, v))
		  | EWriteField (s,e) -> (match eph(env,h,(cl,e)) with 
				(a1,h1,v) -> (a1,(heap_main h1 env s v), v))
		  | ENew s -> if contains_c cl s || s = "Object" then (env,(List.length h,VObject(s,[]))::h,VLoc(List.length h)) else raise Eval_error
		  | EInvoke (e,s,el) -> if s = "send" then 
								(match el with 
									head::t -> (match  eph(env,h,(cl,head)) with 
										(env2, h2, VString s) -> eph(env2,h2,(cl,EInvoke(e,s,t)))
										|_ -> raise Eval_error)
									|_ -> raise Eval_error
								)		  
				else (match eph (env,h,(cl,e)) with
				(a1,h1,l) -> (match l with
					VLoc vlo -> (if s = "class_of" then 
									if List.length el = 0 
									then (match heap_val_getter (VLoc vlo) h1 with 
										VObject(var1,_) -> (a1,h1,VString var1)
										|_ -> raise Temp_error)
									else raise Temp_error 
								
								else if s = "+" || s = "-" || s = "*" || s = "/" then raise Eval_error 
								else if s = "equal?" || s = "print" || s = "to_s" then raise Eval_error 
								
								else (match (*HEAP*)(h_accumulator eph el cl (a1,h1,[])) with
									(env2,h2,val_list) -> (match (*meth*)(lookup (cl,ex) (class_name_getter l h1) s) with
										(name,args,body) -> (match eph ((env_maker ["self",(VLoc vlo)] args val_list),h2,(cl,body)) with
											(env3,h3,v3) -> (env2,h3,v3)
										)
									))
								
							)
					|VInt vint ->  (match (*HEAP*)(h_accumulator eph el cl (a1,h1,[])) with
						(env2,h2,vlh::vlt) -> 
							if s = "+" then
								if List.length el = 1 then (env2,h2,add (VInt vint) vlh) else raise Eval_error
							else if s = "-" then
								if List.length el = 1 then (env2,h2,subtract (VInt vint) vlh) else raise Eval_error
							else if s = "*" then
								if List.length el = 1 then (env2,h2, multiply (VInt vint) vlh) else raise Eval_error
							else if s = "/" then
								if List.length el = 1 then (env2,h2,divide (VInt vint) vlh) else raise Eval_error
							else if s = "equal?" then
								if List.length el = 1 then ( match vlh with
									VInt vint2 -> (env2,h2,eq (VInt vint) (VInt vint2)) 
									|_ -> raise Eval_error)
								else raise Eval_error	
							else raise Eval_error 		
						|(env2,h2,[]) -> 
							if s = "to_s" then
								if List.length el = 0 then (env2,h2,to_str (VInt vint)) else raise Eval_error									
							else raise Eval_error 
					)
					

					|VString vstr -> 
						if s = "print" then if List.length el = 0 then (a1,h1,print_nil (VString vstr)) else raise Eval_error	
						else raise Eval_error 
					
					|VObject (objs,_) -> 
						if s = "class_of" then 
							if List.length el = 0 then (a1,h1,VString objs)
							
							else raise Test_error1
						else raise Test_error2
						
					|VNil-> 
						if s = "to_s" then if List.length el = 0 then (a1,h1,to_str VNil) else raise Eval_error
						else raise Eval_error 
				)
			)
;;


(*do not touch*)


let eval_prog (p:prog):value = match p with
	(cl,e) -> get_val(eph([("self", (VLoc 0))], [(0,VObject ("Object", []))],p))
;;




