(* Définitions des transformations *)
open Graphics
;;

(* Définition des opérations dans les complexes *)
type complexe = C of ( float * float )
;;

let re z=match z with C(x,_)->x;;
let im z=match z with C(_,x)->x;;
let conj z=C(re(z), (-.im(z)));;

let moducar z=re(z)*.re(z) +. im(z)*.im(z);;

let plus x y=C(re(x)+.re(y), im(x)+.im(y));;

let moins x y=C(re(x)-.re(y), im(x)-.im(y));;

let fois x y=C(re(x)*.re(y)-.im(x)*.im(y), re(x)*.im(y)+.re(y)*.im(x));;

let divi x y= let a=fois x (conj y) in C(re(a)/.(moducar y),im(a)/.(moducar y));;

(* Ici arg(z) est dans ]-pi;pi] *)
let arg z=
  if re(z)= -1.
  then 4.*.atan(1.)
  else 2.*.atan(im(z)/.(re(z)+.1.))
;;

let valeurs a = [|C(0.,0.);C(a,0.);C(0.5,sqrt(a-.0.25));C(1.-.a,0.);C(1.,0.)|];;

let s alpha = let a=valeurs alpha and result = Array.make 4 conj
  in  for j=0 to 3 do
      result.(j)<-(function z -> plus (fois (moins a.(j+1) a.(j)) z) a.(j))
    done;
    result;;

(* Von Koch -> 1/3; Segment -> 1/4; Triangle -> 1/2; Essayez 0.4 ! *)
let similitudes=s (1./.3.);;
let s_0 = similitudes.(0);;
let s_1 = similitudes.(1);;
let s_2 = similitudes.(2);;
let s_3 = similitudes.(3);;

(* Tracé avec des itérations de similitudes *)
let rec calcul = function
  | [] -> []
  | t::q -> (s_0 t)::(s_1 t)::(s_2 t)::(s_3 t)::(calcul q)
;;

let convert t = int_of_float (1000.*.t)+10;; (* é adapter é la résolution de l'affichage *)
let affiche (C(x,y)) = plot (convert x) (convert y);;
let trace = List.iter affiche;;

let rec iteration liste = function
  | 0 -> trace liste
  | n -> ((*trace liste;*) iteration (calcul liste) (n-1))
;;

(* Tracé avec des convergences de suites de fonctions *)
let ligne (C(x,y)) = lineto (convert x) (convert y);;

let trace_fct f =
  moveto 10 10;
  for t=0 to 4999 do
    ligne (f (float_of_int t /.5000.));
  done
;;

let u f = function t ->
  let j=int_of_float (4.*.t) in
    if j=4
    then similitudes.(3)(f(4.*.t-.float_of_int j))
    else similitudes.(j)(f(4.*.t-.float_of_int j));;

let rec iteration_fct f = function
  | 0 -> trace_fct f
  | n -> ((*trace_fct f;*) iteration_fct (u f) (n-1))
;;

(* Définissons de la méme faéon le triangle de Serpinski *)
let similitudes=[|(function z -> fois (C(0.5,0.)) z);
            (function z -> plus (fois (C(0.5,0.)) z) (C(0.5,0.)));
            (function z -> plus (fois (C(0.5,0.)) z) (C(0.25,(sqrt 3.)/.4.)))|];;
let s_0 = similitudes.(0);;
let s_1 = similitudes.(1);;
let s_2 = similitudes.(2);;

(* Tracé avec des itérations de similitudes *)
let rec calcul = function
  | [] -> []
  | t::q -> (s_0 t)::(s_1 t)::(s_2 t)::(calcul q)
;;

let convert t = int_of_float (800.*.t)+10;; (* é adapter é la résolution de l'affichage *)
let affiche (C(x,y)) = plot (convert x) (convert y);;
let trace = List.iter affiche;;

let rec iteration_serpinski liste = function
  | 0 -> trace liste
  | n -> ((*trace liste;*) iteration_serpinski (calcul liste) (n-1))
;;

let main () =
  open_graph "";
  iteration_serpinski [C(0.,0.);C(1.,0.);C(0.5,(sqrt 3.)/.2.)] 9;
  iteration [C(0.,0.);C(1.,0.)] 9;
  iteration_fct (function t -> C(t,0.)) 9
;;

let _ = main ()
;;
