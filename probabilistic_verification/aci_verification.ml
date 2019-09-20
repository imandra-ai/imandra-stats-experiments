(* Functions for forming and verifying adaptive concentration inequalities *)

type t_term = Estimate of string
            | Constant of float
            | Plus of t_term * t_term
            | Neg of t_term
            | Times of t_term * t_term
            | Inv of t_term

type s_term = Gte_zero of t_term
            | And of s_term * s_term
            | Or of s_term * s_term
            | Not of s_term

type x_formula = { term     : t_term ;
                   estimate : float ;
                   epsilon  : float ;
                   delta    : float }

type y_formula = { term      : s_term ;
                   indicator : bool ;
                   gamma     : float }

let get_epsilon delta n =
  let a = (3. /. 5.) *. log ((log (n +. 1.)) /.  log (11. /. 10.)) in
  let b = (5. /. 9.) *. log (24. /. delta) in
  sqrt ((a +. b) /. n)

let get_vars spec =
  let vars = Hashtbl.create 1 in
  let rec from_t_term t =
    match t with
      | Estimate(name) -> Hashtbl.replace vars name 0
      | Constant(c) -> ()
      | Plus(t1, t2) -> from_t_term t1; from_t_term t2
      | Neg(t) -> from_t_term t
      | Times(t1, t2) -> from_t_term t1; from_t_term t2
      | Inv(t) -> from_t_term t in
  let rec from_s_term s = 
    match s with
      | Gte_zero(t) -> from_t_term t
      | And(s1, s2) -> from_s_term s1; from_s_term s2
      | Or(s1, s2) -> from_s_term s1; from_s_term s2
      | Not(s) -> from_s_term s
  in let () = from_s_term spec in
  vars

let get_delta spec =
  let rec from_t_term t =
    match t with
      | Estimate(name) -> 1
      | Constant(c) -> 0
      | Plus(t1, t2) -> from_t_term t1 + from_t_term t2
      | Neg(t) -> from_t_term t
      | Times(t1, t2) -> from_t_term t1 + from_t_term t2
      | Inv(t) -> from_t_term t in
  let rec from_s_term s = 
    match s with
      | Gte_zero(t) -> from_t_term t
      | And(s1, s2) -> from_s_term s1 + from_s_term s2
      | Or(s1, s2) -> from_s_term s1 + from_s_term s2
      | Not(s) -> from_s_term s
  in from_s_term spec

let aci_proof gamma spec =
  let rec get_x_formula t =
    match t with
      | Estimate(name) -> let x =
                            try Hashtbl.find gamma name
                            with Not_found -> failwith ("Variable " ^ name ^ " not given in defs") in
                          Some x
      | Constant(c) -> Some { term     = Constant(c) ;
                              estimate = c ;
                              epsilon  = 0. ;
                              delta    = 0. }
      | Plus(t1, t2) -> begin
                        match get_x_formula t1, get_x_formula t2 with
                          | None, _ -> None
                          | _, None -> None
                          | Some x1, Some x2 -> 
                            Some { term     = Plus(x1.term, x2.term) ;
                                   estimate = x1.estimate +. x2.estimate ;
                                   epsilon  = x1.epsilon +. x2.epsilon ;
                                   delta    = x1.delta +. x2.delta }
                        end
      | Neg(t) -> begin
                  match get_x_formula t with
                    | None -> None
                    | Some x ->
                      Some { term     = Neg(x.term) ;
                             estimate = -.x.estimate ;
                             epsilon  = x.epsilon ;
                             delta    = x.delta }
                  end
      | Times(t1, t2) -> begin
                         match get_x_formula t1, get_x_formula t2 with
                          | None, _ -> None
                          | _, None -> None
                          | Some x1, Some x2 ->
                           Some { term     = Times(x1.term, x2.term) ;
                                  estimate = x1.estimate *. x2.estimate ;
                                  epsilon  = ((abs_float x1.estimate) *. x2.epsilon) +. ((abs_float x2.estimate) *. x1.epsilon) +. (x1.epsilon *. x2.epsilon) ;
                                  delta    = x1.delta +. x2.delta }
                         end
      | Inv(t) -> begin
                  match get_x_formula t with
                    | None -> None
                    | Some x -> if (abs_float x.estimate) <= x.epsilon then None else
                      Some { term     = Inv(x.term) ;
                             estimate = 1. /. x.estimate ;
                             epsilon  = x.epsilon /. ((abs_float x.estimate) *. ((abs_float x.estimate) -. x.epsilon)) ;
                             delta    = x.delta }
                  end
  in let rec get_y_formula s =
    match s with
      | Gte_zero(t) -> begin
                      match get_x_formula t with
                        | None -> None
                        | Some x -> let result = x.estimate -. x.epsilon >= 0. in
                          Some { term      = Gte_zero(x.term) ;
                                 indicator = result ;
                                 gamma     = x.delta }
                      end  
      | And(s1, s2) -> begin
                      match get_y_formula s1, get_y_formula s2 with
                        | None, _ -> None
                        | _, None -> None
                        | Some y1, Some y2 ->
                          Some { term      = And(y1.term, y2.term) ;
                                 indicator = y1.indicator && y2.indicator ;
                                 gamma     = y1.gamma +. y2.gamma }
                        end
      | Or(s1, s2) -> begin
                      match get_y_formula s1, get_y_formula s2 with
                        | None, _ -> None
                        | _, None -> None
                        | Some y1, Some y2 ->
                          Some { term      = Or(y1.term, y2.term) ;
                                 indicator = y1.indicator || y2.indicator ;
                                 gamma     = y1.gamma +. y2.gamma }
                      end
      | Not(s) -> begin
                  match get_y_formula s with
                    | None -> None
                    | Some y ->
                      Some { term      = Not(y.term) ;
                             indicator = not y.indicator ;
                             gamma     = y.gamma }
                  end
  in get_y_formula spec
    
let verify ?(max=1000000) dist spec conf defs =

  let counts = Hashtbl.create (Hashtbl.length defs) in
  let init_count n _ = 
    Hashtbl.add counts n 0 in
  let () = Hashtbl.iter init_count defs in

  let m = get_delta spec in
  let delta = conf /. float m in

  let rec take_samples to_sample sampled =

    let update name count =
      let rec loop samples_left =
        match samples_left with
          | 0 -> ()
          | n -> 
            let sample = dist name in
            let condition = Hashtbl.find defs name in
            if condition sample then
              begin
              Hashtbl.replace counts name (count + 1);
              loop (n - 1)
              end
            else
              loop (n - 1)
      in loop to_sample in
       
    let () = Hashtbl.iter update counts in

    let epsilon = get_epsilon delta (float sampled) in

    let gamma = Hashtbl.create (Hashtbl.length counts) in

    let make_lemma name count = 
      let lemma = { term     = Estimate(name) ;
                    estimate = float count /. float sampled ;
                    epsilon  = epsilon ;
                    delta    = delta } in
      Hashtbl.add gamma name lemma in

    let () = Hashtbl.iter make_lemma counts in
    
    match aci_proof gamma spec with
      | Some y -> if y.gamma <= conf then 
                    y.indicator 
                  else if sampled < max then
                    take_samples 1000 (sampled + 1000)
                  else
                    let max_string = string_of_int max in
                    failwith ("No proof after " ^ max_string ^ " samples, consider\n
                                - Reducing the confidence required\n
                                - Checking that the value of any variable estimate E in a lemma including 1/E is not too close to 0\n
                                - Increasing the maximum number of samples")
      | None -> take_samples 1000 (sampled + 1000)

  in take_samples 1000 1000


(* Test example *)

type colour = Red | Green | Blue;;
  
type obj = {colour : colour;
            mass   : Q.t;
            faces  : Z.t};;

type dom = (obj * bool * Q.t);;

let f ((obj, sunny, temp) : dom) =
  let a = 
    if obj.colour = Red then
      obj.mass *. temp
    else
      100. -. temp
  in let b =
    if sunny then 
      obj.faces
    else 
      23
  in (a, b);;

let g ((obj, sunny, temp) : dom) =
  obj.colour = Green && temp >. 17.;;

let d () =
  let c = categorical ~classes:[Red; Green; Blue] ~probs:[0.5; 0.2; 0.3] () in
  let s = bernoulli ~p:0.4 () in
  let mu = if s then 20. else 10. in
  let t = logistic ~mu ~s:5.0 () in
  let m_constraints = [(10.4, 24.7);(33.2, 36.9)] in
  let m = uniform ~a:10.4 ~b:36.9 ~constraints:m_constraints () in
  let f = 
    if c = Green then
      poisson ~lambda:6.5 ~constraints:[(7, 10)] ()
    else
      poisson ~lambda:6.5 () in
  let o = {colour = c; mass = m; faces = f} in
  (o, s, t) [@@program];;

let defs = Hashtbl.create 3;;
 
let () = Hashtbl.add defs "v1" (fun (obj, sunny, temp) -> obj.colour = Red);
         Hashtbl.add defs "v2" (fun (obj, sunny, temp) -> obj.colour = Green);
         Hashtbl.add defs "v3" (fun (obj, sunny, temp) -> obj.colour = Blue);;

let dist _ = d ();;

let spec = And
             (Gte_zero
               (Plus
                 (Plus
                   (Estimate("v2"),
                    Estimate("v3")),
                  Neg
                    (Estimate("v1")))),
              Gte_zero(
                Neg
                  (Plus
                    (Plus
                      (Estimate("v2"),
                       Estimate("v3")),
                     Neg
                       (Estimate("v1"))))));;

let conf = 0.9;;

verify dist spec conf defs;;