(**

   Aesthetic Integration Limited
   Copyright (c) 2014 - 2019. All rights reserved.

   Visit https://www.imandra.ai for further information.

*)


(* Necessary modules *)

module RS = Random.State

module NC = Nocrypto.Rng

module GSL = Gsl.Rng

module D = Gsl.Cdf


(* Constants *)

let pi = 4. *. (atan 1.)

(* let log_2_pi = log (2. *. pi)  *)

let max_int_bits = (int_of_float (2. ** 30.)) - 1

let bits_26 = (int_of_float (2. ** 26.))


(* OCaml standard PRNG *)

let rs_seed = [| 19841983; 7298712389; 17862387612 |]

let rs_prng = RS.make rs_seed

let rs_bool st () = RS.bool st

let rs_int st ?(bound = max_int_bits) () = 
  let i = (RS.int st bound) in
  if rs_bool st () then i
  else -i

let rs_Z st ?(bound = max_int_bits) () = 
  let i = rs_int st ~bound () in Z.of_int i

let rs_float st ?(bound = float_of_int max_int_bits) () =
  let f = RS.float st bound in
  if rs_bool st () then f
  else -.f

let rs_Q st ?(bound = Q.of_int max_int_bits) () =
  let n = rs_Z st () in
  let d = rs_Z st () in
  Q.mul (Q.make n d) bound

let rs_base st ?(a = 0.) ?(b = 1.) () = 
  if a > b then failwith "Cannot sample from base of negative range"   
  else a +. RS.float st (b -. a)


(* Nocrypto PRNG *)

let nc_seed = Cstruct.of_string "19841983, 7298712389, 17862387612"

let nc_prng = NC.(create ~seed:nc_seed (module Generators.Fortuna))

let nc_bool st () = 
  let b = NC.Int.gen_bits ~g:st 1 in 
  if b = 0 then false else true

let nc_int st ?(bound = max_int_bits) () = 
  let i = (NC.Int.gen ~g:st bound) in
  if nc_bool st () then i
  else -i

let nc_Z st ?(bound = Z.of_int max_int_bits) () =
  let z = NC.Z.gen ~g:st bound in
  if nc_bool st () then z
  else Z.neg z
  
let nc_base st ?(a = 0.) ?(b = 1.) () = 
  if a > b then 
	failwith "Cannot sample from base of negative range"
  else 
	let x = float_of_int (NC.Int.gen ~g:st (pred bits_26)) in
    let y = float_of_int (NC.Int.gen ~g:st (pred bits_26)) in
    let scale = float_of_int bits_26 in
    let base = (x /. scale +. y) /. scale in
    (b -. a) *. base +. a

let nc_float st ?(bound = float_of_int max_int_bits) () = 
  let f = nc_base st ~a:0. ~b:bound () in
  if nc_bool st () then f
  else -.f
 
let nc_Q st ?(bound = Q.of_int max_int_bits) () =
  let n = nc_Z st () in
  let d = nc_Z st () in
  Q.mul (Q.make n d) bound


(* GSL PRNG *)

let gsl_seed = Nativeint.of_int 19841983

let gsl_prng = GSL.make GSL.MT19937

let () = GSL.set gsl_prng gsl_seed

let gsl_bool st () =
  let b = GSL.uniform_int st 2 in
  if b = 0 then false else true

let gsl_int st ?(bound = max_int_bits) () = 
  let i = (GSL.uniform_int st bound) in
  if gsl_bool st () then i
  else -i

let gsl_Z st ?(bound = max_int_bits) () = 
  let i = gsl_int st ~bound () in Z.of_int i
  
let gsl_base st ?(a = 0.) ?(b = 1.) () = 
  if a > b then 
	failwith "Cannot sample from base of negative range"
  else 
	let base = GSL.uniform st in
    (b -. a) *. base +. a

let gsl_float st ?(bound = float_of_int max_int_bits) () = 
  let f = gsl_base st ~a:0. ~b:bound () in
  if gsl_bool st () then f
  else -.f
 
let gsl_Q st ?(bound = Q.of_int max_int_bits) () =
  let n = gsl_Z st () in
  let d = gsl_Z st () in
  Q.mul (Q.make n d) bound


(* Primitive random element *)

type prng = RS | NC | GSL

let base ?(prng = NC) ?(a = 0.) ?(b = 1.) () = 
  match prng with
    | RS -> rs_base rs_prng ~a ~b ()
    | NC -> nc_base nc_prng ~a ~b ()
    | GSL -> gsl_base gsl_prng ~a ~b ()


(* Helper functions *)

let bounded x = x >= 0. && x <= 1.

let sum l = List.fold_left (+.) 0. l
 
let normalise l = let s = (sum l) in List.map (fun x -> x /. s) l

let log_factorial x =
  let rec loop i a =
    if i > x then a
    else loop (i +. 1.) (a +. log i)
  in loop 1. 0.
  
let choose n k =
  if k < 0. then failwith "Must choose a non-negative number k in nCk"
  else if n < 0. then failwith "Must choose from non-negative number n in nCk"
  else
    let rec loop i j a =
      if j = 0. then a
      else loop (i -. 1.) (j -. 1.) (a *. (i /. j))
    in loop n k 1.

(* let log_nemes_closed_form x =
  let log_x = log x in
  let fifteen_x_sq = 15. *. (x ** 2.) in
  (x *. log_x) -. x +. (0.5 *. (log_2_pi -. log_x)) +. (1.25 *. x *. (log (fifteen_x_sq +. 1.) -. log (fifteen_x_sq))) *)
  
(* let gamma x = (exp (log_nemes_closed_form (x +. 1.))) /. x *)
  
let constrain_categorical (constraints) (classes, probs) =
  let rec loop old_c old_p new_c new_p =
	match old_c, old_p with
	  | [], _ -> new_c, new_p
	  | _, [] -> new_c, new_p
	  | c :: cs, p :: ps ->
		if List.mem c constraints then
		  loop cs ps (c :: new_c) (p :: new_p)
		else
		  loop cs ps new_c new_p
  in let new_c, new_p = loop classes probs [] [] in
  if sum new_p = 0. then failwith "Constrained classes have zero total probability mass"
  else new_c, (normalise new_p)

let constraint_comparison (a, b) (a', b') =
  if compare a a' <> 0 then compare a a' else compare b b'

let get_uniform_constraints l cdf =
  let sorted = List.sort constraint_comparison l in
  let rec loop rs u_rs w_rs last_b =
	match rs with
	  | [] -> (u_rs, w_rs)
	  | (a, b) :: t -> 
        let new_b =
        match last_b with
          | None -> a
          | Some b' -> b' in
        if b < a then failwith "Each constraint (a, b) must be such that b >= a"
        else if a < new_b then failwith "Constraints must not overlap"
        else
		  let (c_a, c_b) = (cdf a, cdf b) in
		  let w = c_b -. c_a in
		  loop t ((c_a, c_b) :: u_rs) (w :: w_rs) (Some b)
  in let (u_rs, w_rs) = loop sorted [] [] None in
  if sum w_rs = 0. then failwith "Constrained regions have zero total probability mass"
  else u_rs, (normalise w_rs)

(* let process_constraints l =
  let sorted = List.sort constraint_comparison l in
  let rec loop l' low high domain_size last_b =
    match l' with
      | [] -> sorted, (low, high), (domain_size /. (high -. low))
      | (a, b) :: t ->
        let new_b =
        match last_b with
          | None -> a
          | Some b' -> b' in
        if b < a then failwith "Each constraint (a, b) must be such that b >= a"
        else if a < new_b then failwith "Constraints must not overlap"
        else
          let new_low = min a low in
          let new_high = max b high in
          let new_domain_size = domain_size +. (b -. a) in
          loop t new_low new_high new_domain_size (Some b)
  in loop sorted max_float min_float 0. None

let closest m cs =
  let rec loop curr dist list =
    match list with
      | [] -> curr
      | h :: t -> 
        let (a, b) = h in
        if a <= m && m <= b then
          m
        else
          let a_dist = abs_float (a -. m) in
          let b_dist = abs_float (b -. m) in
          if dist = 0. then
            if a_dist < b_dist then loop a a_dist t
            else loop b b_dist t
          else if a_dist < dist then 
            if a_dist < b_dist then loop a a_dist t
            else loop b b_dist t
          else if b_dist < dist then
            loop b b_dist t
          else
            loop curr dist t
  in loop 0. 0. cs *)

let rec make_inclusive c c_inclusive =
  match c with
    | [] -> c_inclusive
    | (a, b) :: t -> make_inclusive t ((a - 1, b) :: c_inclusive)

let float_of constraints =
  match constraints with
    | None -> None
    | Some c_list -> Some (List.map (fun (x, y) -> (Q.to_float x, Q.to_float y)) c_list)

let int_of constraints =
  match constraints with
    | None -> None
    | Some c_list -> Some (List.map (fun (x, y) -> (Z.to_int x, Z.to_int y)) c_list)

let print_probs ?(precision = Z.of_int 5) probs =
  let printer i f = 
    let padding =
      if i < 10 then "        "
      else if i < 100 then "       "
      else if i < 1000 then "      "
      else if i < 10000 then "     "
      else failwith "Too many regions to print" in
    Printf.fprintf stdout "%n" i;
    Printf.fprintf stdout "%s" padding;
    Printf.fprintf stdout "%.*f\n" (Z.to_int precision) f in
  print_string "Region   Probability\n"; 
  Hashtbl.iter printer probs;
  print_endline ""


(* Quantile functions *)

let q_bernoulli ~p x = 
  if x <= (1. -. p) then false else true

let q_binomial ~n ~p x =
  let rec get_successes k prob =
    let term = (choose n k) *. (p ** k) *. ((1. -. p) ** (n -. k)) in
    let new_term = term +. prob in
    if x <= new_term || k = n then int_of_float k
    else get_successes (k +. 1.) new_term
  in if x = 1. then int_of_float n else
  get_successes 0. 0.

let q_categorical ~classes ~probs x =
  let rec loop prob cs ps =
    match cs, ps with
      | [], _ -> failwith "Must have same number of classes as probabilities"
      | _, [] -> failwith "Must have same number of classes as probabilities"
      | c :: _cs, p :: _ps -> 
        let new_prob = p +. prob in
        if x <= new_prob || new_prob = 1. then c
        else loop new_prob _cs _ps
  in loop 0. classes probs

let q_cauchy ~x_0 ~gamma x =
  if x = 0. then -.infinity
  else if x = 1. then infinity
  else x_0 +. (gamma *. tan(pi *. (x -. 0.5)))

let q_exponential ~lambda x = 
  if x = 1. then -.infinity
  else -.(log (1. -. x)) /. lambda

let q_laplace ~mu ~b x = 
  if x = 0. then -.infinity
  else if x = 1. then infinity
  else if x <= 0.5 then mu +. (b *. log (2. *. x))
  else mu -. (b *. log (2. -. (2. *. x)))

let q_logistic ~mu ~s x = 
  if x = 0. then -.infinity
  else if x = 1. then infinity
  else mu +. (s *. log (x /. (1. -. x)))

let q_poisson ~lambda x =
  let rec get_successes k prob =
    let log_term = -.lambda +. (k *. log lambda) -. log_factorial k in
    let term = exp log_term in
    if x <= term +. prob then int_of_float k
    else get_successes (k +. 1.) (term +. prob)
  in if x = 1. then max_int
  else get_successes 0. 0.

let q_uniform ~a ~b x = 
  a +. ((b -. a) *. x)

let q_beta ~a ~b x = 
  D.beta_Pinv ~p:x ~a ~b

let q_gamma ~k ~theta x =
  D.gamma_Pinv ~p:x ~a:k ~b:theta

let q_gaussian ~mu ~sigma x =
  mu +. D.gaussian_Pinv ~p:x ~sigma

let q_lognormal ~mu ~sigma x =
  D.lognormal_Pinv ~p:x ~zeta:mu ~sigma


(* Cumulative density functions *)

let c_bernoulli ~p x = 
  if x then 1. else (1. -. p)

let c_binomial ~n ~p x =
  let rec get_prob k prob =
    if k > x then prob
    else
      let term = (choose n k) *. (p ** k) *. ((1. -. p) ** (n -. k)) in
      get_prob (k +. 1.) (term +. prob)
  in if x > n then 1.0
  else get_prob 0. 0.

let c_categorical ~classes ~probs x =
  let rec loop prob cs ps =
    match cs, ps with
      | [], _ -> failwith "Must have same number of classes as probabilities"
      | _, [] -> failwith "Must have same number of classes as probabilities"
      | c :: _cs, p :: _ps -> 
        if c = x then (p +. prob)
        else loop (p +. prob) _cs _ps
  in loop 0. classes probs

let c_cauchy ~x_0 ~gamma x = 
  (1. /. pi) *. atan ((x -. x_0) /. gamma) +. 0.5

let c_exponential ~lambda x = 
  if x < 0. then 0.
  else 1. -. exp (-.lambda *. x) 

let c_laplace ~mu ~b x = 
  if x <= mu then 0.5 *. exp ((x -. mu) /. b)
  else 1. -. (0.5 *. exp (-. (x -. mu) /. b))

let c_logistic ~mu ~s x = 
  1. /. (1. +. exp (-. (x -. mu) /. s))

let c_poisson ~lambda x =
  let rec get_prob k prob =
    if k > x then prob
    else
      let log_term = -.lambda +. (k *. log lambda) -. log_factorial k in
      let term = exp log_term in
      get_prob (k +. 1.) (term +. prob)
  in get_prob 0. 0.

let c_uniform ~a ~b x =
  if x < a then 0.
  else if x > b then 1.
  else (x -. a) /. (b -. a)

let c_beta ~a ~b x = 
  D.beta_P ~x ~a ~b

let c_gamma ~k ~theta x =
  D.gamma_P ~x ~a:k ~b:theta

let c_gaussian ~mu ~sigma x =
  D.gaussian_P ~x:(x -. mu) ~sigma

let c_lognormal ~mu ~sigma x =
  D.lognormal_P ~x ~zeta:mu ~sigma


(* Density functions *)

(* let d_beta ~a ~b x =
  if x < 0. then failwith "Beta PDF is not defined for x < 0"
  else if x > 1. then failwith "Beta PDF is not defined for x > 1"
  else if a < 1. && x = 0. then infinity
  else if b < 1. && x = 1. then infinity
  else
    let z = (gamma a) *. (gamma b) /. gamma (a +. b) in
    (1. /. z) *. (x ** (a -. 1.)) *. ((1. -. x) ** (b -. 1.))

let d_gamma ~k ~theta x =
  if x < 0. then failwith "Gamma PDF is not defined for x < 0"
  else if k < 1. && x = 0. then infinity
  else 
    let z = (gamma k) *. (theta ** k) in
    (1. /. z) *. (x ** (k -. 1.)) *. (exp (-.x /. theta))

let d_gaussian ~mu ~sigma x =
  let z = sigma *. sqrt (2. *. pi) in
  let e = ((x -. mu) ** 2.) /. (2. *. (sigma ** 2.)) in
  (1. /. z) *. exp (-.e)

let d_lognormal ~mu ~sigma x =
  if x < 0. then failwith "LogNormal PDF is not defined for x < 0"
  else if x = 0. then 0.
  else
    let z = x *. sigma *. sqrt (2. *. pi) in
    let e = (((log x) -. mu) ** 2.) /. (2. *. (sigma ** 2.)) in
    (1. /. z) *. exp (-.e) *)


(* Sampling algorithms *)

let inverse_transform_sample qf (u_rs, n_w_rs) =
  match u_rs, n_w_rs with
	| [], [] -> qf (base ())
	| _, _ -> let (r1, r2) = q_categorical (base ()) ~classes:u_rs ~probs:n_w_rs in
              base ~a:r1 ~b:r2 () |> qf
    
(* let mcmc_sample pdf constraints bounds gradient cur_x step =
  let (lower, upper) = bounds in
  let rec apply_constraints p constraints prev =
    match constraints with
      | (a, b) :: t -> 
        let new_region = prev +. ((b -. a) /. gradient) in
        if p <= new_region then
          Some (((p -. prev) *. gradient) +. a)
        else
          apply_constraints p t new_region
      | [] -> None
  in let rec get_proposal () =
    let new_x = q_logistic ~mu:cur_x ~s:step (base ()) in
    if new_x < lower || new_x > upper then
      get_proposal ()
    else if constraints = [] then
      new_x
    else
      let result = apply_constraints new_x constraints lower in
      match result with
        | None -> get_proposal ()
        | Some f -> f 
  in let new_x = get_proposal () in
  let ratio = (pdf new_x) /. (pdf cur_x) in
  if base () <= ratio then new_x else cur_x *)


(* Sampler functor and related types *)

module type Inverse_Transform_S = sig
  type value
  val qf : float -> value
  val cdf : value -> float
  val constraints : (float * float) list * float list
end

(* module type MCMC_S = sig
  type value
  val pdf : float -> float
  val constraints : (float * float) list
  val bounds : float * float
  val gradient : float
  val start : float
  val step : float
  val to_burn : int
end *)

module Sampler = struct

  module type S = sig
    type value
    val sample_n : ?start:float -> ?step:float -> int -> value list
    val sample_1 : unit -> value
  end

  module Make_Inverse_Transform (D : Inverse_Transform_S) : S with type value = D.value = struct
    type value = D.value
    let sample_n ?start:_ ?step:_ n =
      let rec loop samples num =
        match num with
          | 0 -> samples
          | _ -> 
            let s = inverse_transform_sample D.qf D.constraints in 
            loop (s :: samples) (num - 1)
      in loop [] n
    let sample_1 () = inverse_transform_sample D.qf D.constraints
  end

  (* module Make_MCMC (D : MCMC_S) : S with type value = float = struct
    type value = float
    let batch = D.to_burn / 20
    let lower, upper = D.bounds
    let max_step = (upper -. lower)
    let rec burn b_num b_last b_step a r =
        match b_num with
          | 0 -> b_step
          | _ -> 
            let s = mcmc_sample D.pdf D.constraints D.bounds D.gradient b_last b_step in 
            let a', r' = if s = b_last then a, r +. 1. else a +. 1., r in
            if (b_num - 1) mod batch = 0 then
              let rate = a' /. (a' +. r') in
              let diff = 1. +. (rate -. (1./.3.)) in
              let new_step = min (diff *. b_step) max_step in
              burn (b_num - 1) s new_step 0. 0.
            else
              burn (b_num - 1) s b_step a' r'
    let burn_in_step = burn D.to_burn D.start D.step 0. 0.
    let sample_n ?(start = D.start) ?(step = burn_in_step) n =
      let rec loop samples num last a r =
        match num with
          | 0 -> samples
          | _ -> 
            let s = mcmc_sample D.pdf D.constraints D.bounds D.gradient last step in 
            if s = last then loop (s :: samples) (num - 1) s a (r + 1)
            else loop (s :: samples) (num - 1) s (a + 1) r
      in loop [] n start 0 0
    let sample_1 () = D.start
    end *)

end


(* Distribution modules *)

module Bernoulli 
  (Params : sig val p : float end) 
  (Constraints : sig val c : bool option end)
  : Sampler.S with type value = bool = struct
    assert (bounded Params.p)
    include Sampler.Make_Inverse_Transform (struct
      type value = bool
      let qf x = q_bernoulli x ~p:Params.p
      let cdf x = c_bernoulli x ~p:Params.p
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some b -> if b then [(1., 1.)], [1.] else [(0., 0.)], [1.]
    end)
end

(* module Beta 
  (Params : sig val a : float val b : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.a > 0. &&
            Params.b > 0.)
    include Sampler.Make_MCMC (struct
      type value = float
      let pdf x = d_beta x ~a:Params.a ~b:Params.b
      let constraints, bounds, gradient = 
        match Constraints.c with
          | None -> [], (0., 1.), 0.
          | Some l -> process_constraints l
      let step =
        let lower, upper = bounds in 0.2 *. (upper -. lower)
      let start = 
        let mean = Params.a /. (Params.a +. Params.b) in
        if constraints = [] then mean
        else closest mean constraints
      let to_burn = if true then 20000 else 0
    end)
end *)

module Beta 
  (Params : sig val a : float val b : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.a > 0. &&
            Params.b > 0.)
    include Sampler.Make_Inverse_Transform (struct
      type value = float
      let qf x = q_beta x ~a:Params.a ~b:Params.b
      let cdf x = c_beta x ~a:Params.a ~b:Params.b
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints l cdf
    end)
end

module Binomial 
  (Params : sig val n : int val p : float end) 
  (Constraints : sig val c : (int * int) list option end)
  : Sampler.S with type value = int = struct
    assert (bounded Params.p &&
            Params.n >= 0)
    include Sampler.Make_Inverse_Transform (struct
      type value = int
      let float_n = float Params.n
      let qf x = q_binomial x ~n:float_n ~p:Params.p
      let cdf x = c_binomial (float x) ~n:float_n ~p:Params.p
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints (make_inclusive l []) cdf
    end)
end

module Categorical 
  (Params : sig type t val classes : t list val probs : float list end) 
  (Constraints : sig type t = Params.t val c : t list option end)
  : Sampler.S with type value = Params.t = struct
    assert (Params.classes <> [] &&
            Constraints.c <> Some [] &&
            List.length Params.classes = List.length Params.probs &&
            List.for_all bounded Params.probs &&
            sum Params.probs = 1.)
    include Sampler.Make_Inverse_Transform (struct
      type value = Params.t
      let constraints = [], []
      let cs, ps =
        match Constraints.c with
          | None -> Params.classes, Params.probs
          | Some l -> constrain_categorical l (Params.classes, Params.probs) 
      let qf x = q_categorical x ~classes:cs ~probs:ps
      let cdf x = c_categorical x ~classes:cs ~probs:ps
    end)
end

module Cauchy 
  (Params : sig val x_0 : float val gamma : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.gamma > 0.)
    include Sampler.Make_Inverse_Transform (struct
      type value = float
      let qf x = q_cauchy x ~x_0:Params.x_0 ~gamma:Params.gamma
      let cdf x = c_cauchy x ~x_0:Params.x_0 ~gamma:Params.gamma
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints l cdf
    end)
end

module Exponential 
  (Params : sig val lambda : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.lambda > 0.)
    include Sampler.Make_Inverse_Transform (struct
      type value = float
      let qf x = q_exponential x ~lambda:Params.lambda
      let cdf x = c_exponential x ~lambda:Params.lambda
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints l cdf
    end)
end

(* module Gamma 
  (Params : sig val k : float val theta : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.k > 0. 
            && Params.theta > 0.)
    include Sampler.Make_MCMC (struct
      type value = float
      let pdf x = d_gamma x ~k:Params.k ~theta:Params.theta
      let constraints, bounds, gradient = 
        match Constraints.c with
          | None -> [], (0., max_float), 0.
          | Some l -> process_constraints l
      let step =
        let std = Params.theta *. sqrt Params.k in
        let lower, upper = bounds in
        if constraints = [] then 2.5 *. std
        else min (0.2 *. (upper -. lower)) (2.5 *. std)
      let start = 
        let mean = Params.k *. Params.theta in
        if constraints = [] then mean
        else closest mean constraints
      let to_burn = if true then 20000 else 0
    end)
end *)

module Gamma 
  (Params : sig val k : float val theta : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.k > 0. 
            && Params.theta > 0.)
    include Sampler.Make_Inverse_Transform (struct
      type value = float
      let qf x = q_gamma x ~k:Params.k ~theta:Params.theta
      let cdf x = c_gamma x ~k:Params.k ~theta:Params.theta
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints l cdf
    end)
end

(* module Gaussian
  (Params : sig val mu : float val sigma : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.sigma > 0.)
    include Sampler.Make_MCMC (struct
      type value = float
      let pdf x = d_gaussian x ~mu:Params.mu ~sigma:Params.sigma
      let constraints, bounds, gradient = 
        match Constraints.c with
          | None -> [], (min_float, max_float), 0.
          | Some l -> process_constraints l
      let step =
        let std = Params.sigma in
        let lower, upper = bounds in
        if constraints = [] then 2.5 *. std
        else min (0.25 *. (upper -. lower)) (2.5 *. std)
      let start = 
        let mean = Params.mu in
        if constraints = [] then mean
        else closest mean constraints
      let to_burn = 
        if true then 20000 else 0
    end)
end *)

module Gaussian
  (Params : sig val mu : float val sigma : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.sigma > 0.)
    include Sampler.Make_Inverse_Transform (struct
      type value = float
      let qf x = q_gaussian x ~mu:Params.mu ~sigma:Params.sigma
      let cdf x = c_gaussian x ~mu:Params.mu ~sigma:Params.sigma
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints l cdf
    end)
end

module Laplace 
  (Params : sig val mu : float val b : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.b > 0.)
    include Sampler.Make_Inverse_Transform (struct
      type value = float
      let qf x = q_laplace x ~mu:Params.mu ~b:Params.b
      let cdf x = c_laplace x ~mu:Params.mu ~b:Params.b
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints l cdf
    end)
end

module Logistic 
  (Params : sig val mu : float val s : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.s > 0.)
    include Sampler.Make_Inverse_Transform (struct
      type value = float
      let qf x = q_logistic x ~mu:Params.mu ~s:Params.s
      let cdf x = c_logistic x ~mu:Params.mu ~s:Params.s
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints l cdf
    end)
end

(* module LogNormal
  (Params : sig val mu : float val sigma : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.sigma > 0.)
    include Sampler.Make_MCMC (struct
      type value = float
      let pdf x = d_lognormal x ~mu:Params.mu ~sigma:Params.sigma
      let constraints, bounds, gradient = 
        match Constraints.c with
          | None -> [], (0., max_float), 0.
          | Some l -> process_constraints l
      let step =
        let std = ((exp (Params.sigma ** 2.)) -. 1.) *. exp ((2. *. Params.mu) +. (Params.sigma ** 2.)) in
        let lower, upper = bounds in
        if constraints = [] then 2.5 *. std
        else min (0.25 *. (upper -. lower)) (2.5 *. std)
      let start = 
        let mean = exp (Params.mu +. ((Params.sigma ** 2.) /. 2.)) in
        if constraints = [] then mean
        else closest mean constraints
      let to_burn = if true then 20000 else 0
    end)
end *)

module LogNormal
  (Params : sig val mu : float val sigma : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.sigma > 0.)
    include Sampler.Make_Inverse_Transform (struct
      type value = float
      let qf x = q_lognormal x ~mu:Params.mu ~sigma:Params.sigma
      let cdf x = q_lognormal x ~mu:Params.mu ~sigma:Params.sigma
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints l cdf
    end)
end

module Poisson 
  (Params : sig val lambda : float end) 
  (Constraints : sig val c : (int * int) list option end)
  : Sampler.S with type value = int = struct
    assert (Params.lambda >= 0.)
    include Sampler.Make_Inverse_Transform (struct
      type value = int
      let qf x = q_poisson x ~lambda:Params.lambda
      let cdf x = c_poisson (float x) ~lambda:Params.lambda
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints (make_inclusive l []) cdf
    end)
end

module Uniform 
  (Params : sig val a : float val b : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.a <= Params.b)
    include Sampler.Make_Inverse_Transform (struct
      type value = float
      let qf x = q_uniform x ~a:Params.a ~b:Params.b
      let cdf x = c_uniform x ~a:Params.a ~b:Params.b
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints l cdf
    end)
end


(* Imandra functions for defining univariate distributions *)

let bernoulli ~p ?(constraints=None) () =
  let p = Q.to_float p in
  let module Params = (struct let p = p end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Bernoulli(Params)(Consts) in
  D.sample_1 ()

let beta ~a ~b ?(constraints=None) () =
  let a, b, constraints = Q.to_float a, Q.to_float b, float_of constraints in
  let module Params = (struct let a = a let b = b end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Beta(Params)(Consts) in
  Q.of_float (D.sample_1 ())

let binomial ~n ~p ?(constraints=None) () =
  let n, p, constraints = Z.to_int n, Q.to_float p, int_of constraints in
  let module Params = (struct let n = n let p = p end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Binomial(Params)(Consts) in
  Z.of_int (D.sample_1 ())

let categorical (type a) ~classes ~probs ?(constraints=None) () =
  let probs = List.map Q.to_float probs in
  let module Params = (struct type t = a let classes = classes let probs = probs end) in
  let module Consts = (struct type t = a let c = constraints end) in
  let module D = Categorical(Params)(Consts) in
  D.sample_1 () 

let cauchy ~x_0 ~gamma ?(constraints=None) () =
  let x_0, gamma, constraints = Q.to_float x_0, Q.to_float gamma, float_of constraints in
  let module Params = (struct let x_0 = x_0 let gamma = gamma end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Cauchy(Params)(Consts) in
  Q.of_float (D.sample_1 ())

let exponential ~lambda ?(constraints=None) () =
  let lambda, constraints = Q.to_float lambda, float_of constraints in
  let module Params = (struct let lambda = lambda end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Exponential(Params)(Consts) in
  Q.of_float (D.sample_1 ())

let gamma ~k ~theta ?(constraints=None) () =
  let k, theta, constraints = Q.to_float k, Q.to_float theta, float_of constraints in
  let module Params = (struct let k = k let theta = theta end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Gamma(Params)(Consts) in
  Q.of_float (D.sample_1 ())

let gaussian ~mu ~sigma ?(constraints=None) () =
  let mu, sigma, constraints = Q.to_float mu, Q.to_float sigma, float_of constraints in
  let module Params = (struct let mu = mu let sigma = sigma end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Gaussian(Params)(Consts) in
  Q.of_float (D.sample_1 ())

let laplace ~mu ~b ?(constraints=None) () =
  let mu, b, constraints = Q.to_float mu, Q.to_float b, float_of constraints in
  let module Params = (struct let mu = mu let b = b end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Laplace(Params)(Consts) in
  Q.of_float (D.sample_1 ())

let logistic ~mu ~s ?(constraints=None) () =
  let mu, s, constraints = Q.to_float mu, Q.to_float s, float_of constraints in
  let module Params = (struct let mu = mu let s = s end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Logistic(Params)(Consts) in
  Q.of_float (D.sample_1 ())

let lognormal ~mu ~sigma ?(constraints=None) () =
  let mu, sigma, constraints = Q.to_float mu, Q.to_float sigma, float_of constraints in
  let module Params = (struct let mu = mu let sigma = sigma end) in
  let module Consts = (struct let c = constraints end) in
  let module D = LogNormal(Params)(Consts) in
  Q.of_float (D.sample_1 ())

let poisson ~lambda ?(constraints=None) () =
  let lambda, constraints = Q.to_float lambda, int_of constraints in
  let module Params = (struct let lambda = lambda end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Poisson(Params)(Consts) in
  Z.of_int (D.sample_1 ())

let uniform ~a ~b ?(constraints=None) () =
  let a, b, constraints = Q.to_float a, Q.to_float b, float_of constraints in
  let module Params = (struct let a = a let b = b end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Uniform(Params)(Consts) in
  Q.of_float (D.sample_1 ())


(* Imandra submodule for creating and sampling from models *)

module type Distribution = sig
  type domain
  val dist : unit -> domain
end

module type Make = sig
  type domain
  val get_indices : Imandra_surface.Decompose_region.t list -> (int * Imandra_surface.Decompose_region.t) list
  val get_probs : Imandra_surface.Decompose_region.t list -> ?n:Z.t -> unit -> (int, float) Hashtbl.t
  val query : (domain -> bool) -> ?n:Z.t -> unit -> float
  val save_samples : string -> Z.t -> unit
  val load_samples : string -> domain list
end

module Make (D : Distribution) : Make with type domain = D.domain = struct

  type domain = D.domain

  let get_indices regions =
    let indices, _ = Region_idx.indexer_for (module struct type args = domain end) regions in
    indices

  let get_probs regions ?(n=Z.of_int 10000) () =
    let indices, indexer = Region_idx.indexer_for (module struct type args = domain end) regions in
    let num_samples = Z.to_int n in
    let num_regions = List.length regions in
    let rec init_freqs rs f_tbl =
      match rs with
        | [] -> f_tbl
        | (i, _) :: t -> let () = Hashtbl.add f_tbl i 0. in init_freqs t f_tbl
    in let freqs = init_freqs indices (Hashtbl.create num_regions) in
    let rec loop num_remaining num_accepted fs =
      match num_remaining with
        | 0 -> let () = Hashtbl.filter_map_inplace (fun _ y -> Some (y /. float num_accepted)) fs in fs
        | m ->
          let i = 
            try indexer (D.dist ())
            with Not_found -> -1 in
          if i = -1 then
            loop (m - 1) (num_accepted) fs
          else
            let c = Hashtbl.find fs i in
            let () = Hashtbl.replace fs i (c +. 1.) in
            loop (m - 1) (num_accepted + 1) fs
    in loop num_samples 0 freqs

  let query condition ?(n=Z.of_int 10000) () =
    let rec loop num_remaining count =
      if num_remaining = 0 then float count /. Z.to_float n
      else if condition (D.dist ()) then loop (num_remaining - 1) (count + 1)
      else loop (num_remaining - 1) count
    in loop (Z.to_int n) 0
    
  let save_samples name n =
    let rec loop n' l =
    if n' = 0 then l
    else
      let s = D.dist () in
      loop (n' - 1) (s :: l)
    in let samples = loop (Z.to_int n) [] in
    let oc =  open_out name in
    Marshal.to_channel oc samples []; close_out oc

  let load_samples name =
    let ic = open_in name in
    (Marshal.from_channel ic : domain list)
    
  end