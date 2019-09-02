(* utils.ml - Utility functions for sampling from distributions 
Lewis Hammond - lewis@imandra.ai
2019 *)


(* Necessary modules *)

module RS = Random.State

module NC = Nocrypto.Rng

module GSL = Gsl.Rng

module D = Gsl.Cdf


(* Constants *)

let pi = 4. *. (atan 1.)

let log_2_pi = log (2. *. pi) 

let max_int_bits = (int_of_float (2. ** 30.)) - 1

let bits_26 = (int_of_float (2. ** 26.))

let q0 = Q.zero


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
  Q.mul {num = n; den = d} bound

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
  Q.mul {num = n; den = d} bound


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
  Q.mul {num = n; den = d} bound


(* Primitive random element *)

let base ?(a = 0.) ?(b = 1.) () = rs_base rs_prng ~a ~b ()

let base ?(a = 0.) ?(b = 1.) () = nc_base nc_prng ~a ~b ()

let base ?(a = 0.) ?(b = 1.) () = gsl_base gsl_prng ~a ~b ()


(* Helper functions *)

let bounded x = x >= 0. && x <= 1.

let sum l = List.fold_left (+.) 0. l

let sum_n n l =
  let rec loop l' m s =
    match m with
      | 0 -> s
      | _ -> 
        match l' with
          | [] -> s
          | h :: t -> loop t (m - 1) (s +. h)
  in loop l n 0.
 
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

let log_nemes_closed_form x =
  let log_x = log x in
  let fifteen_x_sq = 15. *. (x ** 2.) in
  (x *. log_x) -. x +. (0.5 *. (log_2_pi -. log_x)) +. (1.25 *. x *. (log (fifteen_x_sq +. 1.) -. log (fifteen_x_sq)))
  
let gamma x = (exp (log_nemes_closed_form (x +. 1.))) /. x
  
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

let process_constraints l =
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
  in loop 0. 0. cs

let rec make_inclusive c c_inclusive =
  match c with
    | [] -> c_inclusive
    | (a, b) :: t -> make_inclusive t ((a - 1, b) :: c_inclusive)


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

let d_beta ~a ~b x =
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
    (1. /. z) *. exp (-.e)


(* Sampling algorithms *)

let inverse_transform_sample qf (u_rs, n_w_rs) =
  match u_rs, n_w_rs with
	| [], [] -> qf (base ())
	| _, _ -> let (r1, r2) = q_categorical (base ()) ~classes:u_rs ~probs:n_w_rs in
              base ~a:r1 ~b:r2 () |> qf
    
let mcmc_sample pdf constraints bounds gradient cur_x step =
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
    (* let epsilon = base ~a:(-.step) ~b:step () in *)
    (* let new_x = cur_x +. epsilon in *)
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
  if base () <= ratio then new_x else cur_x