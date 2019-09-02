(* dists.ml - A module for sampling from a range of probability distributions parameterised/linearly constrained by the user
Lewis Hammond - lewis@imandra.ai
2019 *)


(* Constants *)

let pi = 4. *. (atan 1.)

let log_2_pi = log (2. *. pi) 

let max_int_bits = (int_of_float (2. ** 30.)) - 1

let bits_26 = (int_of_float (2. ** 26.))

let q0 = Q.zero


(* OCaml standard PRNG *)

module RS = Random.State

type st = RS.t

let default_seed = [| 19841983; 7298712389; 17862387612 |]

let prng = RS.make default_seed

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

module NC = Nocrypto.Rng

let () = Nocrypto_entropy_unix.initialize ()

let nc_bool () = 
  let b = NC.Int.gen_bits 1 in 
  if b = 0 then false else true

let nc_int ?(bound = max_int_bits) () = 
  let i = (NC.Int.gen bound) in
  if nc_bool () then i
  else -i

let nc_Z ?(bound = Z.of_int max_int_bits) () =
  let z = NC.Z.gen bound in
  if nc_bool () then z
  else Z.neg z
  
let nc_base ?(a = 0.) ?(b = 1.) () = 
  if a > b then 
	failwith "Cannot sample from base of negative range"
  else 
	let a = float_of_int (NC.Int.gen (pred bits_26)) in
    let b = float_of_int (NC.Int.gen (pred bits_26)) in
    let scale = float_of_int bits_26 in
    let base = (a /. scale +. b) /. scale in
    (b -. a) *. base +. a

let nc_float ?(bound = float_of_int max_int_bits) () = 
  let f = nc_base ~a:0. ~b:bound () in
  if nc_bool () then f
  else -.f
 
let nc_Q ?(bound = Q.of_int max_int_bits) () =
  let n = nc_Z () in
  let d = nc_Z () in
  Q.mul {num = n; den = d} bound


(* Primitive random element *)

(* let base ?(a = 0.) ?(b = 1.) () = rs_base prng ~a ~b () *)

let base ?(a = 0.) ?(b = 1.) () = nc_base ~a ~b ()


(* Helper functions *)

let bounded x = 0. <= x && x <= 1.

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
    if x < i then a
    else loop (i +. 1.) (a +. (log i))
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

let proposal_dist ~mu ~s x = 
  if x = 0. then -.infinity
  else if x = 1. then infinity
  else mu +. (s *. log (x /. (1. -. x)))
  
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
        if compare b a < 0 then failwith "Each constraint (a, b) must be such that b >= a"
        else if compare a new_b < 0 then failwith "Constraints must not overlap"
        else
		  let (c_a, c_b) = (cdf a, cdf b) in
		  let w = c_b -. c_a in
		  loop t ((c_a, c_b) :: u_rs) (w :: w_rs) (Some b)
  in let (u_rs, w_rs) = loop sorted [] [] None in
  if sum w_rs = 0. then failwith "Constrained regions have zero total probability mass"
  else u_rs, (normalise w_rs)

let process_constraints l =
  let rec to_float_list l_Q l_float =
    match l_Q with
      | [] -> l_float
      | (a, b) :: t -> to_float_list t ((Q.to_float a, Q.to_float b) :: l_float)
  in let sorted = List.sort constraint_comparison (to_float_list l []) in
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
    | (a, b) :: t -> make_inclusive t ((Z.pred a, b) :: c_inclusive)
  

(* Quantile functions *)

let q_bernoulli ~p x = 
  if x <= (1. -. p) then false else true

let q_binomial ~n ~p x =
  let rec get_successes k prob =
    let term = (choose n k) *. (p ** k) *. ((1. -. p) ** (n -. k)) in
    let new_term = term +. prob in
    if x <= new_term || k = n then Z.of_float k
    else get_successes (k +. 1.) new_term
  in if x = 1. then Z.of_float n else get_successes 0. 0.

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
  if x = 0. then Q.minus_inf
  else if x = 1. then Q.inf
  else Q.of_float (x_0 +. (gamma *. tan(pi *. (x -. 0.5))))

let q_exponential ~lambda x = 
  if x = 1. then Q.minus_inf
  else Q.of_float (-.(log (1. -. x)) /. lambda)

let q_laplace ~mu ~b x = 
  if x = 0. then Q.minus_inf
  else if x = 1. then Q.inf
  else if x <= 0.5 then Q.of_float (mu +. (b *. log (2. *. x)))
  else Q.of_float (mu -. (b *. log (2. -. (2. *. x))))

let q_logistic ~mu ~s x = 
  if x = 0. then Q.minus_inf
  else if x = 1. then Q.inf
  else Q.of_float (mu +. (s *. log (x /. (1. -. x))))

let q_poisson ~lambda x =
  let rec get_successes k prob =
    let log_term = -.lambda +. (k *. log lambda) -. log_factorial k in
    let term = exp log_term in
    if x <= term +. prob then Z.of_float k
    else get_successes (k +. 1.) (term +. prob)
  in if x = 1. then Z.of_int max_int
  else get_successes 0. 0.

let q_uniform ~a ~b x = 
  Q.of_float (a +. ((b -. a) *. x))


(* Cumulative density functions *)

let c_bernoulli ~p x = 
  if x then 1. else (1. -. p)

let c_binomial ~n ~p x =
  let x = Z.to_float x in
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
  let x = Q.to_float x in
  (1. /. pi) *. atan ((x -. x_0) /. gamma) +. 0.5

let c_exponential ~lambda x = 
  let x = Q.to_float x in
  if x < 0. then 0.
  else 1. -. exp (-.lambda *. x) 

let c_laplace ~mu ~b x = 
  let x = Q.to_float x in
  if x <= mu then 0.5 *. exp ((x -. mu) /. b)
  else 1. -. (0.5 *. exp (-. (x -. mu) /. b))

let c_logistic ~mu ~s x = 
  let x = Q.to_float x in
  1. /. (1. +. exp (-. (x -. mu) /. s))

let c_poisson ~lambda x =
  let x = Z.to_float x in
  let rec get_prob k prob =
    if k > x then prob
    else
      let log_term = -.lambda +. (k *. log lambda) -. log_factorial k in
      let term = exp log_term in
      get_prob (k +. 1.) (term +. prob)
  in get_prob 0. 0.

let c_uniform ~a ~b x =
  let x = Q.to_float x in
  if x < a then 0.
  else if x > b then 1.
  else (x -. a) /. (b -. a) 


(* Density functions *)

let d_beta ~a ~b x =
  (* let x = Q.to_float x in *)
  if x < 0. then failwith "Beta PDF is not defined for x < 0"
  else if x > 1. then failwith "Beta PDF is not defined for x > 1"
  else if a < 1. && x = 0. then infinity
  else if b < 1. && x = 1. then infinity
  else
    let z = (gamma a) *. (gamma b) /. gamma (a +. b) in
    (1. /. z) *. (x ** (a -. 1.)) *. ((1. -. x) ** (b -. 1.))

let d_gamma ~k ~theta x =
  (* let x = Q.to_float x in *)
  if x < 0. then failwith "Gamma PDF is not defined for x < 0"
  else if k < 1. && x = 0. then infinity
  else 
    let z = (gamma k) *. (theta ** k) in
    (1. /. z) *. (x ** (k -. 1.)) *. (exp (-.x /. theta))

let d_gaussian ~mu ~sigma x =
  (* let x = Q.to_float x in *)
  let z = sigma *. sqrt (2. *. pi) in
  let e = ((x -. mu) ** 2.) /. (2. *. (sigma ** 2.)) in
  (1. /. z) *. exp (-.e)

let d_lognormal ~mu ~sigma x =
  (* let x = Q.to_float x in *)
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
    let new_x = proposal_dist ~mu:cur_x ~s:step (base ()) in
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


(* Sampler functor and related types *)

module type Inverse_Transform_S = sig
  type value
  val qf : float -> value
  val cdf : value -> float
  val constraints : (float * float) list * float list
end

module type MCMC_S = sig
  type value
  val pdf : float -> float
  val constraints : (float * float) list
  val bounds : float * float
  val gradient : float
  val start : float
  val step : float
  val to_burn : int
end

module Sampler = struct
  module type S = sig
    type value
    val sample : ?start:float -> ?step:float -> int -> value list
  end
  module Make_Inverse_Transform (D : Inverse_Transform_S) : S with type value = D.value = struct
    type value = D.value
    let sample ?start:_ ?step:_ n =
      let rec loop samples num =
        match num with
          | 0 -> samples
          | _ -> 
            let s = inverse_transform_sample D.qf D.constraints in 
            loop (s :: samples) (num - 1)
      in loop [] n
  end
  module Make_MCMC (D : MCMC_S) : S with type value = Q.t = struct
    type value = Q.t
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
              (* print_string ("Rate: " ^ string_of_float rate ^ "\n"); *)
              (* print_string ("Step: " ^ string_of_float b_step ^ " --> " ^ string_of_float (new_step) ^ "\n"); *)
              burn (b_num - 1) s new_step 0. 0.
            else
              burn (b_num - 1) s b_step a' r'
    let burn_in_step = burn D.to_burn D.start D.step 0. 0.
    let sample ?(start = D.start) ?(step = burn_in_step) n =
      let rec loop samples num last a r =
        match num with
          | 0 -> samples
          | _ -> 
            let s = mcmc_sample D.pdf D.constraints D.bounds D.gradient last step in
            if s = last then loop (Q.of_float s :: samples) (num - 1) s a (r + 1)
            else loop (Q.of_float s :: samples) (num - 1) s (a + 1) r
      in loop [] n start 0 0
    end
end


(* Distribution modules *)

module Bernoulli 
  (Params : sig val p : Q.t end) 
  (Constraints : sig val c : bool option end)
  : Sampler.S with type value = bool = struct
    let p = Q.to_float Params.p in
    assert (bounded p)
    include Sampler.Make_Inverse_Transform (struct
      type value = bool
      let p = Q.to_float Params.p
      let qf = q_bernoulli ~p
      let cdf = c_bernoulli ~p
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some b -> if b then [(1., 1.)], [1.] else [(0., 0.)], [1.]
    end)
end

module Beta 
  (Params : sig val a : Q.t val b : Q.t end) 
  (Constraints : sig val c : (Q.t * Q.t) list option end)
  : Sampler.S with type value = Q.t = struct
    let a, b = Q.to_float Params.a, Q.to_float Params.b in
    assert (a > 0. &&
            b > 0.)
    include Sampler.Make_MCMC (struct
      type value = Q.t
      let a, b = Q.to_float Params.a, Q.to_float Params.b
      let pdf = d_beta ~a ~b
      let constraints, bounds, gradient = 
        match Constraints.c with
          | None -> [], (0., 1.), 0.
          | Some l -> process_constraints l
      let step =
        let lower, upper = bounds in 
        0.2 *. (upper -. lower)
      let start = 
        let mean = a /. (a +. b) in
        if constraints = [] then mean
        else closest mean constraints
      let to_burn = if true then 20000 else 0
    end)
end

module Binomial 
  (Params : sig val n : Z.t val p : Q.t end) 
  (Constraints : sig val c : (Z.t * Z.t) list option end)
  : Sampler.S with type value = Z.t = struct
    let p, n = Q.to_float Params.p, Z.to_int Params.n in
    assert (bounded p &&
            n >= 0)
    include Sampler.Make_Inverse_Transform (struct
      type value = Z.t
      let p, n = Q.to_float Params.p, Z.to_float Params.n
      let qf = q_binomial ~n ~p
      let cdf = c_binomial ~n ~p
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints (make_inclusive l []) cdf
    end)
end

module Categorical 
  (Params : sig type t val classes : t list val probs : Q.t list end) 
  (Constraints : sig type t = Params.t val c : t list option end)
  : Sampler.S with type value = Params.t = struct
    let classes, probs = Params.classes, List.map Q.to_float Params.probs in
    assert (classes <> [] &&
            Constraints.c <> Some [] &&
            List.length classes = List.length probs &&
            List.for_all bounded probs &&
            sum probs = 1.)
    include Sampler.Make_Inverse_Transform (struct
      type value = Params.t
      let cs, ps = Params.classes, List.map Q.to_float Params.probs
      let constraints = [], []
      let classes, probs =
        match Constraints.c with
          | None -> cs, ps
          | Some l -> constrain_categorical l (cs, ps) 
      let qf = q_categorical ~classes ~probs
      let cdf = c_categorical ~classes ~probs
    end)
end

module Cauchy 
  (Params : sig val x_0 : Q.t val gamma : Q.t end) 
  (Constraints : sig val c : (Q.t * Q.t) list option end)
  : Sampler.S with type value = Q.t = struct
    let gamma = Q.to_float Params.gamma in
    assert (gamma > 0.)
    include Sampler.Make_Inverse_Transform (struct
      type value = Q.t
      let x_0, gamma = Q.to_float Params.x_0, Q.to_float Params.gamma
      let qf = q_cauchy ~x_0 ~gamma
      let cdf = c_cauchy ~x_0 ~gamma
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints l cdf
    end)
end

module Exponential 
  (Params : sig val lambda : Q.t end) 
  (Constraints : sig val c : (Q.t * Q.t) list option end)
  : Sampler.S with type value = Q.t = struct
    let lambda = Q.to_float Params.lambda in
    assert (lambda > 0.)
    include Sampler.Make_Inverse_Transform (struct
      type value = Q.t
      let lambda = Q.to_float Params.lambda
      let qf = q_exponential ~lambda
      let cdf = c_exponential ~lambda
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints l cdf
    end)
end

module Gamma 
  (Params : sig val k : Q.t val theta : Q.t end) 
  (Constraints : sig val c : (Q.t * Q.t) list option end)
  : Sampler.S with type value = Q.t = struct
    let k, theta = Q.to_float Params.k, Q.to_float Params.theta in
    assert (k > 0. 
            && theta > 0.)
    include Sampler.Make_MCMC (struct
      type value = Q.t
      let k, theta = Q.to_float Params.k, Q.to_float Params.theta
      let pdf = d_gamma ~k ~theta
      let constraints, bounds, gradient = 
        match Constraints.c with
          | None -> [], (0., max_float), 0.
          | Some l -> process_constraints l
      let step =
        let std = theta *. sqrt k in
        let lower, upper = bounds in
        if constraints = [] then 2.5 *. std
        else min (0.2 *. (upper -. lower)) (2.5 *. std)
      let start = 
        let mean = k *. theta in
        if constraints = [] then mean
        else closest mean constraints
      let to_burn = if true then 20000 else 0
    end)
end

module Gaussian
  (Params : sig val mu : Q.t val sigma : Q.t end) 
  (Constraints : sig val c : (Q.t * Q.t) list option end)
  : Sampler.S with type value = Q.t = struct
    let sigma = Q.to_float Params.sigma in
    assert (sigma > 0.)
    include Sampler.Make_MCMC (struct
      type value = Q.t
      let mu, sigma = Q.to_float Params.mu, Q.to_float Params.sigma
      let pdf = d_gaussian ~mu ~sigma
      let constraints, bounds, gradient = 
        match Constraints.c with
          | None -> [], (min_float, max_float), 0.
          | Some l -> process_constraints l
      let step =
        let std = sigma in
        let lower, upper = bounds in
        if constraints = [] then 2.5 *. std
        else min (0.25 *. (upper -. lower)) (2.5 *. std)
      let start = 
        let mean = mu in
        if constraints = [] then mean
        else closest mean constraints
      let to_burn = 
        if true then 20000 else 0
    end)
end

module Laplace 
  (Params : sig val mu : Q.t val b : Q.t end) 
  (Constraints : sig val c : (Q.t * Q.t) list option end)
  : Sampler.S with type value = Q.t = struct
    let b = Q.to_float Params.b in
    assert (b > 0.)
    include Sampler.Make_Inverse_Transform (struct
      type value = Q.t
      let mu, b = Q.to_float Params.mu, Q.to_float Params.b
      let qf = q_laplace ~mu ~b
      let cdf = c_laplace ~mu ~b
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints l cdf
    end)
end

module Logistic 
  (Params : sig val mu : Q.t val s : Q.t end) 
  (Constraints : sig val c : (Q.t * Q.t) list option end)
  : Sampler.S with type value = Q.t = struct
    let s = Q.to_float Params.s in
    assert (s > 0.)
    include Sampler.Make_Inverse_Transform (struct
      type value = Q.t
      let mu, s = Q.to_float Params.mu, Q.to_float Params.s
      let qf = q_logistic ~mu ~s
      let cdf = c_logistic ~mu ~s
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints l cdf
    end)
end

module LogNormal
  (Params : sig val mu : Q.t val sigma : Q.t end) 
  (Constraints : sig val c : (Q.t * Q.t) list option end)
  : Sampler.S with type value = Q.t = struct
    let sigma = Q.to_float Params.sigma in
    assert (sigma > 0.)
    include Sampler.Make_MCMC (struct
      type value = Q.t
      let mu, sigma = Q.to_float Params.mu, Q.to_float Params.sigma
      let pdf = d_lognormal ~mu ~sigma
      let constraints, bounds, gradient = 
        match Constraints.c with
          | None -> [], (0., max_float), 0.
          | Some l -> process_constraints l
      let step =
        let std = ((exp (sigma ** 2.)) -. 1.) *. exp ((2. *. mu) +. (sigma ** 2.)) in
        let lower, upper = bounds in
        if constraints = [] then 2.5 *. std
        else min (0.25 *. (upper -. lower)) (2.5 *. std)
      let start = 
        let mean = exp (mu +. ((sigma ** 2.) /. 2.)) in
        if constraints = [] then mean
        else closest mean constraints
      let to_burn = if true then 20000 else 0
    end)
end

module Poisson 
  (Params : sig val lambda : Q.t end) 
  (Constraints : sig val c : (Z.t * Z.t) list option end)
  : Sampler.S with type value = Z.t = struct
    let lambda = Q.to_float Params.lambda in
    assert (lambda >= 0.)
    include Sampler.Make_Inverse_Transform (struct
      type value = Z.t
      let lambda = Q.to_float Params.lambda
      let qf = q_poisson ~lambda
      let cdf = c_poisson ~lambda
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints (make_inclusive l []) cdf
    end)
end

module Uniform 
  (Params : sig val a : Q.t val b : Q.t end) 
  (Constraints : sig val c : (Q.t * Q.t) list option end)
  : Sampler.S with type value = Q.t = struct
    let a, b = Q.to_float Params.a, Q.to_float Params.b in
    assert (a <= b)
    include Sampler.Make_Inverse_Transform (struct
      let a, b = Q.to_float Params.a, Q.to_float Params.b
      type value = Q.t
      let qf = q_uniform ~a ~b
      let cdf = c_uniform ~a ~b
      let constraints = 
        match Constraints.c with
          | None -> [], []
          | Some l -> get_uniform_constraints l cdf
    end)
end