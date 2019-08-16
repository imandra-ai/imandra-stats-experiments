(* dists.ml - A module for sampling from a range of probability distributions parameterised/linearly constrained by the user
Lewis Hammond - lewis@imandra.ai
2019 *)


(* Necessary modules *)

module RS = Random.State


(* Constants *)

let pi = 4. *. atan 1.

let log_2_pi = log (2. *. pi) 


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

let get_uniform_constraints rs cdf =
  let rec loop rs u_rs w_rs =
	match rs with
	  | [] -> (u_rs, w_rs)
	  | (a, b) :: t -> 
		let (c_a, c_b) = (cdf a, cdf b) in
		let w = c_b -. c_a in
		loop t ((c_a, c_b) :: u_rs) (w :: w_rs)
  in let (u_rs, w_rs) = loop rs [] [] in
  if sum w_rs = 0. then failwith "Constrained regions have zero total probability mass"
  else u_rs, (normalise w_rs)

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

let rec get_domain_size c d =
  match c with
    | [] -> d
    | (a, b) :: t -> get_domain_size t ((b -. a) +. d)

let rec make_inclusive c c_inclusive =
  match c with
    | [] -> c_inclusive
    | (a, b) :: t -> make_inclusive t ((a - 1, b) :: c_inclusive)
  

(* Primitive random elements *)

let rand_bool () = Random.bool ()

let rand_int ?(bound = max_int) () = 
  let i = Random.int bound in
  if rand_bool () then i
  else -i

(* let rand_Z ?(bound = max_int) () = 
  let i = rand_int ~bound () in Z.of_int i *)

let rand_float ?(bound = max_float) () =
  let f = Random.float bound in
  if rand_bool () then f
  else -.f

(* let rand_Q ?(bound = max_float) () =
  let f = rand_float ~bound () in Q.of_float f *)

let base ?(a = 0.) ?(b = 1.) () = 
  if a <= b then a +. Random.float (b -. a)
  else failwith "Cannot sample from base of negative range"


(* Quantile functions *)

let q_bernoulli x ~p = 
  if x <= (1. -. p) then false else true

let q_binomial x ~n ~p =
  let rec get_successes k prob =
    let term = (choose n k) *. (p ** k) *. ((1. -. p) ** (n -. k)) in
    let new_term = term +. prob in
    if x <= new_term || k = n then int_of_float k
    else get_successes (k +. 1.) new_term
  in if x = 1. then int_of_float n else
  get_successes 0. 0.

let q_categorical x ~classes ~probs =
  let rec loop prob cs ps =
    match cs, ps with
      | [], _ -> failwith "Must have same number of classes as probabilities"
      | _, [] -> failwith "Must have same number of classes as probabilities"
      | c :: _cs, p :: _ps -> 
        let new_prob = p +. prob in
        if x <= new_prob || new_prob = 1. then c
        else loop new_prob _cs _ps
  in loop 0. classes probs

let q_cauchy x ~x_0 ~gamma =
  if x = 0. then -.infinity
  else if x = 1. then infinity
  else x_0 +. (gamma *. tan(pi *. (x -. 0.5)))

let q_exponential x ~lambda = 
  if x = 1. then -.infinity
  else -.(log (1. -. x)) /. lambda

let q_laplace x ~mu ~b = 
  if x = 0. then -.infinity
  else if x = 1. then infinity
  else if x <= 0.5 then mu +. (b *. log (2. *. x))
  else mu -. (b *. log (2. -. (2. *. x)))

let q_logistic x ~mu ~s = 
  if x = 0. then -.infinity
  else if x = 1. then infinity
  else mu +. (s *. log (x /. (1. -. x)))

let q_poisson x ~lambda =
  let rec get_successes k prob =
    let log_term = -.lambda +. (k *. log lambda) -. log_factorial k in
    let term = exp log_term in
    if x <= term +. prob then int_of_float k
    else get_successes (k +. 1.) (term +. prob)
  in if x = 1. then max_int
  else get_successes 0. 0.

let q_uniform x ~a ~b = 
  a +. ((b -. a) *. x)


(* Cumulative density functions *)

let c_bernoulli x ~p = 
  if x then 1. else (1. -. p)

let c_binomial x ~n ~p =
  let rec get_prob k prob =
    if k > x then prob
    else
      let term = (choose n k) *. (p ** k) *. ((1. -. p) ** (n -. k)) in
      get_prob (k +. 1.) (term +. prob)
  in if x > n then 1.0
  else get_prob 0. 0.

let c_categorical x ~classes ~probs =
  let rec loop prob cs ps =
    match cs, ps with
      | [], _ -> failwith "Must have same number of classes as probabilities"
      | _, [] -> failwith "Must have same number of classes as probabilities"
      | c :: _cs, p :: _ps -> 
        if c = x then (p +. prob)
        else loop (p +. prob) _cs _ps
  in loop 0. classes probs

let c_cauchy x ~x_0 ~gamma = 
  (1. /. pi) *. atan ((x -. x_0) /. gamma) +. 0.5

let c_exponential x ~lambda = 
  if x < 0. then 0.
  else 1. -. exp (-.lambda *. x) 

let c_laplace x ~mu ~b = 
  if x <= mu then 0.5 *. exp ((x -. mu) /. b)
  else 1. -. (0.5 *. exp (-. (x -. mu) /. b))

let c_logistic x ~mu ~s = 
  1. /. (1. +. exp (-. (x -. mu) /. s))

let c_poisson x ~lambda =
  let rec get_prob k prob =
    if k > x then prob
    else
      let log_term = -.lambda +. (k *. log lambda) -. log_factorial k in
      let term = exp log_term in
      get_prob (k +. 1.) (term +. prob)
  in get_prob 0. 0.

let c_uniform x ~a ~b =
  if x < a then 0.
  else if x > b then 1.
  else (x -. a) /. (b -. a) 


(* Density functions *)

let d_beta x ~a ~b =
  if x < 0. then failwith "Beta PDF is not defined for x < 0"
  else if x > 1. then failwith "Beta PDF is not defined for x > 1"
  else if a < 1. && x = 0. then infinity
  else if b < 1. && x = 1. then infinity
  else
    let z = (gamma a) *. (gamma b) /. gamma (a +. b) in
    (1. /. z) *. (x ** (a -. 1.)) *. ((1. -. x) ** (b -. 1.))

let d_gamma x ~k ~theta =
  if x < 0. then failwith "Gamma PDF is not defined for x < 0"
  else if k < 1. && x = 0. then infinity
  else 
    let z = (gamma k) *. (theta ** k) in
    (1. /. z) *. (x ** (k -. 1.)) *. (exp (-.x /. theta))

let d_gaussian x ~mu ~sigma =
  let z = sigma *. sqrt (2. *. pi) in
  let e = ((x -. mu) ** 2.) /. (2. *. (sigma ** 2.)) in
  (1. /. z) *. exp (-.e)

let d_lognormal x ~mu ~sigma =
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
    
let mcmc_sample pdf bounds constraints cur_x step =
  let (lower, upper) = bounds in
  let rec apply_constraints rs (a, b) p =
    match rs with
      | (c, d) :: t -> 
        if p < c then 
          match a, b with
            | _, Some f_b ->
              let q = c +. (p -. f_b) in
              apply_constraints rs (a, b) q
            | _, _ -> None
        else if p <= d then
          Some p
        else 
          apply_constraints t (Some c, Some d) p
      | _ -> None 
  in let rec get_proposal () =
    let epsilon = base ~a:(-.step) ~b:step () in
    let new_x = cur_x +. epsilon in
    if new_x < lower || new_x > upper then
      get_proposal ()
    else if constraints = [] then
      new_x
    else
      let result = apply_constraints constraints (None, None) new_x in
      match result with
        | None -> get_proposal ()
        | Some f -> f
  in let new_x = get_proposal () in
  let ratio = (pdf new_x) /. (pdf cur_x) in
  if base () <= ratio then new_x else cur_x


(* Sampler functor and related types *)

type st = RS.t

module type Inverse_Transform_S = sig
  type value
  val qf : float -> value
  val cdf : value -> float
  val constraints : (float * float) list * float list
end

module type MCMC_S = sig
  type value
  val pdf : float -> float
  val bounds : float * float
  val constraints : (float * float) list
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
  module Make_MCMC (D : MCMC_S) : S with type value = float = struct
    type value = float
    let batch = D.to_burn / 10
    let rec burn b_num b_last b_step a r =
        match b_num with
          | 0 -> b_step
          | _ -> 
            let s = mcmc_sample D.pdf D.bounds D.constraints b_last b_step in 
            let a', r' = if s = b_last then a, r +. 1. else a +. 1., r in
            if (b_num - 1) mod batch = 0 then
              let rate = a' /. (a' +. r') in
              let diff = 1. +. (rate -. (1./.3.)) in

              print_string ("Rate: " ^ string_of_float rate ^ "\n");
              print_string ("Step: " ^ string_of_float b_step ^ " --> " ^ string_of_float (diff *. b_step) ^ "\n");

              burn (b_num - 1) s (diff *. b_step) 0. 0.
            else
              burn (b_num - 1) s b_step a' r'
    let burn_in_step = burn D.to_burn D.start D.step 0. 0.
    let sample ?(start = D.start) ?(step = burn_in_step) n =
      let rec loop samples num last a r =
        match num with
          | 0 -> samples
          | _ -> 
            let s = mcmc_sample D.pdf D.bounds D.constraints last step in 
            if s = last then loop (s :: samples) (num - 1) s a (r + 1)
            else loop (s :: samples) (num - 1) s (a + 1) r
      in loop [] n start 0 0
    end
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

module Beta 
  (Params : sig val a : float val b : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.a > 0. &&
            Params.b > 0.)
    include Sampler.Make_MCMC (struct
      type value = float
      let pdf x = d_beta x ~a:Params.a ~b:Params.b
      let bounds = (0., 1.)
      let constraints = 
        match Constraints.c with
          | None -> []
          | Some l -> l
      let step =
        (* let std = sqrt ((Params.a *. Params.b) /. ((Params.a +. Params.b) ** 2.) *. (Params.a +. Params.b +. 1.)) in *)
        (* 5. *. std *)
        if constraints = [] then
          0.2
        else
          1. *. get_domain_size constraints 0.
      let start = 
        let mean = Params.a /. (Params.a +. Params.b) in
        if constraints = [] then mean
        else closest mean constraints
      let to_burn = if constraints = [] then 10000 else 0
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

module Gamma 
  (Params : sig val k : float val theta : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.k > 0. 
            && Params.theta > 0.)
    include Sampler.Make_MCMC (struct
      type value = float
      let pdf x = d_gamma x ~k:Params.k ~theta:Params.theta
      let bounds = (0., max_float)
      let constraints = 
        match Constraints.c with
          | None -> []
          | Some l -> l
      let step =
        if constraints = [] then
          let std = Params.theta *. sqrt Params.k in
          5. *. std
        else
          0.2 *. get_domain_size constraints 0.
      let start = 
        let mean = Params.k *. Params.theta in
        if constraints = [] then mean
        else closest mean constraints
      let to_burn = if constraints = [] then 10000 else 0
    end)
end

module Gaussian
  (Params : sig val mu : float val sigma : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.sigma > 0.)
    include Sampler.Make_MCMC (struct
      type value = float
      let pdf x = d_gaussian x ~mu:Params.mu ~sigma:Params.sigma
      let bounds = (min_float, max_float)
      let constraints = 
        match Constraints.c with
          | None -> []
          | Some l -> l
      let step =
        if constraints = [] then
          let std = Params.sigma in
          5. *. std
        else
          0.2 *. get_domain_size constraints 0.
      let start = 
        let mean = Params.mu in
        if constraints = [] then mean
        else closest mean constraints
      let to_burn = 
        if constraints = [] then 10000 else 0
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

module LogNormal
  (Params : sig val mu : float val sigma : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.sigma > 0.)
    include Sampler.Make_MCMC (struct
      type value = float
      let pdf x = d_lognormal x ~mu:Params.mu ~sigma:Params.sigma
      let bounds = (0., max_float)
      let constraints = 
        match Constraints.c with
          | None -> []
          | Some l -> l
      let step = 
        if constraints = [] then
          let std = ((exp (Params.sigma ** 2.)) -. 1.) *. exp ((2. *. Params.mu) +. (Params.sigma ** 2.)) in
          5. *. std
        else
          0.5 *. get_domain_size constraints 0.
      let start = 
        let mean = exp (Params.mu +. ((Params.sigma ** 2.) /. 2.)) in
        if constraints = [] then mean
        else closest mean constraints
      let to_burn = if constraints = [] then 10000 else 0
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