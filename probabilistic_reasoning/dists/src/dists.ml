(* dists.ml - A module for sampling from a range of probability distributions parameterised by the user
Lewis Hammond - lewis@imandra.ai
2019 *)


(* Necessary modules *)

module RS = Random.State


(* Constants *)

let pi = 4. *. atan 1.


(* Helper functions *)

let bounded x = x >= 0. && x <= 1.

let rec sum l =
  match l with
    | [] -> 0.
    | h :: t -> h +. (sum t)
 
let normalise l =
  let s = sum l in
  let rec div n m =
    match n with
      | [] -> m
      | h :: t -> div t ((h /. s) :: m)
  in div l []

let rec log_factorial x =
  let rec loop i a =
    if i > x then a
    else loop (i +. 1.) (a +. log i)
  in loop 1. 0.
  
let choose n k =
  let rec loop i j a =
    if j = 0. then a
    else loop (i -. 1.) (j -. 1.) (a *. (i /. j))
  in loop n k 1.

let gamma _ = failwith "TODO"

let constrain_categorical (constraints) (classes, probs) =
  if constraints = [] then 
    classes, probs
  else
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
	new_c, (normalise new_p)

let get_uniform_constraints rs cdf =
  let rec loop rs u_rs w_rs =
	match rs with
	  | [] -> (u_rs, w_rs)
	  | (a, b) :: t -> 
		let (c_a, c_b) = (cdf a, cdf b) in
		let w = c_b -. c_a in
		loop t ((c_a, c_b) :: u_rs) (w :: w_rs)
  in let (u_rs, w_rs) = loop rs [] [] in
  let n_w_rs = normalise w_rs in
  u_rs, n_w_rs
  

(* Primitive random elements *)

let rand_bool = 
  Random.bool ()

let rand_int ?(bound = max_int) () = 
  let i = Random.int bound in
  if rand_bool then i
  else -i

let rand_Z ?(bound = max_int) () = 
  let i = rand_int ~bound () in Z.of_int i

let rand_float ?(bound = max_float) () =
  let f = Random.float bound in
  if rand_bool then f
  else -.f

let rand_Q ?(bound = max_float) () =
  let f = rand_float ~bound () in Q.of_float f

let base ?(a = 0.) ?(b = 1.) () = 
  let () = assert (a <= b) in
  a +. Random.float (b -. a)


(* Quantile functions *)

let q_bernoulli x ~p:p = 
  if x < p then true else false

let q_binomial x ~n:n ~p:p =
  let rec get_successes k prob =
    let term = (choose n k) *. (p ** k) *. ((1.0 -. p) ** (n -. 1.0)) in
    if x <= term +. prob then int_of_float k
    else get_successes (k +. 1.) (term +. prob)
  in get_successes 0. 0.

let q_categorical x ~classes:classes ~probs:probs =
  let rec loop prob cs ps =
    match cs, ps with
      | [], _ -> failwith "Must have same number of classes as probabilities"
      | _, [] -> failwith "Must have same number of classes as probabilities"
      | c :: _cs, p :: _ps -> 
        if x <= p +. prob then c
        else loop (p +. prob) _cs _ps
  in loop 0. classes probs

let q_cauchy x ~x_0:x_0 ~gamma:gamma = 
  x_0 +. (gamma *. tan(pi *. (x -. 0.5)))

let q_exponential x ~lambda:lambda = 
  -.(log (1.0 -. x)) /. lambda

let q_laplace x ~mu:mu ~b:b = 
  if x <= 0.5 then
    mu +. (b *. log (2.0 *. x))
  else
    mu -. (b *. log (2.0 -. (2.0 *. x)))

let q_logistic x ~mu:mu ~s:s = 
  mu +. (s *. log (x /. (1.0 -. x)))

let q_poisson x ~lambda:lambda =
  let rec get_successes k prob =
    let log_term = -.lambda +. (k *. log lambda) -. log_factorial k in
    let term = exp log_term in
    if x <= term +. prob then int_of_float k
    else get_successes (k +. 1.) (term +. prob)
  in get_successes 0. 0.

let q_uniform x ~a:a ~b:b = 
  a +. ((b -. a) *. x)


(* Cumulative density functions *)

let c_bernoulli x ~p:p = 
  if x then p else (1. -. p)

let c_binomial x ~n:n ~p:p =
  let rec get_prob k prob =
    if k > x then 
      prob
    else
      let term = (choose n k) *. (p ** k) *. ((1.0 -. p) ** (n -. 1.0)) in
      get_prob (k +. 1.) (term +. prob)
  in get_prob 0. 0.

let c_categorical x ~classes:classes ~probs:probs =
  let rec loop prob cs ps =
    match cs, ps with
      | [], _ -> failwith "Must have same number of classes as probabilities"
      | _, [] -> failwith "Must have same number of classes as probabilities"
      | c :: _cs, p :: _ps -> 
        if c = x then prob
        else loop (p +. prob) _cs _ps
  in loop 0. classes probs

let c_cauchy x ~x_0:x_0 ~gamma:gamma = 
  (1. /. pi) *. atan ((x -. x_0) /. gamma) +. 0.5

let c_exponential x ~lambda:lambda = 
  1. -. exp (-.lambda *. x) 

let c_laplace x ~mu:mu ~b:b = 
  if x <= mu then
    0.5 *. exp ((x -. mu) /. b)
  else
    1. -. (0.5 *. exp (-. (x -. mu) /. b))

let c_logistic x ~mu:mu ~s:s = 
  1. /. (1. +. exp (-. (x -. mu) /. s))

let c_poisson x ~lambda:lambda =
  let rec get_prob k prob =
    if k > x then 
      prob
    else
      let log_term = -.lambda +. (k *. log lambda) -. log_factorial k in
      let term = exp log_term in
      get_prob (k +. 1.) (term +. prob)
  in get_prob 0. 0.

let c_uniform x ~a:a ~b:b =
  if x < a then
    0.
  else if x > b then
    1.
  else 
    (x -. a) /. (b -. a) 


(* Define density functions for MCMC sampling *)

let d_beta x ~a:a ~b:b =
  let z = (gamma a) *. (gamma b) /. gamma (a +. b) in
  (1. /. z) *. (x ** (a -. 1.)) *. ((1. -. x) ** (b -. 1.))

let d_gamma x ~k:k ~theta:theta =
  let z = (gamma k) *. (theta ** k) in
  (1. /. z) *. (x ** (k -. 1.)) *. (exp (-.x /. theta))

let d_gaussian x ~mu:mu ~sigma:sigma =
  let z = sigma *. sqrt (2. *. pi) in
  let e = ((x -. mu) /. (2. *. sigma)) ** 2.
  in (1. /. z) *. exp (-.e)

let d_lognormal x ~mu:mu ~sigma:sigma =
  exp (d_gaussian x ~mu:mu ~sigma:sigma)


(* Define sampling functions *)

let inverse_transform_sample qf (u_rs, n_w_rs) =
  match u_rs, n_w_rs with
	| [], [] -> qf (base ())
	| _, _ -> let (r1, r2) = q_categorical (base ()) ~classes:u_rs ~probs:n_w_rs in
              base ~a:r1 ~b:r2 () |> qf
    
let mcmc_sample pdf constraints cur_x step =
  let rec apply_constraints rs prev p =
    let (a, b) = prev in
    match rs with
      | h :: t -> 
        let (c, d) = h in
        if p < c then 
          match a, b with
            | _, Some f_b ->
              let q = c +. (p -. f_b) in
              apply_constraints rs prev q
            | _, _ -> None
        else if p <= d then
          Some p
        else 
          apply_constraints t (Some c, Some d) p
      | _ -> None 
    in
  let rec get_proposal () =
    let epsilon = base ~a:(-.step) ~b:step () in
    let new_x = cur_x +. epsilon in
    if constraints = [] then
      new_x
    else
      let result = apply_constraints constraints (None, None) new_x in
      match result with
        | None -> get_proposal ()
        | Some f -> f 
      in
  let new_x = get_proposal () in
  let ratio = (pdf new_x) /. (pdf cur_x) in
  let u = base () in
  if u <= ratio then new_x else cur_x


(* Define required variable types *)
(* 
type st = RS.t

type 'a consts =
  | NumConst of ('a * 'a) list
  | CatConst of 'a list

type 'a params =
  | NumParam of float list
  | CatParam of ('a list) * (float list)

type 'a dists =
  | Categorical of {qf: float -> ('a list) * (float list) -> 'a; cdf: 'a -> ('a list) * (float list) -> float}
  | InverseTransform of {qf: float -> float list -> 'a; cdf: 'a -> float list -> float}
  | MCMC of {pdf: 'a -> float params -> float}


(* Define module types *)

module type Dist_Type = sig
  type t
  val d : t dists
end

module type Params_Type = sig
  type t
  val p : t params
end

module type Consts_Type = sig
  type t
  val c : t consts
end

module type Sampler_Type = sig
  type t
  val dists : t dists
  val params : t params
  val constraints : t consts
  val sample : int -> t list
end


(* Define functions for forming first-class parameter and constraint modules *)

let params (type a) (param_list : a params) =
  let module P : Params_Type = (struct
    type t = a
    let p = param_list
  end) in (module P : Params_Type)

let constraints (type a) (constraints_list : a consts) =
  let module C : Consts_Type = (struct
    type t = a
    let c = constraints_list
  end) in (module C : Consts_Type) *)


(* Sampler functor and related types *)

module type Inverse_Transform = sig
  type value
  val qf : float -> value
  val cdf : value -> float
  val constraints : (float * float) list * float list
end

module type MCMC = sig
  type value
  val pdf : float -> float
  val start : float
  val step : float
  val constraints : (float * float) list
end

module Sampler = struct
  module type S = sig
    type value
    val sample : ?start:float -> ?step:float -> int -> value list
  end
  module Make_Inverse_Transform (D : Inverse_Transform) : S with type value = D.value = struct
    type value = D.value
    let sample ?start:_ ?step:_  n =
      let rec loop samples num =
        match num with
          | 0 -> samples
          | _ -> 
            let s = inverse_transform_sample D.qf D.constraints in 
            loop (s :: samples) (num - 1)
      in loop [] n
  end
  module Make_MCMC (D : MCMC) : S with type value = float = struct
    type value = float
    let sample ?(start = D.start) ?(step = D.step) n =
      let rec loop samples num =
        match num with
          | 0 -> samples
          | _ -> 
            let s = mcmc_sample D.pdf D.constraints start step in 
            loop (s :: samples) (num - 1)
      in loop [] n
    end
end


(* Distributions *)

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
          | Some b -> if b then [(0., 0.)], [1.] else [(1., 1.)], [1.]
    end)
end

module Beta 
  (Params : sig val a : float val b : float end) 
  (Constraints : sig val c : (float * float) list option end)
  : Sampler.S with type value = float = struct
    assert (Params.a > 0. 
            && Params.b > 0.)
    include Sampler.Make_MCMC (struct
      type value = float
      let pdf x = d_beta x ~a:Params.a ~b:Params.b
      let constraints = 
        match Constraints.c with
          | None -> []
          | Some l -> l
      let step = 0.005
      let start = 
        if constraints = [] then 
          Params.a /. (Params.a +. Params.b)
        else
          let _, weights = get_uniform_constraints constraints (fun x -> x) in
          let (r1, r2) = q_categorical (base ()) ~classes:constraints ~probs:weights in
          (r1 +. r2) /. 2.
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
          | Some l -> get_uniform_constraints l cdf
    end)
end

module Categorical 
  (Params : sig type t val classes : t list val probs : float list end) 
  (Constraints : sig type t = Params.t val c : t list option end)
  : Sampler.S with type value = Params.t = struct
    assert (Params.classes <> [] &&
            List.length Params.classes = List.length Params.probs &&
            List.for_all bounded Params.probs &&
            sum Params.probs = 1.0)
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
      let constraints = 
        match Constraints.c with
          | None -> []
          | Some l -> l
      let step = 0.05
      let start = 
        if constraints = [] then 
          base () ~a:0. ~b:max_float
        else
          let _, weights = get_uniform_constraints constraints (fun x -> x) in
          let (r1, r2) = q_categorical (base ()) ~classes:constraints ~probs:weights in
          (r1 +. r2) /. 2.
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
      let constraints = 
        match Constraints.c with
          | None -> []
          | Some l -> l
      let step = 0.05
      let start = 
        if constraints = [] then 
          Params.mu
        else
          let _, weights = get_uniform_constraints constraints (fun x -> x) in
          let (r1, r2) = q_categorical (base ()) ~classes:constraints ~probs:weights in
          (r1 +. r2) /. 2.
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
      let constraints = 
        match Constraints.c with
          | None -> []
          | Some l -> l
      let step = 0.05
      let start = 
        if constraints = [] then 
          exp (Params.mu +. ((Params.sigma ** 2.) /. 2.))
        else
          let _, weights = get_uniform_constraints constraints (fun x -> x) in
          let (r1, r2) = q_categorical (base ()) ~classes:constraints ~probs:weights in
          (r1 +. r2) /. 2.
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
          | Some l -> get_uniform_constraints l cdf
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


(* Example user code *)
module My_bernoulli = Bernoulli (struct let p = 1.0 end) (struct let c = None end)





































(* 

module Make (A : Dist_Type) : Sampler_Type = struct

  module Uniform = struct

    type t = {
      a : A.arr;
      b : A.arr;
    }

    let make ~a ~b =
      Utility._check_broadcast_shape [|a; b|];
      { a; b }

    let sample t n = A.uniform_rvs ~a:t.a ~b:t.b ~n

    let pdf t x = A.uniform_pdf ~a:t.a ~b:t.b x

    let logpdf t x = A.uniform_logpdf ~a:t.a ~b:t.b x

    let cdf t x = A.uniform_cdf ~a:t.a ~b:t.b x

    let logcdf t x = A.uniform_logcdf ~a:t.a ~b:t.b x

    let ppf t x = A.uniform_ppf ~a:t.a ~b:t.b x

    let sf t x = A.uniform_sf ~a:t.a ~b:t.b x

    let logsf t x = A.uniform_logsf ~a:t.a ~b:t.b x

    let isf t x = A.uniform_isf ~a:t.a ~b:t.b x

  end *)
