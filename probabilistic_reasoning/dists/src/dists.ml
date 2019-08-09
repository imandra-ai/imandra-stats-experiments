(* TO-DO:
   - CDFs for all distributions
   - Gamma function approximation
   - Package distributions as modules/functors
*)

 
(* Import necessary modules *)

module RS = Random.State


(* Define useful constants *)

let pi = 4. *. atan 1.


(* Define useful helper functions *)

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

let gamma x = x  
  

(* Define primitive random elements *)

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


(* Define cumulative density functions for inverse transform sampling *)





(* Define quantile functions for inverse transform sampling *)

let q_binomial x [n, p] =
  let () = assert (bounded p && n >= 0.) in
  let rec get_successes k prob =
    let term = (choose n k) *. (p ** k) *. ((1.0 -. p) ** (n -. 1.0)) in
    if x <= term +. prob then k
    else get_successes (k +. 1.) (term +. prob)
  in get_successes 0. 0.

let q_bernoulli x [p] = 
  let () = assert (bounded p) in
  if x < p then true else false

let q_categorical x [classes, probs] =
  let () = assert (classes <> [] && List.length classes = List.length probs && List.for_all bounded probs && sum probs = 1.0) in
  let rec loop x y cs ps =
    match cs, ps with
      | [], _ -> List.hd classes
      | _, [] -> List.hd classes
      | c :: _cs, p :: _ps -> 
        if x <= p +. y then c
        else loop x (p +. y) _cs _ps
  in loop x 0. classes probs

let q_uniform x [a, b] = 
  let () = assert (a <= b) in
  a +. ((b -. a) *. x)
 
let q_exponential x [lambda] = 
  let () = assert (lambda > 0.) in
  -.(log (1.0 -. x)) /. lambda

let q_logistic x [mu, s] = 
  let () = assert (s > 0.) in
  mu +. (s *. log (x /. (1.0 -. x)))

let q_cauchy x [x_0, gamma] = 
  let () = assert (gamma > 0.) in
  x_0 +. (gamma *. tan(pi *. (x -. 0.5)))

let q_laplace x [mu, b] = 
  let () = assert (b > 0.) in
  if x <= 0.5 then
    mu +. (b *. log (2.0 *. x))
  else
    mu -. (b *. log (2.0 -. (2.0 *. x)))

let q_delta x [d] =
  if x = 1.0 then d 
  else
    let rec get_non_d () =
      let f = rand_float () in
        if f = d then get_non_d ()
        else f
    in get_non_d ()

let q_poisson x [lambda] =
  let () = assert (lambda > 0.) in
  let rec get_successes k prob =
    let log_term = -.lambda +. (k *. log lambda) -. log_factorial k in
    let term = exp log_term in
    if x <= term +. prob then k
    else get_successes (k +. 1.) (term +. prob)
  in get_successes 0. 0.


(* Define density functions for MCMC sampling *)

let d_gaussian x [mu, sigma] =
  let z = sigma *. sqrt (2. *. pi) in
  let e = ((x -. mu) /. (2. *. sigma)) ** 2.
  in (1. /. z) *. exp (-.e)

let d_gamma x [k, theta] =
  let z = (gamma k) *. (theta ** k) in
  (1. /. z) *. (x ** (k -. 1.)) *. (exp (-.x /. theta))
  
let d_beta x [a, b] =
  let z = (gamma a) *. (gamma b) /. gamma (a +. b) in
  (1. /. z) *. (x ** (a -. 1.)) *. ((1. -. x) ** (b -. 1.))


(* Define sampling functions *)

let inverse_transform_sample qf u_rs n_w_rs =
  match u_rs, n_w_rs with
	| [], [] -> qf (base ())
	| _, _ -> let (r1, r2) = q_categorical (base ()) [u_rs, n_w_rs] in
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
  end) in (module C : Consts_Type)


(* Define distribution modules *)

(* module Bernoulli : Dist_Type = struct
  type t = bool
  let d = InverseTransform {qf = q_bernoulli; cdf = }
end

module Beta : Dist_Type = struct
  type t = float
  let d = MCMC {pdf = d_beta}
end

module Binomial : Dist_Type = struct
  type t = int
  let d = InverseTransform {qf = q_binomial; cdf = }
end

module Categorial : Dist_Type = struct
  type t = 'a
  let d = InverseTransform {qf = q_categorical; cdf = }
end

module Cauchy : Dist_Type = struct
  type t = float
  let d = InverseTransform {qf = q_cauchy; cdf = }
end

module Delta : Dist_Type = struct
  type t = float
  let d = InverseTransform {qf = q_delta; cdf = }
end

module Exponential : Dist_Type = struct
  type t = float
  let d = InverseTransform {qf = q_exponential; cdf = }
end

module Gamma : Dist_Type = struct
  type t = float
  let d = MCMC {pdf = d_gamma}
end

module Gaussian : Dist_Type = struct
  type t = float
  let d = MCMC {pdf = d_gaussian}
end

module Laplace : Dist_Type = struct
  type t = float
  let d = InverseTransform {qf = q_laplace; cdf = }
end

module LogNormal : Dist_Type = struct
  type t = float
  let d = MCMC {pdf = d_lognormal}
end

module Poisson : Dist_Type = struct
  type t = int
  let d = InverseTransform {qf = q_poisson; cdf = }
end

module Uniform : Dist_Type = struct
  type t = float
  let d = InverseTransform {qf = q_uniform; cdf = }
end *)


(* Define sampler functor *)

module Sampler (D : Dist_Type) (P : Params_Type with type t = D.t) (C : Consts_Type with type t = D.t) = struct

  type t = D.t

  let dists = D.d
  let params = P.p
  let constraints = C.c

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
	  
  let sample n ?(step = 1.) ?(start = 0.) =
    let get_sample =
      match P.p with
        | NumParam p_l ->
          match C.c with
            | NumConst c_l ->
              match D.d with
                | Categorical _ -> n
                | MCMC {pdf = p} -> 
                  let pdf x = p x (NumParam p_l) in
                  mcmc_sample pdf c_l start step
                | InverseTransform {cdf = c; qf = q} ->
                  let cdf x = c x (NumParam p_l) in
                  let u_rs, n_w_rs = get_uniform_constraints c_l cdf in
                  let qf x = q x (NumParam p_l) in
                  (* inverse_transform_sample qf u_rs n_w_rs *)
            | CatConst _ ->

        | CatParam (c, p) ->
          match C.c with
            | NumConst _ ->
                
            | CatConst c_l ->
              let classes, probs = constrain_categorical c_l c p in
              match D.d with
                | MCMC _ -> 
                | InverseTransform {cdf = _; qf = q} ->
                  let qf x = q x (CatParam (classes, probs)) in 
                  inverse_transform_sample qf [] []        
      in for i in 1 to n 

  
end 





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
