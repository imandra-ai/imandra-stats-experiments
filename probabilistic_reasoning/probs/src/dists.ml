(* dists.ml - Sub-modules and functors defining common distributions
Lewis Hammond - lewis@imandra.ai
2019 *)


(* Necessary modules *)

(* open Utils *)


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
  module Make_MCMC (D : MCMC_S) : S with type value = float = struct
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
              (* print_string ("Rate: " ^ string_of_float rate ^ "\n"); *)
              (* print_string ("Step: " ^ string_of_float b_step ^ " --> " ^ string_of_float (new_step) ^ "\n"); *)
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