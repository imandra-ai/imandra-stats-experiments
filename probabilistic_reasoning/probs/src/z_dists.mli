(* val pi : float *)

(* val log_2_pi : float = 1.83787706640934534
val max_int_bits : int = 1073741823
val bits_26 : int = 67108864
val q0 : Q.t = {Q.num = <abstr>; den = <abstr>}
module RS = Random.State
type st = RS.t
val default_seed : int array = [|19841983; 7298712389; 17862387612|]
val prng : st = <abstr>
val rs_bool : st -> unit -> bool = <fun>
val rs_int : st -> ?bound:int -> unit -> int = <fun>
val rs_Z : st -> ?bound:int -> unit -> Z.t = <fun>
val rs_float : st -> ?bound:float -> unit -> float = <fun>
val rs_Q : st -> ?bound:Q.t -> unit -> Q.t = <fun>
val rs_base : st -> ?a:float -> ?b:float -> unit -> float = <fun>
module NC = Nocrypto.Rng
val nc_bool : unit -> bool = <fun>
val nc_int : ?bound:int -> unit -> int = <fun>
val nc_Z : ?bound:Z.t -> unit -> Z.t = <fun>
val nc_base : ?a:float -> ?b:float -> unit -> float = <fun>
val nc_float : ?bound:float -> unit -> float = <fun>
val nc_Q : ?bound:Q.t -> unit -> Q.t = <fun> *)

(* val base : ?a:float -> ?b:float -> unit -> float *)

(* val bounded : float -> bool = <fun>
val sum : float list -> float = <fun>
val sum_n : int -> float list -> float = <fun>
val normalise : float list -> float list = <fun>
val log_factorial : float -> float = <fun>
val choose : float -> float -> float = <fun>
val log_nemes_closed_form : float -> float = <fun>
val gamma : float -> float = <fun>
val proposal_dist : mu:float -> s:float -> float -> float = <fun>
val constrain_categorical :
  'a list -> 'a list * float list -> 'a list * float list = <fun>
val constraint_comparison : 'a * 'b -> 'a * 'b -> int = <fun>
val get_uniform_constraints :
  ('a * 'a) list -> ('a -> float) -> (float * float) list * float list = <fun>
val process_constraints :
  (Q.t * Q.t) list -> (float * float) list * (float * float) * float = <fun>
val closest : float -> (float * float) list -> float = <fun>
val make_inclusive : (Z.t * 'a) list -> (Z.t * 'a) list -> (Z.t * 'a) list =
  <fun>
val q_bernoulli : p:float -> float -> bool = <fun>
val q_binomial : n:float -> p:float -> float -> Z.t = <fun>
val q_categorical : classes:'a list -> probs:float list -> float -> 'a = <fun>
val q_cauchy : x_0:float -> gamma:float -> float -> Q.t = <fun>
val q_exponential : lambda:float -> float -> Q.t = <fun>
val q_laplace : mu:float -> b:float -> float -> Q.t = <fun>
val q_logistic : mu:float -> s:float -> float -> Q.t = <fun>
val q_poisson : lambda:float -> float -> Z.t = <fun>
val q_uniform : a:float -> b:float -> float -> Q.t = <fun>
val c_bernoulli : p:float -> bool -> float = <fun>
val c_binomial : n:float -> p:float -> Z.t -> float = <fun>
val c_categorical : classes:'a list -> probs:float list -> 'a -> float = <fun>
val c_cauchy : x_0:float -> gamma:float -> Q.t -> float = <fun>
val c_exponential : lambda:float -> Q.t -> float = <fun>
val c_laplace : mu:float -> b:float -> Q.t -> float = <fun>
val c_logistic : mu:float -> s:float -> Q.t -> float = <fun>
val c_poisson : lambda:float -> Z.t -> float = <fun>
val c_uniform : a:float -> b:float -> Q.t -> float = <fun>
val d_beta : a:float -> b:float -> float -> float = <fun>
val d_gamma : k:float -> theta:float -> float -> float = <fun>
val d_gaussian : mu:float -> sigma:float -> float -> float = <fun>
val d_lognormal : mu:float -> sigma:float -> float -> float = <fun>
val inverse_transform_sample :
  (float -> 'a) -> (float * float) list * float list -> 'a = <fun>
val mcmc_sample :
  (float -> float) ->
  (float * float) list -> float * float -> float -> float -> float -> float =
  <fun> *)

module type Inverse_Transform_S =
  sig
    type value
    val qf : float -> value
    val cdf : value -> float
    val constraints : (float * float) list * float list
  end

module type MCMC_S =
  sig
    type value
    val pdf : float -> float
    val constraints : (float * float) list
    val bounds : float * float
    val gradient : float
    val start : float
    val step : float
    val to_burn : int
  end

module Sampler :
  sig
    module type S =
      sig
        type value
        val sample : ?start:float -> ?step:float -> int -> value list
      end
    module Make_Inverse_Transform :
      functor (D : Inverse_Transform_S) ->
        sig
          type value = D.value
          val sample : ?start:float -> ?step:float -> int -> value list
        end
    module Make_MCMC :
      functor (D : MCMC_S) ->
        sig
          type value = Q.t
          val sample : ?start:float -> ?step:float -> int -> value list
        end
  end

module Bernoulli :
  functor
    (Params : sig val p : Q.t end) (Constraints : sig val c : bool option end) ->
    sig
      type value = bool
      val sample : ?start:float -> ?step:float -> int -> value list
    end

module Beta :
  functor
    (Params : sig val a : Q.t val b : Q.t end) (Constraints : sig
                                                                val c :
                                                                  (Q.t * Q.t)
                                                                  list option
                                                              end) ->
    sig
      type value = Q.t
      val sample : ?start:float -> ?step:float -> int -> value list
    end

module Binomial :
  functor
    (Params : sig val n : Z.t val p : Q.t end) (Constraints : sig
                                                                val c :
                                                                  (Z.t * Z.t)
                                                                  list option
                                                              end) ->
    sig
      type value = Z.t
      val sample : ?start:float -> ?step:float -> int -> value list
    end

module Categorical :
  functor
    (Params : sig type t val classes : t list val probs : Q.t list end) (Constraints : 
    sig
      type t = Params.t
      val c : t list option
    end) ->
    sig
      type value = Constraints.t
      val sample : ?start:float -> ?step:float -> int -> value list
    end

module Cauchy :
  functor
    (Params : sig val x_0 : Q.t val gamma : Q.t end) (Constraints : sig
                                                                    val c :
                                                                    (Q.t * Q.t)
                                                                    list option
                                                                    end) ->
    sig
      type value = Q.t
      val sample : ?start:float -> ?step:float -> int -> value list
    end

module Exponential :
  functor
    (Params : sig val lambda : Q.t end) (Constraints : sig
                                                         val c :
                                                           (Q.t * Q.t) list
                                                           option
                                                       end) ->
    sig
      type value = Q.t
      val sample : ?start:float -> ?step:float -> int -> value list
    end

module Gamma :
  functor
    (Params : sig val k : Q.t val theta : Q.t end) (Constraints : sig
                                                                    val c :
                                                                    (Q.t * Q.t)
                                                                    list option
                                                                  end) ->
    sig
      type value = Q.t
      val sample : ?start:float -> ?step:float -> int -> value list
    end

module Gaussian :
  functor
    (Params : sig val mu : Q.t val sigma : Q.t end) (Constraints : sig
                                                                    val c :
                                                                    (Q.t * Q.t)
                                                                    list option
                                                                   end) ->
    sig
      type value = Q.t
      val sample : ?start:float -> ?step:float -> int -> value list
    end

module Laplace :
  functor
    (Params : sig val mu : Q.t val b : Q.t end) 
    (Constraints : sig val c : (Q.t * Q.t) list option end) ->
    sig
      type value = Q.t
      val sample : ?start:float -> ?step:float -> int -> value list
    end

module Logistic :
  functor
    (Params : sig val mu : Q.t val s : Q.t end) (Constraints : sig
                                                                 val c :
                                                                   (Q.t * Q.t)
                                                                   list option
                                                               end) ->
    sig
      type value = Q.t
      val sample : ?start:float -> ?step:float -> int -> value list
    end

module LogNormal :
  functor
    (Params : sig val mu : Q.t val sigma : Q.t end) (Constraints : sig
                                                                    val c :
                                                                    (Q.t * Q.t)
                                                                    list option
                                                                   end) ->
    sig
      type value = Q.t
      val sample : ?start:float -> ?step:float -> int -> value list
    end

module Poisson :
  functor
    (Params : sig val lambda : Q.t end) (Constraints : sig
                                                         val c :
                                                           (Z.t * Z.t) list
                                                           option
                                                       end) ->
    sig
      type value = Z.t
      val sample : ?start:float -> ?step:float -> int -> value list
    end

module Uniform :
  functor
    (Params : sig val a : Q.t val b : Q.t end) (Constraints : sig
                                                                val c :
                                                                  (Q.t * Q.t)
                                                                  list option
                                                              end) ->
    sig
      type value = Q.t
      val sample : ?start:float -> ?step:float -> int -> value list
    end