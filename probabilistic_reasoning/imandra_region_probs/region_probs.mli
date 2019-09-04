(* Univariate distribution functions for generating single samples *)
val bernoulli : p:Q.t -> ?constraints:bool option -> unit -> bool
val beta : a:Q.t -> b:Q.t -> ?constraints:(Q.t * Q.t) list option -> unit -> Q.t
val binomial : n:Z.t -> p:Q.t -> ?constraints:(Z.t * Z.t) list option -> unit -> Z.t
val categorical : classes:'a list -> probs:Q.t list -> ?constraints:'a list option -> unit -> 'a
val cauchy : x_0:Q.t -> gamma:Q.t -> ?constraints:(Q.t * Q.t) list option -> unit -> Q.t
val exponential : lambda:Q.t -> ?constraints:(Q.t * Q.t) list option -> unit -> Q.t
val gamma : k:Q.t -> theta:Q.t -> ?constraints:(Q.t * Q.t) list option -> unit -> Q.t
val gaussian : mu:Q.t -> sigma:Q.t -> ?constraints:(Q.t * Q.t) list option -> unit -> Q.t
val laplace : mu:Q.t -> b:Q.t -> ?constraints:(Q.t * Q.t) list option -> unit -> Q.t
val logistic : mu:Q.t -> s:Q.t -> ?constraints:(Q.t * Q.t) list option -> unit -> Q.t
val lognormal : mu:Q.t -> sigma:Q.t -> ?constraints:(Q.t * Q.t) list option -> unit -> Q.t
val poisson : lambda:Q.t -> ?constraints:(Z.t * Z.t) list option -> unit -> Z.t
val uniform : a:Q.t -> b:Q.t -> ?constraints:(Q.t * Q.t) list option -> unit -> Q.t

(* Distribution module type used as input to Make functor *)
module type Distribution = sig
  type domain
  val dist : unit -> domain
end

(* Functor type used to create a model from from a Distribution module *)
module type Make = sig
  type domain
  val get_indices : Imandra_surface.Decompose_region.t list -> (int * Imandra_surface.Decompose_region.t) list
  val get_probs : Imandra_surface.Decompose_region.t list -> ?n:Z.t -> unit -> (int, float) Hashtbl.t
  val query : (domain -> bool) -> ?n:Z.t -> unit -> float
  val save_samples : string -> Z.t -> unit
  val load_samples : string -> domain list
end

(* Functor outputting model module for sampling from, querying, and estimating probabilities *)
module Make (D : Distribution) : Make with type domain = D.domain

(* Printer for region probabilities with adjustable decimal precsion *)
val print_probs : ?precision:Z.t -> (int, float) Hashtbl.t -> unit