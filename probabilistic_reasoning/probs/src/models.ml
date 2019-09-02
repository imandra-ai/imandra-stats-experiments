(* models.ml - Creates user-generated probabilistic models for sampling
Lewis Hammond - lewis@imandra.ai
2019 *)


(* Necessary modules *)

open Dists


(* Helper functions *)

let float_of constraints =
  let rec loop c l =
    match c with
      | [] -> l
      | (a, b) :: t -> loop t ((Q.to_float a, Q.to_float b) :: l)
  in match constraints with
    | None -> None
    | Some c_list -> Some (loop c_list [])

let int_of constraints =
  let rec loop c l =
    match c with
      | [] -> l
      | (a, b) :: t -> loop t ((Z.to_int a, Z.to_int b) :: l)
  in match constraints with
    | None -> None
    | Some c_list -> Some (loop c_list [])


(* Distributions *)

let bernoulli ~p ?(constraints=None) ?(samples=1) =
  let p = Q.to_float p in
  let module Params = (struct let p = p end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Bernoulli(Params)(Consts) in
  D.sample samples

let beta ~a ~b ?(constraints=None) ?(samples=1) =
  let a, b, constraints = Q.to_float a, Q.to_float b, float_of constraints in
  let module Params = (struct let a = a let b = b end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Beta(Params)(Consts) in
  List.map Q.of_float (D.sample samples)

let binomial ~n ~p ?(constraints=None) ?(samples=1) =
  let n, p, constraints = Z.to_int n, Q.to_float p, int_of constraints in
  let module Params = (struct let n = n let p = p end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Binomial(Params)(Consts) in
  List.map Z.of_int (D.sample samples)

let categorical (type a) ~classes ~probs ?(constraints=None) ?(n=1) =
  let probs = List.map Q.to_float probs in
  let module Params = (struct type t = a let classes = classes let probs = probs end) in
  let module Consts = (struct type t = a let c = constraints end) in
  let module D = Categorical(Params)(Consts) in
  D.sample n 

let cauchy ~x_0 ~gamma ?(constraints=None) ?(samples=1) =
  let x_0, gamma, constraints = Q.to_float x_0, Q.to_float gamma, float_of constraints in
  let module Params = (struct let x_0 = x_0 let gamma = gamma end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Cauchy(Params)(Consts) in
  List.map Q.of_float (D.sample samples)

let exponential ~lambda ?(constraints=None) ?(samples=1) =
  let lambda, constraints = Q.to_float lambda, float_of constraints in
  let module Params = (struct let lambda = lambda end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Exponential(Params)(Consts) in
  List.map Q.of_float (D.sample samples)

let gamma ~k ~theta ?(constraints=None) ?(samples=1) =
  let k, theta, constraints = Q.to_float k, Q.to_float theta, float_of constraints in
  let module Params = (struct let k = k let theta = theta end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Gamma(Params)(Consts) in
  List.map Q.of_float (D.sample samples)

let gaussian ~mu ~sigma ?(constraints=None) ?(samples=1) =
  let mu, sigma, constraints = Q.to_float mu, Q.to_float sigma, float_of constraints in
  let module Params = (struct let mu = mu let sigma = sigma end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Gaussian(Params)(Consts) in
  List.map Q.of_float (D.sample samples)

let laplace ~mu ~b ?(constraints=None) ?(samples=1) =
  let mu, b, constraints = Q.to_float mu, Q.to_float b, float_of constraints in
  let module Params = (struct let mu = mu let b = b end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Laplace(Params)(Consts) in
  List.map Q.of_float (D.sample samples)

let logistic ~mu ~s ?(constraints=None) ?(samples=1) =
  let mu, s, constraints = Q.to_float mu, Q.to_float s, float_of constraints in
  let module Params = (struct let mu = mu let s = s end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Logistic(Params)(Consts) in
  List.map Q.of_float (D.sample samples)

let lognormal ~mu ~sigma ?(constraints=None) ?(samples=1) =
  let mu, sigma, constraints = Q.to_float mu, Q.to_float sigma, float_of constraints in
  let module Params = (struct let mu = mu let sigma = sigma end) in
  let module Consts = (struct let c = constraints end) in
  let module D = LogNormal(Params)(Consts) in
  List.map Q.of_float (D.sample samples)

let poisson ~lambda ?(constraints=None) ?(samples = 1) =
  let lambda, constraints = Q.to_float lambda, int_of constraints in
  let module Params = (struct let lambda = lambda end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Poisson(Params)(Consts) in
  List.map Z.of_int (D.sample samples)

let uniform ~a ~b ?(constraints=None) ?(samples=1) =
  let a, b, constraints = Q.to_float a, Q.to_float b, float_of constraints in
  let module Params = (struct let a = a let b = b end) in
  let module Consts = (struct let c = constraints end) in
  let module D = Uniform(Params)(Consts) in
  List.map Q.of_float (D.sample samples)


(* Module for defining and sampling from distribution *)

module type Distribution_S = sig
  type domain
  val dist : unit -> domain
end

module type Make_S = sig
  type domain
  val get_probs : Imandra_surface.Decompose_region.t list -> ?n:int -> (int, float) Hashtbl.t
  val save_samples : (unit -> domain) -> string -> int -> unit
  val load_samples : string -> domain list
end

module Make (D : Distribution_S) : Make_S with type domain = D.domain = struct

  type domain = D.domain

  let get_probs regions ?(n=10000) =
    let _, indexer = Imandra_tools.Region_idx.indexer_for (module struct type args = domain end) regions in
    let num_samples = Q.of_bigint n in
    let num_regions = Z.to_int (List.length regions) in
    let rec init_freqs rs f_tbl =
      match rs with
        | [] -> f_tbl
        | r :: t -> 
          let i = r.reg_id in
          let () = Hashtbl.add f_tbl i 0. in
          init_freqs t f_tbl
    in let freqs = init_freqs regions (Hashtbl.create num_regions) in
    let rec loop num_remaining fs =
      match num_remaining with
        | 0 -> 
          let () = Hashtbl.filter_map_inplace (fun _ y -> Some (y /. num_samples)) fs in
          fs
        | m ->
          let x = D.dist () in
          let i = indexer x  in
          let c = Hashtbl.find fs i in
          let () = Hashtbl.replace fs i (c +. 1.) in
          loop (m - 1) fs
    in loop n freqs;;

  let save_samples distribution name n =
    let rec loop n' l =
    if n' = 0 then 
      l
    else
      let s = distribution () in
      loop (n' - 1) (s :: l)
    in let samples = loop n [] in
    let oc =  open_out name in
    Marshal.to_channel oc samples []; close_out oc

  let load_samples name =
    let ic = open_in name in
    (Marshal.from_channel ic : domain list)
    
  end