(* models.ml - Creates user-generated probabilistic models for sampling
Lewis Hammond - lewis@imandra.ai
2019 *)


(* Necessary modules *)

(* open Dists *)

open Imandra_tools

(* Helper functions *)

let float_of constraints =
  match constraints with
    | None -> None
    | Some c_list -> Some (List.map (fun (x, y) -> (Q.to_float x, Q.to_float y)) c_list)

let int_of constraints =
  match constraints with
    | None -> None
    | Some c_list -> Some (List.map (fun (x, y) -> (Z.to_int x, Z.to_int y)) c_list)
   

(* Distributions *)

(* let bernoulli ~p ?(constraints=None) () =
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
  Q.of_float (D.sample_1 ()) *)


(* Module for defining and sampling from distribution *)

module type Distribution_S = sig
  type domain
  val dist : unit -> domain
end

module type Make_S = sig
  type domain
  val get_probs : Imandra_surface.Decompose_region.t list -> ?n:Z.t -> unit -> (int, float) Hashtbl.t
  val save_samples : (unit -> domain) -> string -> int -> unit
  val load_samples : string -> domain list
end

module Make (D : Distribution_S) : Make_S with type domain = D.domain = struct

  type domain = D.domain

  let get_probs regions ?(n=10000) () =
    let _, indexer = Region_idx.indexer_for (module struct type args = domain end) regions in
    let num_samples = Z.to_int n in
    let num_regions = List.length regions in
    let rec init_freqs rs f_tbl =
      match rs with
        | [] -> f_tbl
        | (r : Imandra_surface.Decompose_region.t) :: t -> 
          let i = r.reg_id in
          let () = Hashtbl.add f_tbl i 0. in
          init_freqs t f_tbl
    in let freqs = init_freqs regions (Hashtbl.create num_regions) in
    let rec loop num_remaining fs =
      match num_remaining with
        | 0 -> 
          let () = Hashtbl.filter_map_inplace (fun _ y -> Some (y /. float num_samples)) fs in
          fs
        | m ->
          let x = D.dist () in
          let i = indexer x in
          let c = Hashtbl.find fs i in
          let () = Hashtbl.replace fs i (c +. 1.) in
          loop (m - 1) fs
    in loop num_samples freqs

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


(* Example *)

(* type colour = Red | Green | Blue
  
type obj = {colour : colour;
            mass   : Q.t;
            faces  : Z.t}

type domain = (obj * bool * Q.t)

let f (obj, sunny, temp) =
  let a = 
    if obj.colour = Red then
      obj.mass *. temp
    else
      100. -. temp
  in let b =
    if sunny then 
      obj.faces
    else 
      23
  in (a, b)

let regions = Decompose.top "f"

let dist () =
  let c = categorical ~classes:[Red; Green; Blue] ~probs:[0.5; 0.2; 0.3] () in
  let s = bernoulli ~p:0.4 () in
  let mu = if s then 20. else 10. in
  let t = logistic ~mu ~s:5.0 () in
  let m_constraints = Some [(10.4, 24.7);(33.2, 36.9)] in
  let m = uniform ~a:10.4 ~b:36.9 ~constraints:m_constraints () in
  let f_constraints = if c = Green then Some [(7, 10)] else None in
  let f = poisson ~lambda:6.5 ~constraints:f_constraints () in
  let o = {colour = c; mass = m; faces = f} in
  (o, s, t)

module Example = Make (struct type domain = domain let dist = dist end)

let probs = Example.get_probs regions () *)