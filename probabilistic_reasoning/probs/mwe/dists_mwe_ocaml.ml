(* Minimal working example of a hierarchical probabilistic model over function inputs built using dists.ml *)

(* open Dists *)


(* Types and basic function which will be decomposed *)

type colour = Red | Green | Blue

type obj = {colour : colour;
            mass   : Q.t;
            faces  : Z.t}

(* let f obj sunny temp =
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
  in (a, b) *)


(* Sampling function *)

let get_sample () =

  let module Sunny_dist = Bernoulli (struct let p = 0.4 end) (struct let c = None end) in
  let [s] = Sunny_dist.sample 1 in

  let mean = if s then 20. else 10. in
  let module Temp_dist = Logistic (struct let mu = mean let s = 5. end) (struct let c = None end) in
  let [t] = Temp_dist.sample 1 in

  let module Colour_dist = Categorical (struct type t = colour let classes = [Red;Green;Blue] let probs = [0.5; 0.2; 0.3] end) (struct type t = colour let c = None end) in
  let [c] = Colour_dist.sample 1 in

  let face_constraints = if c = Green then Some [(7, 10)] else None in 
  let module Faces_dist = Poisson (struct let lambda = 6.5 end) (struct let c = face_constraints end) in
  let [f] = Faces_dist.sample 1 in

  let module Mass_dist = Uniform (struct let a = 10.4 let b = 36.9 end) (struct let c = Some [(10.4, 24.7);(33.2, 36.9)] end) in
  let [m] = Mass_dist.sample 1 in

  {colour=c; mass=Q.of_float m; faces=Z.of_int f}, s, Q.of_float t

let sample n =
  let rec loop n' l =
  if n' = 0 then 
    l
  else
    let s = get_sample () in
    loop (n' - 1) (s :: l)
  in loop n []

let save_samples () =
  let s = sample 10000 in
  let oc =  open_out "10000.samples" in
  Marshal.to_channel oc s []; close_out oc





 




