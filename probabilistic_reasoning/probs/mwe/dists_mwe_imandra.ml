(* Minimal working example of a hierarchical probabilistic model over function inputs built using dists.ml *)

(* open Dists *)


(* Types and basic function which will be decomposed *)

type colour = Red | Green | Blue
  
type obj = {colour : colour;
              mass   : Q.t;
              faces  : Z.t}

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
  in (a, b);;

let regions = Decompose.top "f" [@@program];;


(* Sampling function *)

(* let get_sample () =

  let module Sunny_dist = Bernoulli (struct let p = Q.of_float 0.4 end) (struct let c = None end) in
  let [s] = Sunny_dist.sample 1 in

  let mean = if s then Q.of_float 20. else Q.of_float 10. in
  let module Temp_dist = Logistic (struct let mu = mean let s = Q.of_float 5. end) (struct let c = None end) in
  let [t] = Temp_dist.sample 1 in

  let module Colour_dist = Categorical (struct type t = colour let classes = [Red;Green;Blue] let probs = [Q.of_float 0.5; Q.of_float 0.2; Q.of_float 0.2] end) (struct type t = colour let c = None end) in
  let [c] = Colour_dist.sample 1 in

  let face_constraints = if c = Green then Some [(Z.of_int 7, Z.of_int 10)] else None in 
  let module Faces_dist = Poisson (struct let lambda = Q.of_float 6.5 end) (struct let c = face_constraints end) in
  let [f] = Faces_dist.sample 1 in

  let module Mass_dist = Uniform (struct let a = Q.of_float 10.4 let b = Q.of_float 36.9 end) (struct let c = Some [(Q.of_float 10.4, Q.of_float 24.7);(Q.of_float 33.2, Q.of_float 36.9)] end) in
  let [m] = Mass_dist.sample 1 in

  {colour=c; mass=m; faces=f}, s, t *)


(* Process regions and produce frequency estimates *)

let recognizer_code region =
    let open Imandra_tools.Region_term_synth in
    let args = List.map Var.name @@ Decompose_region.args region |> String.concat " " in
    let code = synthesize ~default:Term.Syn.False (Term.and_l @@ Decompose_region.constraints region) in
    let g = Imandra_util.Util.gensym () in
    let f = Printf.sprintf "let %s = fun %s -> %s" g args code in
    Pconfig.with_mode_assigned ~to_:Pconfig.State.Logic  Reflect.eval f;
    g
  [@@program];;

let _f = ref None [@@program];;

let make my_fun x =
  Pconfig.with_mode_assigned ~to_:Pconfig.State.Program
     Reflect.eval (Printf.sprintf "_f := Some (Obj.magic (%s))" my_fun);
  let g : 'a -> bool = !_f |> CCOpt.get_exn |> Obj.magic in
  g x [@@program];;

let region_idx_for x regions funs =
  let rec loop rs =
    match rs with
      | [] -> failwith "Input did not fall into any region"
      | (r : Imandra_surface.Decompose_region.t) :: t -> 
        let i = r.reg_id in
        let g = Hashtbl.find funs i in
        if make g x then i else loop t
  in loop regions [@@program];;

let get_freqs (regions : Imandra_surface.Decompose_region.t list) samples = 
  let num_samples = Q.of_bigint (List.length samples) in
  let num_regions = Z.to_int (List.length regions) in
  let rec init_funs_freqs rs g_tbl f_tbl =
    match rs with
      | [] -> g_tbl, f_tbl
      | (r : Imandra_surface.Decompose_region.t) :: t -> 
        let i = r.reg_id in
        let g = recognizer_code r in
        let () = Hashtbl.add f_tbl i 0. in
        let () = Hashtbl.add g_tbl i g in
        init_funs_freqs t g_tbl f_tbl
  in let funs, freqs = init_funs_freqs regions (Hashtbl.create num_regions) (Hashtbl.create num_regions) in
  let rec loop ss fs =
    match ss with
      | [] -> 
        let () = Hashtbl.filter_map_inplace (fun _ y -> Some (y /. num_samples)) fs in
        fs
      | x :: t ->
        (* let i = region_idx_for x regions funs in *)
        let i = (List.hd regions).reg_id in
        let c = Hashtbl.find fs i in
        let () = Hashtbl.replace fs i (c +. 1.) in
        loop t fs
  in loop samples freqs [@@program];;

type sample_list = (obj * bool * Q.t) list

let run samples_file =
  let ic = open_in samples_file in
  let s = (Marshal.from_channel ic : sample_list) in
  get_freqs regions s [@@program]

let time f x =
  let t = Caml_sys.time() in
  let fx = f x in
  let t' = Caml.(-.) (Caml_sys.time ()) t in
  Printf.printf "Execution time: %fs\n" (t');
  t', fx [@@program]