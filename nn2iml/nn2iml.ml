(* Utility functions *)

let split l = List.filter (fun x -> not (x = "")) (String.split_on_char ',' l)

let var_list n c =
  let rec loop m l = 
    let v = c ^ "_" ^ (string_of_int m) in
    if m = 0 then (v :: l) 
    else loop (m - 1) (v :: l) in
  loop (n - 1) []

let remove_e s =
  match String.split_on_char 'e' s with
    | [f; p] ->
      begin
      let ff, minus = 
        if (float_of_string f) < 0. then
          String.sub f 1 (String.length f - 1), "-."
        else
          f, "" in
      let z = int_of_string p in
        if z = 0 then minus ^ ff
        else match String.split_on_char '.' ff with
          | [a; b] -> 
            (let l_z = abs z in
            if z > 0 then
              let bb = string_of_int (int_of_string b) in
              let l_b = String.length bb in
              if l_b >= l_z then
                (minus ^ a ^ (String.sub bb 0 l_z) ^ "." ^ (String.sub bb l_z (l_b - l_z)))
              else
                (minus ^ a ^ bb ^ (String.make (l_z - l_b) '0') ^ ".")
            else
              let aa = string_of_int (int_of_string a) in
              let l_a = String.length aa in
              if l_a > l_z then
                (minus ^ (String.sub aa 0 (l_a - l_z)) ^ "." ^ (String.sub aa (l_a - l_z) l_z) ^ b)
              else 
                (minus ^ "0." ^ (String.make (l_z - l_a) '0') ^ aa ^ b))
          | _ -> failwith "Float has no decimal point!"
      end
    | [f] -> f
    | _ -> failwith "String split incorrectly"

let print_line y xs ws b a oc =
  let () = output_string oc ("  let " ^ y ^ " = " ^ a ^ "(") in
  let rec loop x w =
    if x = [] then ()
    else 
      let () = output_string oc ("(" ^ (List.hd w) ^ ")*." ^ (List.hd x) ^ " +. ") in
      loop (List.tl x) (List.tl w) in
    let () = loop xs ws in
    output_string oc ("(" ^ (List.hd b) ^ ")) in\n")

let print_scaler inputs means ranges oc =
    let xx = var_list inputs "x" in
    let yy = var_list inputs "y" in
    let () = output_string oc ("let scaler (" ^ (String.concat ", " xx) ^ ") =\n") in
    let rec loop xs ys ms rs =
      if xs = [] then ()
      else
        let x = List.hd xs in
        let y = List.hd ys in
        let m = List.hd ms in
        let r = List.hd rs in
        let () = output_string oc ("  let " ^ y ^ " = (" ^ x ^ " -. " ^ m ^ ") /. " ^ r ^ " in\n") in
        loop (List.tl xs) (List.tl ys) (List.tl ms) (List.tl rs) in
    let () = loop xx yy means ranges in
    output_string oc ("  (" ^ (String.concat ", " yy) ^ ");;\n\n")

let print_descaler outputs mean range oc =
  let xx = var_list outputs "x" in
  let yy = var_list outputs "y" in
  let () = output_string oc ("let descaler (" ^ (String.concat ", " xx) ^ ") =\n") in
  let rec loop xs ys =
    if xs = [] then ()
    else
      let x = List.hd xs in
      let y = List.hd ys in
      let () = output_string oc ("  let " ^ y ^ " = (" ^ x ^ " *. " ^ range ^ ") +. " ^ mean ^ " in\n") in
      loop (List.tl xs) (List.tl ys) in
  let () = loop xx yy in
  output_string oc ("  (" ^ (String.concat ", " yy) ^ ");;\n\n")

let print_bounder inputs mins maxs oc =
  let xx = var_list inputs "x" in
  let yy = var_list inputs "y" in
  let () = output_string oc ("let bounder (" ^ (String.concat ", " xx) ^ ") =\n") in
  let rec loop xs ys ms mms =
    if xs = [] then ()
    else
      let x = List.hd xs in
      let y = List.hd ys in
      let m = List.hd ms in
      let mm = List.hd mms in
      let () = output_string oc ("  let " ^ y ^ " = max (" ^ m ^ ") (min " ^ x ^ " (" ^ mm ^ ")) in\n") in
      loop (List.tl xs) (List.tl ys) (List.tl ms) (List.tl mms) in
  let () = loop xx yy mins maxs in
  output_string oc ("  (" ^ (String.concat ", " yy) ^ ");;\n\n")


(* Converts from a .nnet file *)

let from_nnet filename =

  (* Setup *)
  let ic = open_in filename in
  let out_filename = (String.sub filename 0 (String.length filename - 4)) ^ "iml" in
  let oc = open_out out_filename in

  (* Skip header and blank lines *)
  let skip line =
    if String.sub line 0 2 = "//" then true else false in
  let rec get_line () =
    let line = input_line ic in
    if skip line then get_line ()
    else line in

  (* Extract key values from initial lines *)
  let num_layers, num_inputs, num_outputs, max_layer_size =
    match List.map int_of_string (split (get_line ())) with
      | [n_l; n_i; n_o; m_l_s] -> n_l, n_i, n_o, m_l_s
      | _ -> failwith "The .nnet file is incorrectly specified" in
  let layer_sizes = List.map int_of_string (split (get_line ())) in
  let _ = get_line () in
  let min_inputs = List.map remove_e (split (get_line ())) in
  let max_inputs = List.map remove_e (split (get_line ())) in
  let mean_inputs, mean_output =
    match List.rev (List.map remove_e (split (get_line ()))) with 
      | m_o :: m_i -> List.rev m_i, m_o
      | _ -> failwith "Must have at least one input and output" in
  let range_inputs, range_output =
    match List.rev (List.map remove_e (split (get_line ()))) with 
      | r_o :: r_i -> List.rev r_i, r_o
      | _ -> failwith "Must have at least one input and output" in

  (* Output helper functions *)
  let () = output_string oc "let relu x = if x > 0. then x else 0.;;\n\n" in
  let () = print_scaler num_inputs mean_inputs range_inputs oc in
  let () = print_descaler num_outputs mean_output range_output oc in
  let () = print_bounder num_inputs min_inputs max_inputs oc in
  
  (* Output each layer *)
  let rec process_layer l dimensions =
    match dimensions with
      | i :: o :: t ->
        let activation = if t = [] then "" else "relu @@ " in
        let xs = var_list i "x" in
        let ys = var_list o "y" in
        let rec get_weights rows_left w =
          match rows_left with
            | 0 -> List.rev w
            | r -> get_weights (r - 1) ((List.map remove_e (split (get_line ()))) :: w) in
        let ws = get_weights o [] in
        let bs = get_weights o [] in
        let () = output_string oc ("let layer_" ^ (string_of_int l) ^ " (" ^ (String.concat ", " xs) ^ ") =\n") in
        let rec print_weights yy ww bb : unit =
          if yy = [] then ()
          else 
            let () = print_line (List.hd yy) xs (List.hd ww) (List.hd bb) activation oc in
            print_weights (List.tl yy) (List.tl ww) (List.tl bb) in
        print_weights ys ws bs;
        output_string oc ("  (" ^ (String.concat ", " ys) ^ ");;\n\n");
        process_layer (l + 1) (o :: t)
      | _ -> () in
  let () = process_layer 0 layer_sizes in

  (* Output final model architecture *)
  let () = close_in ic in
  let inputs = var_list num_inputs "x" in
  let layers = var_list num_layers "layer" in
  let () = output_string oc ("let network (" ^ (String.concat ", " inputs) ^ ") = \n") in
  let () = output_string oc ("  (" ^ (String.concat ", " inputs) ^ ")\n  |> bounder\n  |> scaler\n  |> ") in
  let () = output_string oc ((String.concat "\n  |> " layers) ^ "\n  |> descaler;;") in
  close_out oc