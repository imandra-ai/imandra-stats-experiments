(* Tests for the main functions and functors in dists.ml *)

(* open Dists *)


(* Helper functions and constants *)

let float_of_bool b = if b then 1. else 0.

let rec print oc l = 
  match l with 
    | [] -> ()
    | h :: t -> Printf.fprintf oc "%f\n" h; print oc t

let save l f =
  let oc = open_out f in
  print oc l;
  close_out oc

let range a b n =
  let step = (b -. a) /. float n in
  let rec loop curr last num =
    if num = n then curr
    else loop ((last +. step) :: curr) (last +. step) (num + 1)
  in List.rev (loop [a] a 0)

let qf_range = (range 0. 0.99 99) @ [1.]


(* Test and save QFs *)

let save_qfs () =
  save (List.map float_of_bool (List.map (q_bernoulli ~p:0.73) qf_range)) "q_bernoulli.csv";
  save (List.map float (List.map (q_binomial ~n:30. ~p:0.2) qf_range)) "q_binomial.csv";
  save (List.map float (List.map (q_categorical ~classes:[2;4;6;8;10] ~probs:[0.1;0.4;0.05;0.15;0.3]) qf_range)) "q_categorical.csv";
  save (List.map (q_cauchy ~x_0:5.6 ~gamma:1.3) qf_range) "q_cauchy.csv";
  save (List.map (q_exponential ~lambda:2.356) qf_range) "q_exponential.csv";
  save (List.map (q_laplace ~mu:(-.53.) ~b:12.) qf_range) "q_laplace.csv";
  save (List.map (q_logistic ~mu:(-.0.004) ~s:0.02) qf_range) "q_logistic.csv";
  save (List.map float (List.map (q_poisson ~lambda:4.3) qf_range)) "q_poisson.csv";
  save (List.map (q_uniform ~a:13.444 ~b:56.876) qf_range) "q_uniform.csv"


(* Test and save CDFs *)

let save_cdfs () =
  save (List.map (c_bernoulli ~p:0.6) [true; false; false; true]) "c_bernoulli.csv";
  save (List.map (c_binomial ~n:30. ~p:0.34) (range 0. 30. 30)) "c_binomial.csv";
  save (List.map (c_categorical ~classes:[1;2;3;4;5] ~probs:[0.26;0.49;0.08;0.17;0.0]) [4;4;3;2;5;1;4]) "c_categorical.csv";
  save (List.map (c_cauchy ~x_0:(-.5.3) ~gamma:4.0) (range (-.25.66) 198.1 100)) "c_cauchy.csv";
  save (List.map (c_exponential ~lambda:2.8) (range (-.1.22) 13.99 100)) "c_exponential.csv";
  save (List.map (c_laplace ~mu:3.01 ~b:0.667) (range (-.1.22) 5.99 100)) "c_laplace.csv";
  save (List.map (c_logistic ~mu:15.9 ~s:4.32) (range (-.14.2) 53.29 100)) "c_logistic.csv";
  save (List.map (c_poisson ~lambda:36.3) (range 0. 100. 100)) "c_poisson.csv";
  save (List.map (c_uniform ~a:(10.334) ~b:10009.8) (range (-.1440.2) 13000.4 100)) "c_uniform.csv"


 (* Test and save PDFs *)

let save_pdfs () =
  save (List.map (d_beta ~a:2.9 ~b:6.7) qf_range) "d_beta.csv";
  save (List.map (d_gamma ~k:3. ~theta:3.8) (range 0. 27.1 100)) "d_gamma.csv";
  save (List.map (d_gaussian ~mu:5900.5 ~sigma:434.5) (range 3603.8 7321.1 100)) "d_gaussian.csv";
  save (List.map (d_lognormal ~mu:12.4 ~sigma:3.111) (range 10.66 13.6 100)) "d_lognormal.csv"


(* Sampling modules *)

(* module My_Bernoulli = Bernoulli (struct let p = 0.3 end) (struct let c = None end)
module My_Beta = Beta (struct let a = 2.3 let b = 4.9 end) (struct let c = None end)
module My_Binomial = Binomial (struct let n = 40 let p = 0.62 end) (struct let c = None end)
module My_Categorical = Categorical (struct type t = int let classes = [1;2;3;4;5] let probs = [0.1;0.6;0.02;0.08;0.2] end) (struct type t = int let c = None end)
module My_Cauchy = Cauchy (struct let x_0 = 6.2 let gamma = 1.1 end) (struct let c = None end)
module My_Exponential = Exponential (struct let lambda = 3.76 end) (struct let c = None end)
module My_Gamma = Gamma (struct let k = 4.3 let theta = 3.4 end) (struct let c = None end)
module My_Gaussian = Gaussian (struct let mu = 100. let sigma = 15. end) (struct let c = None end)
module My_Laplace = Laplace (struct let mu = -13.9 let b = 4.4 end) (struct let c = None end)
module My_Logistic = Logistic (struct let mu = 0.9 let s = 0.22 end) (struct let c = None end)
module My_LogNormal = LogNormal (struct let mu = 0.2 let sigma = 0.8 end) (struct let c = None end)
module My_Poisson = Poisson (struct let lambda = 14.5 end) (struct let c = None end)
module My_Uniform = Uniform (struct let a = -209.6 let b = -44.7 end) (struct let c = None end) *)


(* Sampling modules with constraints *)

module My_Bernoulli = Bernoulli (struct let p = 0.3 end) (struct let c = Some false end)
module My_Beta = Beta (struct let a = 2.3 let b = 4.9 end) (struct let c = Some [(-0.1,0.28); (0.34,0.38); (0.5,0.9)] end)
module My_Binomial = Binomial (struct let n = 40 let p = 0.62 end) (struct let c = Some [(0,7); (12,12); (17,26); (30,40)] end)
module My_Categorical = Categorical (struct type t = int let classes = [1;2;3;4;5] let probs = [0.1;0.6;0.02;0.08;0.2] end) (struct type t = int let c = Some [1; 2; 5] end)
module My_Cauchy = Cauchy (struct let x_0 = 6.2 let gamma = 1.1 end) (struct let c = Some [(0.,5.4); (12.,13.)] end)
module My_Exponential = Exponential (struct let lambda = 3.76 end) (struct let c = Some [(-.0.3,-.0.2); (0.4,0.99); (2.01,2.8)] end)
module My_Gamma = Gamma (struct let k = 4.3 let theta = 3.4 end) (struct let c = Some [(11.,20.); (24.,100.)] end)
module My_Gaussian = Gaussian (struct let mu = 100. let sigma = 15. end) (struct let c = Some [(30.,50.); (100.,150.); (155.,170.)] end)
module My_Laplace = Laplace (struct let mu = -13.9 let b = 4.4 end) (struct let c = Some [(-20.,-10.)] end)
module My_Logistic = Logistic (struct let mu = 0.9 let s = 0.22 end) (struct let c = Some [(-.0.3,-.0.); (0.5,0.81); (1.5,15.)] end)
module My_LogNormal = LogNormal (struct let mu = 0.2 let sigma = 0.8 end) (struct let c = Some [(3.22,3.28); (3.57,3.67)] end)
module My_Poisson = Poisson (struct let lambda = 14.5 end) (struct let c = Some [(-7,-3); (4,12); (16,18); (30,60)] end)
module My_Uniform = Uniform (struct let a = -209.6 let b = -44.7 end) (struct let c = Some [(-.141.3,-.141.3); (-101.1,-75.5); (-50.6,-26.7)] end)


(* Inverse transform sampling tests *)

let inverse_transform () =
  save (List.map float_of_bool (My_Bernoulli.sample 10000)) "bernoulli.csv";
  save (List.map float (My_Binomial.sample 10000)) "binomial.csv";
  save (List.map float (My_Categorical.sample 10000)) "categorical.csv";
  save (My_Cauchy.sample 10000) "cauchy.csv";
  save (My_Exponential.sample 10000) "exponential.csv";
  save (My_Laplace.sample 10000) "laplace.csv";
  save (My_Logistic.sample 10000) "logistic.csv";
  save (List.map float (My_Poisson.sample 10000)) "poisson.csv";
  save (My_Uniform.sample 10000) "uniform.csv"


(* MCMC sampling tests *)

let mcmc () =
  save (My_Beta.sample 30000) "beta.csv";
  save (My_Gamma.sample 30000) "gamma.csv";
  save (My_Gaussian.sample 30000) "gaussian.csv";
  save (My_LogNormal.sample 30000) "lognormal.csv"


(* Run tests *)

let run_function_checks () = save_qfs (); save_cdfs (); save_pdfs ()

let run_sampling_checks () = inverse_transform (); mcmc ()