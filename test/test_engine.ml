open Engine

let () = 
(* 5 minutes of ADA, BTC and ETH respectively *)
  let prices_matrix = [
    [1.1620499999999998; 1.168; 1.1753; 1.16585; 1.17255];
    [55896.285; 55948.685; 56013.785; 55903.575; 55899.995];
    [1965.845; 1969.645; 1975.595; 1969.335; 1970.965]
  ] in

  let weights = [1.; 1.; 1.] in

  let price_series = vec_mul weights prices_matrix in

  Printf.printf "Initial Variance: %f\n" @@ var price_series;

  let opt_weights = optimize weights prices_matrix 0.0001 0.1 100 in

  let opt_price_series = vec_mul opt_weights prices_matrix in

  List.iter (fun p -> Printf.printf "w: %f\n" p) opt_weights;

  Printf.printf "Final Variance: %f\n" @@ var opt_price_series