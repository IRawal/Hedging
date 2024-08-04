(* Multiply each series by a constant weight*)
let apply_weights (weights: float list) (series: float list list) = 
  List.map2 (fun w xs -> List.map (fun x -> w *. x) xs) weights series

(* Sum down columns*)
let rec sum_axis_1 (series: float list list) =
  match series with [] -> [] 
  | [x] -> [x] 
  | [x1; x2] -> [List.map2 (fun x y -> x +. y) x1 x2] 
  | h::l::t -> List.concat [(sum_axis_1 [h; l]); t]

(* Multiply a vector by a matrix -> vector*)
let vec_mul vec mat = 
  List.nth (apply_weights vec mat |> sum_axis_1) 0

let sum (l: float list) : float = 
  List.fold_left (fun acc x -> acc +. x) 0. l

let mean (l: float list) : float = 
  sum l /. (float @@ List.length l)

let var (l: float list) : float = 
  let mu = mean l in 
  List.map (fun x -> (x -. mu) ** 2.) l |> sum


let calc_dw (weights: float list) (series: float list list) = 
  (* Price of portfolio at each time stamp *)
  let price_vec: float list = vec_mul weights series in

  let timestamps: int = List.length price_vec in

  (* Partial of mean with respect to each weight *)
  let mu: float = mean price_vec in 
  let dmus: float list = List.map (fun l -> (sum l) /. float timestamps) series in

  (* Use chain rule to compute partial of variance *)
  let pre_chain: float list = List.map (fun p -> 2. *. (p -. mu)) price_vec in
  let chains = List.map2 (fun dmu s -> (List.map (fun p -> p -. dmu) s)) dmus series in

  (* Multiply partial with chain rule and sum up individual variance terms*)
  List.map (fun chain -> List.map2 (fun c pc -> c *. pc) chain pre_chain) chains |> List.map (fun terms -> sum terms)

let drop_first l: float list =
  match l with [] -> [] | h::t -> t

(* Gradient descent to minimize variance *)
let rec optimize (weights: float list) (series: float list list) (lr: float) momentum ?prev_dw epochs =

  let dws = drop_first @@ calc_dw weights series |> List.map (fun dw -> dw *. lr) in
  let prevs = match prev_dw with Some x -> x | _ -> List.map (fun dw -> 0.) dws in

  let moment_grad = List.map2 (fun dw prev -> (dw +. momentum *. prev)) dws prevs in

  let updated = List.map2 (fun param dw -> (param -. dw)) (drop_first weights) moment_grad in
  
  let new_weights = List.concat [[List.hd weights]; updated] in
  if epochs == 1 then
    new_weights
  else
    optimize new_weights series lr momentum ~prev_dw:moment_grad (epochs - 1)