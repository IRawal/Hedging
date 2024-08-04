(* Deprecated in favor of Gradient Descent *)

type point = {
  x: float;
  y: float;
}

let sphere (p: point) : float = 
  p.x ** 2.0 +. p.y ** 2.0

let bukin_n6 (p: point) : float =
  let x = p.x in
  let y = p.y in
  let term1 = 100.0 *. sqrt (abs_float (y -. 0.01 *. x *. x)) in
  let term2 = 0.01 *. abs_float (x +. 10.0) in
  term1 +. term2

let rec nelderMead (f: point -> float) (points: point list) (iters: int) : point list = 
  if iters == 0 then
    points
  else
  
  let evals = List.map (fun p -> (p, f p)) points in
  let sorted_points = List.sort (fun (_, val0) (_, val1) -> compare val0 val1) evals |> List.map (fun ev -> fst ev) in

  let b = List.nth sorted_points 0 in
  let g = List.nth sorted_points 1 in
  let w = List.nth sorted_points 2 in
  
  let m: point = {x = (b.x +. g.x) /. 2.; y = (b.y +. g.y) /. 2.} in
  let r: point = {x = 2. *. m.x -. 2. *. w.x; y = 2. *. m.y -. 2. *. w.y} in

  if f r < f g then
    if f b < f r then
      (* return r g b*)
      nelderMead f [b; g; r] (iters - 1)
    else
      let e: point = {x = 2. *. r.x -. m.x; y = 2. *. r.y -. m.y} in
      if f e < f b then
        (* return e g b*)
        nelderMead f  [e; g; b] (iters - 1)
      else
        nelderMead f [b; g; r] (iters - 1)
        (* return r g b*)
  else
    if f r < f w then
      (* return r g b*)
      nelderMead f [b; g; r] (iters - 1)
    else
      let c1: point = {x = (w.x +. m.x) /. 2.; y = (w.y +. m.y) /. 2.} in
      let c2: point = {x = (m.x +. r.x) /. 2.; y = (m.y +. r.y) /. 2.} in
      let c: point = [(c1, f c1); (c2, f c2)] |> List.sort (fun (p0, _) (p1, _) -> compare p0 p1) |> fun l -> List.nth l 0 |> fst in
      if f c < f w then
        (* return c g b*)
        nelderMead f [b; g; c] (iters - 1)
      else
        let s: point = {x = (w.x +. b.x) /. 2.; y = (w.y +. b.y) /. 2.} in
        (* return  s m b*)
        nelderMead f [b; m; s] (iters - 1)

        
let main () = 
  let p0: point = {x = 1.0; y = 2.0} in
  let p1: point = {x = -1.0; y = 2.0} in
  let p2: point = {x = 1.0; y = -2.0} in

  nelderMead sphere [p0; p1; p2] 10;;

main ()