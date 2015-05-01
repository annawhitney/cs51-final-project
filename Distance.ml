open Core.Std

(* I'm testing a few of my functions here 
 * thanks,
 * Jacques *)

(*radius of the earth - needed in the distance formula *)
(* note: we are using km *)
let earthrad = 6371.0 

let pi = 4. *. atan 1.

(* haversin = (sin(theta/2))^2 *)
let haversin (x:float) : float =
  (sin (x /. 2.0)) ** 2.0

(* converts lat and long pairs from degrees into radians *)
let coords_to_rad (x: float * float) : float * float = 
  let radians =  ( *.) (pi /. 180.0) in
  match x with 
  | (a, b) -> (radians a, radians b)

(* our spherical distance function is all based on the Haversine Formula -
 * see https://www.youtube.com/watch?v=y1JZNTRDn_w *
 * also see http://www.movable-type.co.uk/scripts/latlong.html *)

let distance (cutoff: float) (st: float * float)
 (fin: float * float) : float option = 
  let _ = Printf.printf "I'm inside the distance function!\n" in
  let (lat1, long1) = coords_to_rad st in 
  let (lat2, long2) = coords_to_rad fin in
  let a = haversin (lat2 -. lat1) +. ((cos lat1) *. (cos lat2) *. 
    haversin (long2 -. long1)) in
  let dis = 2.0 *. earthrad *. (atan2 (sqrt a) (sqrt (1.0 -. a))) in  
  if dis < cutoff then Some dis else None 
