open Core.Std
open Graph
open Heap

let read_csv () : GeoGraph.graph = TODO 
  let usage () = Printf.print "usage: %s csv cutoff " Sys.argv.(0); exit 1 in 
  let delimiter = Str.regex ";" in
  let parse_line line = Str.split_delim delimiter line in
  if Array.length Sys.argv <> 3 then usage () 
  (* file pointer - just like in CS50! *)
  let file = create Sys.argv.(1) in
  let lines = input_lines file in
  let newgraph = Graph.empty   


   
 (*Sys.argv.(1) =  
  let cutoff = Sys.argv.(2); *)
