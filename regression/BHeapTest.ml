
module Integer = struct
  type t = int
  let compare = compare
  let infinity = max_int
  let toString = Printf.sprintf "%i "
end

module H = BinomialHeap.Make (Integer)

let failure = ref false

open DetRandom

let _ =
(*  Random.init 374091; *)
  let { state = s } as drs = loadState "randseed" in
  let last = ref 0. in
(*  while true do*)
  let testSize = 20000 in
    let h = ref H.empty in 
    let ar = Array.make (testSize + 1) min_int in
    begin
       for i = 1 to testSize do 
         let r = Random.State.bits s in
         h:= H.add (!h) r;
	 ar.(i) <- r
       done;
       let x = ref (H.findMin !h) in
    	 Array.sort compare ar;
	 Printf.printf "sorted %f \n" (Sys.time () -. !last);
         for i = 1 to (testSize - 1) do 
	   h := fst (H.removeMin (!h));
(*	   Printf.printf "%d\n" !x;*)
	   if not ((!x) = (ar.(i))) then begin 
	     Printf.printf "%s%!" "UUPS no such el"; 
	     failure := true; 
	   end;
	   if (!x)>(H.findMin (!h)) then begin 
	     Printf.printf "%s%!" "UUUPS"; 
	     failure := true; 
	   end;
	   x:=H.findMin  !h;
	 done
    end;
    Printf.printf "ok%f%s%!" (Sys.time () -. !last) "\n";
    last := Sys.time ();
(*  done;*)
  
  saveState drs;
  exit (if !failure then 1 else 0);
  