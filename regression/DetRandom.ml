
let id x = x

type t = { fileName : string; arrayLen : int; state : Random.State.t }
    
let loadState fname = 
	let inp = open_in fname in
	let n = (Scanf.fscanf inp "%i\n" id) in
	let a = Array.init n (fun _ -> Scanf.fscanf inp "%i\n" id) in
	close_in inp;
	{fileName = fname; arrayLen = n; state = Random.State.make a }
	
let setState { state = s } = Random.set_state s
    
let saveState { fileName = fn; arrayLen = n; state = s } = 
	let outp = open_out fn in
	Printf.fprintf outp "%i\n" n;
	for i = 0 to n - 1 do 
	    Printf.fprintf outp "%i\n" (Random.State.bits s)
	done;
	close_out outp

