(**************************************************************************
 *  Copyright (C) 2005
 *  Oleg Medvedev (dours@mail.ru), St.Petersburg State University
 *  Universitetskii pr., 28, St.Petersburg, 198504, RUSSIA    
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 *
 *  See the GNU Lesser General Public License version 2.1 for more details
 *  (enclosed in the file COPYING).
 **************************************************************************)

module type Element = 
  sig

    type t

    val compare  : t -> t -> int
    val toString : t -> string

  end

module Make (X : Element) = 
  struct

    type tree = Empty | Root of X.t * t
    and  t = tree list

(* Tree means binomial tree here.*)
  
    let merge h1 h2 =
      let rec merge = function
      | [], [], Empty -> []
      | [], [],     z -> [z]
      | [],  y, Empty -> y
      | [],  y,     z -> merge ([z], y, Empty)
      |  x, [],     z -> merge ([], x, z)
      |  x,  y,     z -> 
	  let value, forward = join (List.hd x) (List.hd y) z in
	  value :: (merge ((List.tl x), (List.tl y), forward))
      and join t1 t2 t3 = 
          let join2 t1 t2 = match t1, t2 with
          | Empty, Empty -> Empty, Empty
          | Empty, Root _ -> t2, Empty
          | Root (x, h1), Empty -> t1, Empty
          | Root (x, h1), Root (y, h2) -> 
	       Empty, 
	       if X.compare x y < 0 then 
	          Root (x, List.append h1 [ Root (y, h2) ]) else 
	          Root (y, List.append h2 [ Root (x, h1) ]) 
          in
          let (value, forward) = join2 t1 t2 in
          if value = Empty then (t3, forward) else
          if t3 = Empty then (value, forward) else
         (snd (join2 t3 value), forward)
      in
      merge (h1, h2, Empty)

    let rec fold f x0 = List.fold_left 
	(fun x t -> match t with
	| Empty -> x
	| Root (u, h) -> f (fold f x h) u
	) x0

    let findMin h = 
	match List.fold_left 
	    (fun x t -> match x, t with
		| _, Empty -> x
		| None, Root (y, _) -> Some y
		| Some x, Root (y, _) -> 
		    if X.compare x y < 0 then Some x else Some y
	    ) None h
	with 
	    | None -> raise (Failure "empty heap in findMin")
	    | Some x -> x

    let removeMin = function
      | [] -> raise (Failure "empty heap during removeMin")
      | h ->
	  let u = findMin h in
	  let rec inner (tree :: rest) =
	    match tree with 
	    | Empty -> 
		let a, b, c = inner rest in
		Empty :: a, b, c
	    | Root (v, h1) ->
		if v == u then [], h1, rest
		else
		  let a, b, c = inner rest in
		  tree :: a, b, c 
	  in 
	  let h1, h2, rest = inner h in
	  (List.append (merge h1 h2) rest), u
	  
    let add h x = merge [Root (x, [])] h

    let empty = []

    let isEmpty h = List.for_all (fun x -> x = Empty) h

    let rec iter f = List.iter 
	(fun t -> match t with 
	| Empty -> ()
	| Root (x, h) -> begin f x; iter f h end
	)
	
    let toString = fold (fun s i -> Printf.sprintf "%s, %s" (X.toString i) s) ""
    
end
