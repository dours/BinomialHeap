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

(** 
   Binomial heap is a implementation of a priority queue - a data structure, 
   which supports fast addition of elements and finding that with the minimal priority.

   Binomial heap is a list of binomial trees.
   Binomial tree is a tree (not even binary) of size 2^k for some k. 
   It satisfies the main heap property - a priority of each node 
   is less or equal then priorities of all it's sons.

   Binomial tree of depth 0 is empty.

   Binomial tree of depth k+1 consists of a root node, which has k sons, 
   who are roots of binomial trees of depth 1, 2, ..., k.
   So, the size of binomial tree of depth k+1 is 2^k (by induction).
   
   The representation of a heap is tightly connected with 
   the binary representation of it's size: a tree of size 2^k is included in the 
   heap iff the k-th bit of the size of the heap is set to 1.
   
   The main operation for binomial heap is merge.
   It merges two binomial heaps in one in logarithmic time.
   The procedure of merging is similar to addition of two integers given by their
   binary representation, where adding two bits is merging two binomial
   trees (we can perform this in a constant time).
    
   Sparse representation of a heap is used - some trees can be empty.
*)

(** Heap element *)
module type Element =
  sig

    (** Principal type *)
    type t

    (** Comparison function *)
    val compare : t -> t -> int
    
    (** String conversion for debugging purposes *)
    val toString : t -> string

  end

(** BinaryHeap Constructor *)
module Make (X : Element) :
    sig
    
      (** The type of the heap *)
      type t
      
      (** [merge h1 h2] merges [h1] and [h2] *)
      val merge : t -> t -> t
      
      (** Fold function (no certain order of elements is guaranteed) *)
      val fold : ('a -> X.t -> 'a) -> 'a -> t -> 'a

      (** Iter function (no certain order of elements is guaranteed) *)      
      val iter : (X.t -> 'a) -> t -> unit
      
      (** Returns a minimal element of a heap *)
      val findMin : t -> X.t
      
      (** Returns a heap without its minimal element and that element too.
    	  You cannot remove any element, but only the minimal *)
      val removeMin : t -> t * X.t
      
      (** Add an element to a heap *)
      val add : t -> X.t -> t
            
      (** The empty heap *)
      val empty : t
      
      (** Check if the heap is empty *)
      val isEmpty : t -> bool

      (** String conversion *)      
      val toString : t -> string

    end
