module type X = sig
  val x : int
end

(* a functor is just a function that
  take a module as input and generate a
  new module as output *)
module IncX (M : X) = struct
  let x = M.x + 1
end

module A = struct let x = 0 end

module B = IncX(A)

module C = IncX(B)

(* or regrad it as a parameterized structure
  note Addx does not have a bind to name x *)
module AddX (M : X) = struct
  let add y = M.x + y
end

module Add42 = AddX (struct let x = 42 end)

(* Syntax:
  module F (M : S) = ...
end 
or the anonymous functor like fun
module F = functor (M : S) ...
*)

module type S = sig end
module Foo = struct end
module F (Foo : S) = struct (* definition of module *) end
module F = functor (Foo : S) -> struct (* definition of module *) end

(* can be parameterized on multiple structures *)
(* moudle F (M1 : S1) ... (Mn : Sn) = ... *)
(* module F -> functor (M1 : S1) ... (Mn : Sn) -> ... *)
