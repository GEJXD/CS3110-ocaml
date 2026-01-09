let double x = 2 * x
let square x = x * x

let quad x = double (double x)
let fourth x = square (square x)

(* the = opeartor is eqaul not assign,
  so we can do string replace like mathmenatuc expression *)
let twice f x = f (f x)
let quad' x = twice double x
let fourth' x = twice square x

let quad'' x = double x |> double
let fourth'' x = square x |> square

let quad''' = twice double

let pipeline x f = f x
let (|>) = pipeline
let x = 5 |> double

let compose f g x = f (g x)
let compose' f g x = g x |> f

let both f g x = (f x, g x)
let ds = both double square

let cond p f g x =
  if p x then f x else g x