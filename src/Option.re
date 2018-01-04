let isSome = (x) =>
  switch x {
  | Some(_) => true
  | None => false
  };

let map = (fn, x) =>
  switch x {
  | Some(value) => Some(fn(value))
  | None => None
  };

let withDefault = (default, x) =>
  switch x {
  | Some(x) => x
  | None => default
  };

let run = (fn, x) =>
  switch x {
  | Some(value) =>
    fn(value);
    ()
  | None => ()
  };