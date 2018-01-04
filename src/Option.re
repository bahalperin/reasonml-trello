let isSome = (x) =>
  switch x {
  | Some(_) => true
  | None => false
  };

let run = (fn, x) =>
  switch x {
  | Some(value) =>
    fn(value);
    ()
  | None => ()
  };