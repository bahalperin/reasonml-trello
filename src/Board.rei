type cid;

type t = {
  cid,
  name: string,
  lists: list(CardList.t)
};

let cidFromString: string => cid;

let encode: Json.Encode.encoder(t);

let decode: Js.Json.t => option(t);

let saveLocally: t => unit;

let getFromLocalStorage: unit => option(t);

let init: unit => t;