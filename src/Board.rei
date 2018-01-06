type t = {
  name: string,
  lists: list(CardList.t)
};

let encode: Json.Encode.encoder(t);

let decode: Json.Decode.decoder(t);

let saveLocally: t => unit;

let getFromLocalStorage: unit => option(t);

let init: unit => t;