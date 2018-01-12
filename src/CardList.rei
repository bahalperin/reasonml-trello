type cid;

type t = {
  cid,
  name: string,
  cards: list(Card.t),
  wasJustAdded: bool
};

let create: (~cid: string, ~name: string=?, ~cards: list(Card.t)=?, unit) => t;

let update: (~list: t, ~wasJustAdded: bool=?, unit) => t;

let cidToString: cid => string;

let cidFromString: string => cid;

let encode: Json.Encode.encoder(t);

let decode: Json.Decode.decoder(t);