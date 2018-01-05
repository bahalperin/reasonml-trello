type cid;

type t = {
  cid,
  name: string,
  cards: list(Card.t)
};

let create: (~cid: string, ~name: string=?, ~cards: list(Card.t)=?, unit) => t;

let cidToString: cid => string;

let cidFromString: string => cid;

let encode: Json.Encode.encoder(t);

let decode: Json.Decode.decoder(t);