type cid;

type t = {
  cid,
  name: string
};

let create: (~cid: string, ~name: string=?, unit) => t;

let cidToString: cid => string;

let cidFromString: string => cid;

let encode: Json.Encode.encoder(t);

let decode: Json.Decode.decoder(t);