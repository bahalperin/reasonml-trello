type cid = string;

type t = {
  cid,
  name: string
};

let create = (~cid, ~name="", ()) => {cid, name};

let cidToString = Utils.identity;

let cidFromString = Utils.identity;

let encode = ({cid, name}) =>
  Json.Encode.(object_([("cid", string(cid)), ("name", string(name))]));

let decode = (json) =>
  Json.Decode.{cid: field("cid", string, json), name: field("name", string, json)};