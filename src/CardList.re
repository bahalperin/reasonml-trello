type cid = string;

type t = {
  cid,
  name: string,
  cards: list(Card.t),
  wasJustAdded: bool
};

let create = (~cid, ~name="", ~cards=[], ()) => {cid, name, cards, wasJustAdded: true};

let update = (~list, ~wasJustAdded=list.wasJustAdded, ()) => {...list, wasJustAdded};

let cidToString = Utils.identity;

let cidFromString = Utils.identity;

let encode = ({cid, name, cards}) =>
  Json.Encode.(
    object_([
      ("cid", string(cid)),
      ("name", string(name)),
      ("cardsList", cards |> List.map(Card.encode) |> Array.of_list |> jsonArray)
    ])
  );

let decode = (json) =>
  Json.Decode.{
    cid: field("cid", string, json),
    name: field("name", string, json),
    cards: field("cardsList", list(Card.decode), json),
    wasJustAdded: false
  };