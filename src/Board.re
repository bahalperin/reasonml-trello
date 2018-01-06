type cid = string;

type t = {
  cid,
  name: string,
  lists: list(CardList.t)
};

let cidFromString = Utils.identity;

let encode = ({name, lists}) =>
  Json.Encode.(
    object_([
      ("cid", string(name)),
      ("name", string(name)),
      ("lists", lists |> List.map(CardList.encode) |> Array.of_list |> jsonArray)
    ])
  );

let decodeHelper = (json) =>
  Json.Decode.{
    cid: field("cid", string, json),
    name: field("name", string, json),
    lists: field("lists", list(CardList.decode), json)
  };

let decode = (json) =>
  try (Some(decodeHelper(json))) {
  | _ => None
  };

let localStorageKey = "board";

let saveLocally = (board) =>
  Dom.Storage.setItem(
    localStorageKey,
    board |> encode |> Js.Json.stringify,
    Dom.Storage.localStorage
  );

let getFromLocalStorage = () =>
  Dom.Storage.getItem(localStorageKey, Dom.Storage.localStorage)
  |> Option.andThen((jsonString) => decode(Js.Json.parseExn(jsonString)));

let defaultInitialBoard = () => {
  cid: "1",
  name: "Welcome board",
  lists: [
    CardList.create(
      ~cid="1",
      ~name="This is a list",
      ~cards=[
        Card.create(~cid="1", ~name="This is a card", ()),
        Card.create(~cid="2", ~name="This is also a card", ())
      ],
      ()
    ),
    CardList.create(
      ~cid="2",
      ~name="This is another list",
      ~cards=[
        Card.create(~cid="3", ~name="This is a card", ()),
        Card.create(~cid="4", ~name="This is also a card", ())
      ],
      ()
    )
  ]
};

let init = () => {
  let maybeBoard = getFromLocalStorage();
  Option.withDefault(defaultInitialBoard(), maybeBoard)
};