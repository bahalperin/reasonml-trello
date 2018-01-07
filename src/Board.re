type t = {
  name: string,
  lists: list(CardList.t)
};

let encode = ({name, lists}) =>
  Json.Encode.(
    object_([
      ("name", string(name)),
      ("lists", lists |> List.map(CardList.encode) |> Array.of_list |> jsonArray)
    ])
  );

let decode = (json) =>
  Json.Decode.{
    name: field("name", string, json),
    lists: field("lists", list(CardList.decode), json)
  };

let localStorageKey = "board";

let saveLocally = (board) =>
  Dom.Storage.setItem(
    localStorageKey,
    board |> encode |> Js.Json.stringify,
    Dom.Storage.localStorage
  );

let getFromLocalStorage = () => {
  let maybeResult = Dom.Storage.getItem(localStorageKey, Dom.Storage.localStorage);
  Option.map((jsonString) => decode(Js.Json.parseExn(jsonString)), maybeResult)
};

let defaultInitialBoard = () => {
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