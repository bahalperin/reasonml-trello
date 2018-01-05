type cardList = {
  cid: string,
  name: string,
  cards: list(Card.t)
};

let encodeCardList = ({cid, name, cards}) =>
  Json.Encode.(
    object_([
      ("cid", string(cid)),
      ("name", string(name)),
      ("cardsList", cards |> List.map(Card.encode) |> Array.of_list |> jsonArray)
    ])
  );

let decodeCardList = (json) =>
  Json.Decode.{
    cid: field("cid", string, json),
    name: field("name", string, json),
    cards: field("cardsList", list(Card.decode), json)
  };

type board = {
  name: string,
  lists: list(cardList)
};

let encodeBoard = ({name, lists}) =>
  Json.Encode.(
    object_([
      ("name", string(name)),
      ("lists", lists |> List.map(encodeCardList) |> Array.of_list |> jsonArray)
    ])
  );

let decodeBoard = (json) =>
  Json.Decode.{
    name: field("name", string, json),
    lists: field("lists", list(decodeCardList), json)
  };

type newCardForm = {
  listCid: string,
  name: string,
  inputRef: ref(option(Dom.element))
};

type newListForm = {
  name: string,
  isOpen: bool,
  inputRef: ref(option(Dom.element))
};

type dragMovement =
  | Started
  | Moving;

type listDropLocation = int;

type cardDropLocation = (string, int);

type dropLocation =
  | List(listDropLocation)
  | Card(cardDropLocation);

type dragItem('item, 'location) = {
  item: 'item,
  dropLocation: 'location
};

type dragList = dragItem(cardList, listDropLocation);

type dragCard = dragItem(Card.t, cardDropLocation);

type dragTarget =
  | List(dragList)
  | Card(dragCard);

type dragState = {
  movement: dragMovement,
  target: dragTarget,
  initialClickOffset: (int, int),
  mousePosition: (int, int)
};

type state = {
  board,
  newListForm,
  newCardForm: option(newCardForm),
  editListCid: option(string),
  editListInputRef: ref(option(Dom.element)),
  isEditBoardNameFormOpen: bool,
  drag: option(dragState)
};