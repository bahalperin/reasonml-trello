type board = {
  name: string,
  lists: list(CardList.t)
};

let encodeBoard = ({name, lists}) =>
  Json.Encode.(
    object_([
      ("name", string(name)),
      ("lists", lists |> List.map(CardList.encode) |> Array.of_list |> jsonArray)
    ])
  );

let decodeBoard = (json) =>
  Json.Decode.{
    name: field("name", string, json),
    lists: field("lists", list(CardList.decode), json)
  };

type newCardForm = {
  listCid: CardList.cid,
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

type cardDropLocation = (CardList.cid, int);

type dropLocation =
  | List(listDropLocation)
  | Card(cardDropLocation);

type dragItem('item, 'location) = {
  item: 'item,
  dropLocation: 'location
};

type dragList = dragItem(CardList.t, listDropLocation);

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
  editListCid: option(CardList.cid),
  editListInputRef: ref(option(Dom.element)),
  isEditBoardNameFormOpen: bool,
  drag: option(dragState)
};