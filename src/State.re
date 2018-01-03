type card = {
  cid: string,
  name: string
};

type cardList = {
  cid: string,
  name: string,
  cards: list(card)
};

type board = {
  name: string,
  lists: list(cardList)
};

type newCardForm = {
  listCid: string,
  name: string
};

type newListForm = {
  name: string,
  isOpen: bool
};

type dragMovement =
  | Started
  | Moving;

type dragTarget =
  | List(cardList, int)
  | Card(card, string, int);

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
  drag: option(dragState)
};