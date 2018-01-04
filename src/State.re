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

type dragTarget =
  | List(cardList, int)
  | Card(card, string, int);

type dropTarget =
  | List(int)
  | Card(string, int);

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
  drag: option(dragState)
};