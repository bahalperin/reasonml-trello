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

type dragCard = dragItem(card, cardDropLocation);

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