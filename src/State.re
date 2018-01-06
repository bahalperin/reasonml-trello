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

type t = {
  board: Board.t,
  newListForm,
  newCardForm: option(newCardForm),
  editListCid: option(CardList.cid),
  editListInputRef: ref(option(Dom.element)),
  isEditBoardNameFormOpen: bool,
  drag: option(dragState)
};

let init = () => {
  board: Board.init(),
  newListForm: {name: "", isOpen: false, inputRef: ref(None)},
  newCardForm: None,
  drag: None,
  editListCid: None,
  editListInputRef: ref(None),
  isEditBoardNameFormOpen: false
};