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

type dragState = {
  list: cardList,
  mousePosition: (int, int),
  initialClickOffset: (int, int),
  dropTarget: int
};

type state = {
  board,
  newListForm,
  newCardForm: option(newCardForm),
  drag: option(dragState)
};