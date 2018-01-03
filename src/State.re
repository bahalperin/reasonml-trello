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

type dragState = {
  list: cardList,
  mousePosition: (int, int),
  initialClickOffset: (int, int),
  dropTarget: int
};

type state = {
  board,
  newListName: string,
  newCardForm: option(newCardForm),
  drag: option(dragState)
};