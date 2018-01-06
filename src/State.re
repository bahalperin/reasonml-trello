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

type t = {
  board: Board.t,
  newListForm,
  newCardForm: option(newCardForm),
  editListCid: option(CardList.cid),
  editListInputRef: ref(option(Dom.element)),
  isEditBoardNameFormOpen: bool,
  drag: Drag.t
};

let init = () => {
  board: Board.init(),
  newListForm: {name: "", isOpen: false, inputRef: ref(None)},
  newCardForm: None,
  drag: Drag.init(),
  editListCid: None,
  editListInputRef: ref(None),
  isEditBoardNameFormOpen: false
};