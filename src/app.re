open State;

type action =
  /* Adding lists */
  | AddList
  | AddListHelper(string)
  | OpenNewListForm
  | CloseNewListForm
  | SetNewListName(string)
  /* Adding cards */
  | AddCardToList(string)
  | AddCardToListHelper(string, string)
  | OpenNewCardForm(string)
  | CloseNewCardForm
  | SetNewCardName(string)
  /* Dragging */
  | MoveDraggedItem(int, int)
  | StartDragging(dragState)
  | SetDropLocation(dropLocation)
  | StopDragging
  /* Edit list name */
  | StartEditingListName(string)
  | StopEditingListName
  | EditListName(string, string)
  /* Edit board name */
  | StartEditingBoardName
  | CloseEditBoardNameForm
  | ToggleEditingBoardName
  | SetBoardName(string)
  /* Misc */
  | CloseAllOpenForms
  | NoOp;

let localStorageKey = "board";

let saveBoardLocally = (board) =>
  Dom.Storage.setItem(
    localStorageKey,
    board |> State.encodeBoard |> Js.Json.stringify,
    Dom.Storage.localStorage
  );

let getBoardFromLocalStorage = () => {
  let maybeResult = Dom.Storage.getItem(localStorageKey, Dom.Storage.localStorage);
  Option.map((jsonString) => State.decodeBoard(Js.Json.parseExn(jsonString)), maybeResult)
};

let defaultInitialBoard = () => {
  name: "Welcome board",
  lists: [
    {
      cid: "1",
      name: "This is a list",
      cards: [{cid: "1", name: "This is a card"}, {cid: "2", name: "This is also a card"}]
    },
    {
      cid: "2",
      name: "This is another list",
      cards: [{cid: "3", name: "This is a card"}, {cid: "4", name: "This is also a card"}]
    }
  ]
};

let initialState = () => {
  let maybeBoard = getBoardFromLocalStorage();
  let board = Option.withDefault(defaultInitialBoard(), maybeBoard);
  {
    board,
    newListForm: {name: "", isOpen: false, inputRef: ref(None)},
    newCardForm: None,
    drag: None,
    editListCid: None,
    editListInputRef: ref(None),
    isEditBoardNameFormOpen: false
  }
};

let reducer = (action, state) =>
  switch action {
  | AddList => ReasonReact.SideEffects(((self) => self.reduce(() => AddListHelper(Uuid.v4()), ())))
  | AddListHelper(cid) =>
    ReasonReact.UpdateWithSideEffects(
      {
        ...state,
        newListForm: {...state.newListForm, name: ""},
        board: {
          ...state.board,
          lists: List.append(state.board.lists, [{cid, name: state.newListForm.name, cards: []}])
        }
      },
      (({state}) => saveBoardLocally(state.board))
    )
  | SetNewListName(newListName) =>
    ReasonReact.Update({...state, newListForm: {...state.newListForm, name: newListName}})
  | AddCardToList(listCid) =>
    ReasonReact.SideEffects(
      ((self) => self.reduce(() => AddCardToListHelper(listCid, Uuid.v4()), ()))
    )
  | AddCardToListHelper(listCid, cardCid) =>
    switch state.newCardForm {
    | Some(newCardForm) =>
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          newCardForm: Some({...newCardForm, name: ""}),
          board: {
            ...state.board,
            lists:
              List.map(
                (list) =>
                  list.cid === listCid ?
                    {
                      ...list,
                      cards: List.append(list.cards, [{cid: cardCid, name: newCardForm.name}])
                    } :
                    list,
                state.board.lists
              )
          }
        },
        (
          ({state}) => {
            state.newCardForm
            |> Option.run(
                 (newCardForm: State.newCardForm) => Utils.Dom.focusElement(newCardForm.inputRef)
               );
            saveBoardLocally(state.board)
          }
        )
      )
    | None => ReasonReact.NoUpdate
    }
  | SetNewCardName(newCardName) =>
    switch state.newCardForm {
    | Some(newCardForm) =>
      ReasonReact.Update({...state, newCardForm: Some({...newCardForm, name: newCardName})})
    | None => ReasonReact.NoUpdate
    }
  | OpenNewCardForm(listCid) =>
    ReasonReact.UpdateWithSideEffects(
      {...state, newCardForm: Some({name: "", inputRef: ref(None), listCid})},
      (
        ({state}) =>
          state.newCardForm
          |> Option.run(
               (newCardForm: State.newCardForm) => Utils.Dom.focusElement(newCardForm.inputRef)
             )
      )
    )
  | CloseNewCardForm => ReasonReact.Update({...state, newCardForm: None})
  | MoveDraggedItem(x, y) =>
    switch state.drag {
    | Some(drag) =>
      ReasonReact.Update({
        ...state,
        drag: Some({...drag, movement: Moving, mousePosition: (x, y)})
      })
    | None => ReasonReact.NoUpdate
    }
  | StartDragging(drag) =>
    switch drag.target {
    | List({item: cardList}) =>
      ReasonReact.Update({
        ...state,
        board: {
          ...state.board,
          lists: List.filter((list) => list.cid !== cardList.cid, state.board.lists)
        },
        drag: Some(drag)
      })
    | Card({item: card}) =>
      ReasonReact.Update({
        ...state,
        board: {
          ...state.board,
          lists:
            List.map(
              (list: cardList) => {
                ...list,
                cards: List.filter((c: card) => card.cid !== c.cid, list.cards)
              },
              state.board.lists
            )
        },
        drag: Some(drag)
      })
    }
  | SetDropLocation(dropLocation) =>
    switch state.drag {
    | Some(drag) =>
      switch (drag.target, dropLocation) {
      | (List(target), List(dropLocation)) =>
        ReasonReact.Update({
          ...state,
          drag: Some({...drag, target: List({...target, dropLocation})})
        })
      | (Card(target), Card(dropLocation)) =>
        ReasonReact.Update({
          ...state,
          drag: Some({...drag, target: Card({...target, dropLocation})})
        })
      | (List(_), Card(_)) => ReasonReact.NoUpdate
      | (Card(_), List(_)) => ReasonReact.NoUpdate
      }
    | None => ReasonReact.NoUpdate
    }
  | StopDragging =>
    switch state.drag {
    | Some(drag) =>
      switch drag.target {
      | List({item: list, dropLocation: index}) =>
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            board: {
              ...state.board,
              lists: Utils.List.insertAt(~index, ~elem=list, ~list=state.board.lists)
            },
            drag: None
          },
          (({state}) => saveBoardLocally(state.board))
        )
      | Card({item: card, dropLocation: (listCid, index)}) =>
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            board: {
              ...state.board,
              lists:
                List.map(
                  (list) =>
                    list.cid === listCid ?
                      {...list, cards: Utils.List.insertAt(~index, ~elem=card, ~list=list.cards)} :
                      list,
                  state.board.lists
                )
            },
            drag: None
          },
          (({state}) => saveBoardLocally(state.board))
        )
      }
    | None => ReasonReact.NoUpdate
    }
  | OpenNewListForm =>
    ReasonReact.UpdateWithSideEffects(
      {...state, newListForm: {...state.newListForm, isOpen: true}},
      (({state}) => Utils.Dom.focusElement(state.newListForm.inputRef))
    )
  | CloseNewListForm =>
    ReasonReact.Update({...state, newListForm: {...state.newListForm, isOpen: false}})
  | EditListName(cid, name) =>
    ReasonReact.Update({
      ...state,
      board: {
        ...state.board,
        lists: List.map((list) => list.cid === cid ? {...list, name} : list, state.board.lists)
      }
    })
  | StartEditingListName(cid) =>
    ReasonReact.UpdateWithSideEffects(
      {...state, editListCid: Some(cid)},
      (({state}) => Utils.Dom.focusAndHighlightElement(state.editListInputRef))
    )
  | StopEditingListName => ReasonReact.Update({...state, editListCid: None})
  | CloseAllOpenForms =>
    ReasonReact.Update({
      ...state,
      newListForm: {...state.newListForm, isOpen: false},
      newCardForm: None,
      editListCid: None,
      isEditBoardNameFormOpen: false
    })
  | ToggleEditingBoardName =>
    ReasonReact.Update({...state, isEditBoardNameFormOpen: ! state.isEditBoardNameFormOpen})
  | StartEditingBoardName => ReasonReact.Update({...state, isEditBoardNameFormOpen: true})
  | CloseEditBoardNameForm => ReasonReact.Update({...state, isEditBoardNameFormOpen: false})
  | SetBoardName(name) =>
    ReasonReact.UpdateWithSideEffects(
      {...state, board: {...state.board, name}, isEditBoardNameFormOpen: false},
      (({state}) => saveBoardLocally(state.board))
    )
  | NoOp => ReasonReact.NoUpdate
  };

let component = ReasonReact.reducerComponent("App");

let listsForDisplay = (state) =>
  switch state.drag {
  | Some(drag) =>
    switch drag.target {
    | List({item: list, dropLocation: index}) =>
      Utils.List.insertAt(~index, ~elem=list, ~list=state.board.lists)
    | Card({item: card, dropLocation: (listCid, index)}) =>
      List.map(
        (list) =>
          list.cid === listCid ?
            {...list, cards: Utils.List.insertAt(~index, ~elem=card, ~list=list.cards)} : list,
        state.board.lists
      )
    }
  | None => state.board.lists
  };

let isListBeingDragged = (~list, ~drag) =>
  switch drag {
  | Some(drag) =>
    switch drag.target {
    | List({item}) => item.cid === list.cid && drag.movement === Moving
    | Card(_) => false
    }
  | None => false
  };

let handleKeyDown = (event) => {
  let keyCode = ReactEventRe.Keyboard.keyCode(event);
  switch keyCode {
  | 27 => CloseAllOpenForms
  | _ => NoOp
  }
};

let stopDragging = (_event) => StopDragging;

let toggleEditingBoardName = (_event) => ToggleEditingBoardName;

let changeListName = (list, event) => EditListName(list.cid, Utils.getValueFromEvent(event));

let startEditingListName = (list, ()) => {
  Js.log("editing");
  StartEditingListName(list.cid)
};

let stopEditingListName = () => StopEditingListName;

let startDraggingList = (~list, ~index, ~event) =>
  StartDragging({
    movement: Started,
    target: List({item: list, dropLocation: index}),
    mousePosition: Utils.getMousePositionFromEvent(event),
    initialClickOffset: Utils.getClickOffsetFromEvent(event)
  });

let startDraggingCard = (~card, ~listCid, ~cardIndex, ~event) =>
  StartDragging({
    movement: Started,
    target: Card({item: card, dropLocation: (listCid, cardIndex)}),
    mousePosition: Utils.getMousePositionFromEvent(event),
    /* TODO: this may not work on all browsers, so should probably be treated as an option type */
    initialClickOffset: Utils.getClickOffsetFromEvent(event)
  });

let setDropLocation = (~drag, ~list, ~listIndex, _event) =>
  switch drag {
  | Some(drag) =>
    switch drag.target {
    | List(_) => SetDropLocation(List(listIndex))
    | Card(_) => SetDropLocation(Card((list.cid, List.length(list.cards))))
    }
  | None => NoOp
  };

let make = (_children) => {
  ...component,
  initialState,
  reducer,
  render: ({state, reduce, handle}) =>
    View.(
      <Container
        drag=state.drag
        onMouseMove=(
          reduce(
            (event) => {
              let (x, y) = Utils.getMousePositionFromEvent(event);
              MoveDraggedItem(x, y)
            }
          )
        )
        onKeyDown=(reduce(handleKeyDown))
        onMouseUp=(reduce(stopDragging))>
        <AppHeader />
        <BoardHeader boardName=state.board.name openForm=(reduce(toggleEditingBoardName)) />
        <div className="flex-auto flex flex-row overflow-x-scroll">
          (
            state
            |> listsForDisplay
            |> List.mapi(
                 (index, list: cardList) =>
                   <CardList
                     key=list.cid
                     list
                     showPlaceholderOnly=(isListBeingDragged(~list, ~drag=state.drag))
                     isEditingName=(
                       switch state.editListCid {
                       | Some(cid) => cid === list.cid
                       | None => false
                       }
                     )
                     changeListName=(reduce(changeListName(list)))
                     openForm=(reduce(() => startEditingListName(list, ())))
                     closeForm=(reduce(stopEditingListName))
                     onMouseEnter=(
                       reduce(setDropLocation(~list, ~listIndex=index, ~drag=state.drag))
                     )
                     onMouseDown=(reduce((event) => startDraggingList(~list, ~index, ~event)))
                     setInputRef=(
                       handle(
                         (theRef, {state}) => state.editListInputRef := Js.Nullable.to_opt(theRef)
                       )
                     )
                     viewCard=(
                       (cardIndex, card: State.card) =>
                         <Card
                           card
                           onMouseEnter=(
                             reduce((_event) => SetDropLocation(Card((list.cid, cardIndex))))
                           )
                           showPlaceholderOnly=(
                             switch state.drag {
                             | Some(drag) =>
                               switch drag.target {
                               | Card({item: draggedCard}) =>
                                 draggedCard.cid === card.cid && drag.movement === Moving
                               | List(_) => false
                               }
                             | None => false
                             }
                           )
                           onDragStart=(
                             reduce(
                               (event) =>
                                 startDraggingCard(~card, ~listCid=list.cid, ~cardIndex, ~event)
                             )
                           )
                         />
                     )>
                     <NewCardForm
                       listCid=list.cid
                       newCardForm=state.newCardForm
                       changeNewCardName=(
                         reduce((event) => SetNewCardName(Utils.getValueFromEvent(event)))
                       )
                       addCard=(reduce((_event) => AddCardToList(list.cid)))
                       openForm=(reduce((_event) => OpenNewCardForm(list.cid)))
                       closeForm=(reduce((_event) => CloseNewCardForm))
                       setInputRef=(
                         handle(
                           (theRef, {state}) =>
                             Option.run(
                               (newCardForm: State.newCardForm) =>
                                 newCardForm.inputRef := Js.Nullable.to_opt(theRef),
                               state.newCardForm
                             )
                         )
                       )
                     />
                   </CardList>
               )
            |> Array.of_list
            |> ReasonReact.arrayToElement
          )
          <AddListForm
            addList=(reduce((_event) => AddList))
            newListForm=state.newListForm
            changeNewListName=(reduce((event) => SetNewListName(Utils.getValueFromEvent(event))))
            openForm=(reduce((_event) => OpenNewListForm))
            closeForm=(reduce((_event) => CloseNewListForm))
            setInputRef=(
              handle((theRef, {state}) => state.newListForm.inputRef := Js.Nullable.to_opt(theRef))
            )
          />
        </div>
        <DraggedItem drag=state.drag newCardForm=state.newCardForm />
        (
          state.isEditBoardNameFormOpen ?
            <EditBoardNamePopup
              boardName=state.board.name
              closeForm=(reduce(() => CloseEditBoardNameForm))
              onSubmit=(reduce((name) => SetBoardName(name)))
            /> :
            ReasonReact.nullElement
        )
      </Container>
    )
};