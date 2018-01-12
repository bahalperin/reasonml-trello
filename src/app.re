type action =
  /* Adding lists */
  | AddList
  | AddListHelper(string)
  | AfterAddList(CardList.t)
  | OpenNewListForm
  | CloseNewListForm
  | SetNewListName(string)
  /* Adding cards */
  | AddCardToList(CardList.cid)
  | AddCardToListHelper(CardList.cid, string)
  | OpenNewCardForm(CardList.cid)
  | CloseNewCardForm
  | SetNewCardName(string)
  /* Dragging */
  | MoveDraggedItem(int, int)
  | StartDragging(Drag.t_)
  | SetDropLocation(Drag.dropLocation)
  | StopDragging
  /* Edit list name */
  | StartEditingListName(CardList.cid)
  | StopEditingListName
  | EditListName(CardList.cid, string)
  /* Edit board name */
  | StartEditingBoardName
  | CloseEditBoardNameForm
  | ToggleEditingBoardName
  | SetBoardName(string)
  /* Misc */
  | CloseAllOpenForms
  | NoOp;

let reducer = (action, state: State.t) =>
  switch action {
  | AddList => ReasonReact.SideEffects(((self) => self.send(AddListHelper(Uuid.v4()))))
  | AddListHelper(cid) =>
    let newList = CardList.create(~cid, ~name=state.newListForm.name, ());
    ReasonReact.UpdateWithSideEffects(
      {
        ...state,
        newListForm: {...state.newListForm, name: ""},
        board: {...state.board, lists: List.append(state.board.lists, [newList])}
      },
      (
        ({state, send}) => {
          Board.saveLocally(state.board);
          Js.Global.setTimeout(() => send(AfterAddList(newList)), 300) |> ignore
        }
      )
    )
  | AfterAddList({cid}) =>
    ReasonReact.Update({
      ...state,
      board: {
        ...state.board,
        lists:
          List.map(
            (list: CardList.t) =>
              list.cid === cid ? CardList.update(~list, ~wasJustAdded=false, ()) : list,
            state.board.lists
          )
      }
    })
  | SetNewListName(newListName) =>
    ReasonReact.Update({...state, newListForm: {...state.newListForm, name: newListName}})
  | AddCardToList(listCid) =>
    ReasonReact.SideEffects(((self) => self.send(AddCardToListHelper(listCid, Uuid.v4()))))
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
                (list: CardList.t) =>
                  list.cid === listCid ?
                    {
                      ...list,
                      cards:
                        List.append(
                          list.cards,
                          [Card.create(~cid=cardCid, ~name=newCardForm.name, ())]
                        )
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
            Board.saveLocally(state.board)
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
  | StartDragging((drag: Drag.t_)) =>
    switch drag.target {
    | List({item: cardList}) =>
      ReasonReact.Update({
        ...state,
        board: {
          ...state.board,
          lists: List.filter((list: CardList.t) => list.cid !== cardList.cid, state.board.lists)
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
              (list: CardList.t) => {
                ...list,
                cards: List.filter((c: Card.t) => card.cid !== c.cid, list.cards)
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
          (({state}) => Board.saveLocally(state.board))
        )
      | Card({item: card, dropLocation: (listCid, index)}) =>
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            board: {
              ...state.board,
              lists:
                List.map(
                  (list: CardList.t) =>
                    list.cid === listCid ?
                      {...list, cards: Utils.List.insertAt(~index, ~elem=card, ~list=list.cards)} :
                      list,
                  state.board.lists
                )
            },
            drag: None
          },
          (({state}) => Board.saveLocally(state.board))
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
    ReasonReact.UpdateWithSideEffects(
      {
        ...state,
        board: {
          ...state.board,
          lists:
            List.map(
              (list: CardList.t) => list.cid === cid ? {...list, name} : list,
              state.board.lists
            )
        }
      },
      (({state}) => Board.saveLocally(state.board))
    )
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
      (({state}) => Board.saveLocally(state.board))
    )
  | NoOp => ReasonReact.NoUpdate
  };

let component = ReasonReact.reducerComponent("App");

let listsForDisplay = (state: State.t) =>
  switch state.drag {
  | Some(drag) =>
    switch drag.target {
    | List({item: list, dropLocation: index}) =>
      Utils.List.insertAt(~index, ~elem=list, ~list=state.board.lists)
    | Card({item: card, dropLocation: (listCid, index)}) =>
      List.map(
        (list: CardList.t) =>
          list.cid === listCid ?
            {...list, cards: Utils.List.insertAt(~index, ~elem=card, ~list=list.cards)} : list,
        state.board.lists
      )
    }
  | None => state.board.lists
  };

let isListBeingDragged = (~list: CardList.t, ~drag: Drag.t) =>
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

let stopDragging = StopDragging;

let toggleEditingBoardName = ToggleEditingBoardName;

let changeListName = (list: CardList.t, name) => EditListName(list.cid, name);

let startEditingListName = (list: CardList.t, ()) => StartEditingListName(list.cid);

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

let setDropLocation = (~drag: Drag.t, ~list: CardList.t, ~listIndex) =>
  switch drag {
  | Some(drag) =>
    switch drag.target {
    | List(_) => SetDropLocation(List(listIndex))
    | Card(_) => SetDropLocation(Card((list.cid, List.length(list.cards))))
    }
  | None => NoOp
  };

type self = ReasonReact.self(State.t, ReasonReact.noRetainedProps, action);

let make = (_children) => {
  ...component,
  initialState: State.init,
  reducer,
  render: ({state, send, handle}: self) =>
    <View.Container
      drag=state.drag
      onMouseMove=(
        (event) => {
          let (x, y) = Utils.getMousePositionFromEvent(event);
          send(MoveDraggedItem(x, y))
        }
      )
      onKeyDown=((event) => send(handleKeyDown(event)))
      onMouseUp=((_event) => send(stopDragging))>
      <View.AppHeader />
      <View.BoardHeader
        boardName=state.board.name
        openForm=((_event) => send(toggleEditingBoardName))
      />
      <div className="flex-auto flex flex-row overflow-x-scroll">
        (
          state
          |> listsForDisplay
          |> List.mapi(
               (index, list: CardList.t) =>
                 <View.CardList
                   key=(CardList.cidToString(list.cid))
                   list
                   showPlaceholderOnly=(isListBeingDragged(~list, ~drag=state.drag))
                   isEditingName=(
                     switch state.editListCid {
                     | Some(cid) => cid === list.cid
                     | None => false
                     }
                   )
                   changeListName=(
                     (event) => send(changeListName(list, Utils.getValueFromEvent(event)))
                   )
                   openForm=(() => send(startEditingListName(list, ())))
                   closeForm=((_event) => send(stopEditingListName()))
                   onMouseEnter=(
                     (_event) => send(setDropLocation(~list, ~listIndex=index, ~drag=state.drag))
                   )
                   onMouseDown=((event) => send(startDraggingList(~list, ~index, ~event)))
                   setInputRef=(
                     handle(
                       (theRef, {state}) => state.editListInputRef := Js.Nullable.to_opt(theRef)
                     )
                   )
                   viewCard=(
                     (cardIndex, card) =>
                       <View.Card
                         card
                         onMouseEnter=(
                           (_event) => send(SetDropLocation(Card((list.cid, cardIndex))))
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
                           (event) =>
                             send(startDraggingCard(~card, ~listCid=list.cid, ~cardIndex, ~event))
                         )
                       />
                   )>
                   <View.NewCardForm
                     listCid=list.cid
                     newCardForm=state.newCardForm
                     changeNewCardName=(
                       (event) => send(SetNewCardName(Utils.getValueFromEvent(event)))
                     )
                     addCard=((_event) => send(AddCardToList(list.cid)))
                     openForm=((_event) => send(OpenNewCardForm(list.cid)))
                     closeForm=((_event) => send(CloseNewCardForm))
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
                 </View.CardList>
             )
          |> Array.of_list
          |> ReasonReact.arrayToElement
        )
        <View.AddListForm
          addList=((_event) => send(AddList))
          newListForm=state.newListForm
          changeNewListName=((event) => send(SetNewListName(Utils.getValueFromEvent(event))))
          openForm=((_event) => send(OpenNewListForm))
          closeForm=((_event) => send(CloseNewListForm))
          setInputRef=(
            handle((theRef, {state}) => state.newListForm.inputRef := Js.Nullable.to_opt(theRef))
          )
        />
      </div>
      <View.DraggedItem drag=state.drag newCardForm=state.newCardForm />
      (
        state.isEditBoardNameFormOpen ?
          <View.EditBoardNamePopup
            boardName=state.board.name
            closeForm=(() => send(CloseEditBoardNameForm))
            onSubmit=((name) => send(SetBoardName(name)))
          /> :
          ReasonReact.nullElement
      )
    </View.Container>
};