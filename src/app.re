open State;

type action =
  | AddList
  | AddListHelper(string)
  | SetNewListName(string)
  | AddCardToList(string)
  | AddCardToListHelper(string, string)
  | OpenNewListForm
  | CloseNewListForm
  | OpenNewCardForm(string)
  | CloseNewCardForm
  | SetNewCardName(string)
  | UpdateMousePosition(int, int)
  | StartDraggingList(cardList, int, (int, int), (int, int))
  | StartDraggingCard(card, string, int, (int, int), (int, int))
  | SetDropTargetForList(int)
  | SetDropTargetForCard(string, int)
  | StartEditingListName(string)
  | StopEditingListName
  | EditListName(string, string)
  | StopDragging
  | CloseAllOpenForms
  | ToggleOpenEditBoardNameForm
  | OpenEditBoardNameForm
  | CloseEditBoardNameForm
  | ChangeEditBoardFormName(string)
  | ChangeBoardName
  | NoOp;

let initialState = () => {
  board: {
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
  },
  newListForm: {name: "", isOpen: false, inputRef: ref(None)},
  newCardForm: None,
  drag: None,
  editListCid: None,
  editListInputRef: ref(None),
  editBoardNameForm: {isOpen: false, name: "", inputRef: ref(None), containerRef: ref(None)}
};

let reducer = (action, state) =>
  switch action {
  | AddList => ReasonReact.SideEffects(((self) => self.reduce(() => AddListHelper(Uuid.v4()), ())))
  | AddListHelper(cid) =>
    ReasonReact.Update({
      ...state,
      newListForm: {...state.newListForm, name: ""},
      board: {
        ...state.board,
        lists: List.append(state.board.lists, [{cid, name: state.newListForm.name, cards: []}])
      }
    })
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
          ({state}) =>
            state.newCardForm
            |> Option.run(
                 (newCardForm: State.newCardForm) => Utils.Dom.focusElement(newCardForm.inputRef)
               )
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
  | UpdateMousePosition(x, y) =>
    switch state.drag {
    | Some(drag) =>
      ReasonReact.Update({
        ...state,
        drag: Some({...drag, movement: Moving, mousePosition: (x, y)})
      })
    | None => ReasonReact.NoUpdate
    }
  | StartDraggingList(cardList, index, mousePosition, offset) =>
    ReasonReact.Update({
      ...state,
      board: {
        ...state.board,
        lists: List.filter((list) => list.cid !== cardList.cid, state.board.lists)
      },
      drag:
        Some({
          target: List(cardList, index),
          initialClickOffset: offset,
          mousePosition,
          movement: Started
        })
    })
  | StartDraggingCard(card, listCid, index, mousePosition, offset) =>
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
      drag:
        Some({
          target: Card(card, listCid, index),
          initialClickOffset: offset,
          mousePosition,
          movement: Started
        })
    })
  | SetDropTargetForList(index) =>
    switch state.drag {
    | Some(drag) =>
      switch drag.target {
      | List(list, _index) =>
        ReasonReact.Update({...state, drag: Some({...drag, target: List(list, index)})})
      | Card(_, _, _) => ReasonReact.NoUpdate
      }
    | None => ReasonReact.NoUpdate
    }
  | SetDropTargetForCard(listCid, index) =>
    switch state.drag {
    | Some(drag) =>
      switch drag.target {
      | List(_list, _index) => ReasonReact.NoUpdate
      | Card(card, _listCid, _index) =>
        ReasonReact.Update({...state, drag: Some({...drag, target: Card(card, listCid, index)})})
      }
    | None => ReasonReact.NoUpdate
    }
  | StopDragging =>
    switch state.drag {
    | Some(drag) =>
      switch drag.target {
      | List(list, index) =>
        ReasonReact.Update({
          ...state,
          board: {
            ...state.board,
            lists: Utils.List.insertAt(~index, ~elem=list, ~list=state.board.lists)
          },
          drag: None
        })
      | Card(card, listCid, index) =>
        ReasonReact.Update({
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
        })
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
      editBoardNameForm: {...state.editBoardNameForm, isOpen: false}
    })
  | ToggleOpenEditBoardNameForm =>
    ReasonReact.SideEffects(
      (
        (self) =>
          self.state.editBoardNameForm.isOpen ?
            self.reduce(() => CloseEditBoardNameForm, ()) :
            self.reduce(() => OpenEditBoardNameForm, ())
      )
    )
  | OpenEditBoardNameForm =>
    ReasonReact.UpdateWithSideEffects(
      {
        ...state,
        editBoardNameForm: {...state.editBoardNameForm, name: state.board.name, isOpen: true}
      },
      (({state}) => Utils.Dom.focusAndHighlightElement(state.editBoardNameForm.inputRef))
    )
  | CloseEditBoardNameForm =>
    ReasonReact.Update({...state, editBoardNameForm: {...state.editBoardNameForm, isOpen: false}})
  | ChangeBoardName =>
    ReasonReact.Update({
      ...state,
      board: {...state.board, name: state.editBoardNameForm.name},
      editBoardNameForm: {...state.editBoardNameForm, name: "", isOpen: false}
    })
  | ChangeEditBoardFormName(name) =>
    ReasonReact.Update({...state, editBoardNameForm: {...state.editBoardNameForm, name}})
  | NoOp => ReasonReact.NoUpdate
  };

let component = ReasonReact.reducerComponent("App");

let lists = (state) =>
  switch state.drag {
  | Some(drag) =>
    switch drag.target {
    | List(list, index) => Utils.List.insertAt(~index, ~elem=list, ~list=state.board.lists)
    | Card(card, listCid, index) =>
      List.map(
        (list) =>
          list.cid === listCid ?
            {...list, cards: Utils.List.insertAt(~index, ~elem=card, ~list=list.cards)} : list,
        state.board.lists
      )
    }
  | None => state.board.lists
  };

let make = (_children) => {
  ...component,
  initialState,
  reducer,
  render: ({state, reduce, handle}) =>
    View.(
      <Container
        drag=state.drag
        onClick=(
          reduce(
            (event) => {
              let target = ReactEventRe.Mouse.target(event);
              switch state.editBoardNameForm.containerRef^ {
              | Some(r) =>
                let container = ReactDOMRe.domElementToObj(r);
                Js.to_bool(container##contains(target)) ? NoOp : CloseEditBoardNameForm
              | None => NoOp
              }
            }
          )
        )
        onMouseMove=(
          reduce(
            (event) =>
              UpdateMousePosition(ReactEventRe.Mouse.pageX(event), ReactEventRe.Mouse.pageY(event))
          )
        )
        onKeyDown=(
          reduce(
            (event) => {
              let keyCode = ReactEventRe.Keyboard.keyCode(event);
              switch keyCode {
              | 27 => CloseAllOpenForms
              | _ => NoOp
              }
            }
          )
        )
        onMouseUp=(reduce((_event) => StopDragging))>
        <AppHeader />
        <BoardHeader
          boardName=state.board.name
          openForm=(reduce((_event) => ToggleOpenEditBoardNameForm))
        />
        <div className="flex-auto flex flex-row overflow-x-scroll">
          (
            state
            |> lists
            |> List.mapi(
                 (index, list: cardList) =>
                   <CardList
                     key=list.cid
                     list
                     showPlaceholderOnly=(
                       switch state.drag {
                       | Some(drag) =>
                         switch drag.target {
                         | List(draggedList, _index) =>
                           list.cid === draggedList.cid && drag.movement === Moving
                         | Card(_, _, _) => false
                         }
                       | None => false
                       }
                     )
                     isEditingName=(
                       switch state.editListCid {
                       | Some(cid) => cid === list.cid
                       | None => false
                       }
                     )
                     changeListName=(
                       reduce(
                         (event) =>
                           EditListName(
                             list.cid,
                             ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
                           )
                       )
                     )
                     openForm=(reduce(() => StartEditingListName(list.cid)))
                     closeForm=(reduce(() => StopEditingListName))
                     onMouseEnter=(
                       reduce(
                         (_event) =>
                           switch state.drag {
                           | Some(drag) =>
                             switch drag.target {
                             | List(_, _) => SetDropTargetForList(index)
                             | Card(_, _, _) =>
                               SetDropTargetForCard(list.cid, List.length(list.cards))
                             }
                           | None => NoOp
                           }
                       )
                     )
                     onMouseDown=(
                       reduce(
                         (event) => {
                           let nativeEvent = ReactEventRe.Mouse.nativeEvent(event);
                           StartDraggingList(
                             list,
                             index,
                             (ReactEventRe.Mouse.pageX(event), ReactEventRe.Mouse.pageY(event)),
                             /* TODO: this may not work on all browsers, so should probably be treated as an option type */
                             (nativeEvent##offsetX, nativeEvent##offsetY)
                           )
                         }
                       )
                     )
                     setInputRef=(
                       handle(
                         (theRef, {state}) => state.editListInputRef := Js.Nullable.to_opt(theRef)
                       )
                     )
                     viewCard=(
                       (cardIndex, card: State.card) =>
                         <Card
                           drag=None
                           card
                           onMouseEnter=(
                             reduce((_event) => SetDropTargetForCard(list.cid, cardIndex))
                           )
                           showPlaceholderOnly=(
                             switch state.drag {
                             | Some(drag) =>
                               switch drag.target {
                               | Card(draggedCard, _listCid, _index) =>
                                 draggedCard.cid === card.cid && drag.movement === Moving
                               | List(_draggedList, _index) => false
                               }
                             | None => false
                             }
                           )
                           onDragStart=(
                             reduce(
                               (event) => {
                                 let nativeEvent = ReactEventRe.Mouse.nativeEvent(event);
                                 StartDraggingCard(
                                   card,
                                   list.cid,
                                   cardIndex,
                                   (
                                     ReactEventRe.Mouse.pageX(event),
                                     ReactEventRe.Mouse.pageY(event)
                                   ),
                                   /* TODO: this may not work on all browsers, so should probably be treated as an option type */
                                   (nativeEvent##offsetX, nativeEvent##offsetY)
                                 )
                               }
                             )
                           )
                         />
                     )>
                     <NewCardForm
                       listCid=list.cid
                       newCardForm=state.newCardForm
                       changeNewCardName=(
                         reduce(
                           (event) =>
                             SetNewCardName(
                               ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
                             )
                         )
                       )
                       addCard=(reduce((_event) => AddCardToList(list.cid)))
                       openForm=(reduce((_event) => OpenNewCardForm(list.cid)))
                       closeForm=(reduce((_event) => CloseNewCardForm))
                       setInputRef=(
                         handle(
                           (theRef, {state}) =>
                             switch state.newCardForm {
                             | Some(newCardForm) =>
                               newCardForm.inputRef := Js.Nullable.to_opt(theRef)
                             | None => ()
                             }
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
            changeNewListName=(
              reduce(
                (event) =>
                  SetNewListName(
                    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
                  )
              )
            )
            openForm=(reduce((_event) => OpenNewListForm))
            closeForm=(reduce((_event) => CloseNewListForm))
            setInputRef=(
              handle((theRef, {state}) => state.newListForm.inputRef := Js.Nullable.to_opt(theRef))
            )
          />
        </div>
        (
          switch state.drag {
          | Some(drag) =>
            switch (drag.target, drag.movement) {
            | (List(list, _index), Moving) =>
              <CardList
                list
                drag=state.drag
                isEditingName=false
                openForm=(() => ())
                closeForm=(() => ())
                changeListName=((_event) => ())
                setInputRef=((_theRef) => ())
                viewCard=(
                  (_index, card) =>
                    <Card
                      card
                      onDragStart=((_event) => ())
                      onMouseEnter=((_event) => ())
                      drag=state.drag
                    />
                )>
                <NewCardForm
                  listCid=list.cid
                  newCardForm=state.newCardForm
                  changeNewCardName=((_event) => ())
                  addCard=((_event) => ())
                  openForm=((_event) => ())
                  closeForm=((_event) => ())
                  setInputRef=((_ref) => ())
                />
              </CardList>
            | (List(_, _), Started) => ReasonReact.nullElement
            | (Card(card, _, _), Moving) =>
              <Card
                card
                onDragStart=((_event) => ())
                onMouseEnter=((_event) => ())
                drag=state.drag
              />
            | (Card(_, _, _), Started) => ReasonReact.nullElement
            }
          | None => ReasonReact.nullElement
          }
        )
        (
          state.editBoardNameForm.isOpen ?
            <div
              ref=(
                handle(
                  (theRef, {state}) =>
                    state.editBoardNameForm.containerRef := Js.Nullable.to_opt(theRef)
                )
              )
              style=(
                ReactDOMRe.Style.make(
                  ~position="absolute",
                  ~left="8px",
                  ~top="90px",
                  ~width="300px",
                  ()
                )
              )
              className="br2 bg-near-white ba b--silver flex flex-column">
              <div>
                <div
                  className="flex flex-row justify-between items-center ml1 mr1 bb b--moon-gray h2">
                  <span style=(ReactDOMRe.Style.make(~visibility="hidden", ()))>
                    (ReasonReact.stringToElement("X"))
                  </span>
                  <span className="helvetica f6 gray">
                    (ReasonReact.stringToElement("Rename Board"))
                  </span>
                  <button
                    _type="button"
                    onClick=(reduce((_event) => CloseEditBoardNameForm))
                    className="bg-transparent bn pointer button-reset light-silver hover-dark-gray f7">
                    (ReasonReact.stringToElement("X"))
                  </button>
                </div>
              </div>
              <Form
                className="flex flex-column ma2" onSubmit=(reduce((_event) => ChangeBoardName))>
                <label className="pb1 fw7 helvetica f6 dark-gray">
                  (ReasonReact.stringToElement("Name"))
                </label>
                <input
                  value=state.editBoardNameForm.name
                  onChange=(
                    reduce(
                      (event) =>
                        ChangeEditBoardFormName(
                          ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
                        )
                    )
                  )
                  ref=(
                    handle(
                      (theRef, {state}) =>
                        state.editBoardNameForm.inputRef := Js.Nullable.to_opt(theRef)
                    )
                  )
                  className="pa2 mb3 br2 input-reset ba b--silver"
                />
                <button
                  _type="submit"
                  className="h2 pointer button-reset bg-green bn near-white fw7 br2 hover-bg-dark-green mr1 self-start mb1 ml1 w4"
                  disabled=(
                    Js.Boolean.to_js_boolean(String.length(state.editBoardNameForm.name) === 0)
                  )>
                  (ReasonReact.stringToElement("Rename"))
                </button>
              </Form>
            </div> :
            ReasonReact.nullElement
        )
      </Container>
    )
};