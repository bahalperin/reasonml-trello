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
  | SetDropTarget(int)
  | DropList;

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
  newListForm: {name: "", isOpen: false},
  newCardForm: None,
  drag: None
};

let rec splitAtHelper = (index, originalList, newList) =>
  switch originalList {
  | [] => (List.rev(newList), [])
  | [head, ...tail] when List.length(newList) < index =>
    splitAtHelper(index, tail, [head, ...newList])
  | [_head, ..._tail] => (List.rev(newList), originalList)
  };

let splitAt = (index, list) => splitAtHelper(index, list, []);

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
      ReasonReact.Update({
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
      })
    | None => ReasonReact.NoUpdate
    }
  | SetNewCardName(newCardName) =>
    switch state.newCardForm {
    | Some(newCardForm) =>
      ReasonReact.Update({...state, newCardForm: Some({...newCardForm, name: newCardName})})
    | None => ReasonReact.NoUpdate
    }
  | OpenNewCardForm(listCid) =>
    ReasonReact.Update({...state, newCardForm: Some({name: "", listCid})})
  | CloseNewCardForm => ReasonReact.Update({...state, newCardForm: None})
  | UpdateMousePosition(x, y) =>
    switch state.drag {
    | Some(drag) => ReasonReact.Update({...state, drag: Some({...drag, mousePosition: (x, y)})})
    | None => ReasonReact.NoUpdate
    }
  | StartDraggingList(cardList, index, mousePosition, offset) =>
    ReasonReact.Update({
      ...state,
      board: {
        ...state.board,
        lists: List.filter((list) => list.cid !== cardList.cid, state.board.lists)
      },
      drag: Some({dropTarget: index, mousePosition, list: cardList, initialClickOffset: offset})
    })
  | SetDropTarget(index) =>
    switch state.drag {
    | Some(drag) => ReasonReact.Update({...state, drag: Some({...drag, dropTarget: index})})
    | None => ReasonReact.NoUpdate
    }
  | DropList =>
    switch state.drag {
    | Some(drag) =>
      let (first, rest) = splitAt(drag.dropTarget, state.board.lists);
      ReasonReact.Update({
        ...state,
        board: {...state.board, lists: List.concat([first, [drag.list], rest])},
        drag: None
      })
    | None => ReasonReact.NoUpdate
    }
  | OpenNewListForm =>
    ReasonReact.Update({...state, newListForm: {...state.newListForm, isOpen: true}})
  | CloseNewListForm =>
    ReasonReact.Update({...state, newListForm: {...state.newListForm, isOpen: false}})
  };

let component = ReasonReact.reducerComponent("App");

let lists = (state) =>
  switch state.drag {
  | Some(drag) =>
    let (first, rest) = splitAt(drag.dropTarget, state.board.lists);
    List.concat([first, [drag.list], rest])
  | None => state.board.lists
  };

let make = (_children) => {
  ...component,
  initialState,
  reducer,
  render: ({state, reduce}) =>
    View.(
      <Container
        onMouseMove=(
          reduce(
            (event) =>
              UpdateMousePosition(ReactEventRe.Mouse.pageX(event), ReactEventRe.Mouse.pageY(event))
          )
        )
        onMouseUp=(reduce((_event) => DropList))>
        <AppHeader />
        <BoardHeader boardName=state.board.name />
        <div className="flex-auto flex flex-row overflow-x-scroll">
          (
            state
            |> lists
            |> List.mapi(
                 (index, list: cardList) => {
                   let draggedListCid =
                     switch state.drag {
                     | Some(drag) => drag.list.cid
                     | None => ""
                     };
                   <CardList
                     key=list.cid
                     list
                     showPlaceholderOnly=(list.cid === draggedListCid)
                     onMouseEnter=(reduce((_event) => SetDropTarget(index)))
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
                       addCard=(
                         reduce(
                           (event) => {
                             ReactEventRe.Form.preventDefault(event);
                             AddCardToList(list.cid)
                           }
                         )
                       )
                       openForm=(reduce((_event) => OpenNewCardForm(list.cid)))
                       closeForm=(reduce((_event) => CloseNewCardForm))
                     />
                   </CardList>
                 }
               )
            |> Array.of_list
            |> ReasonReact.arrayToElement
          )
          <AddListForm
            addList=(
              reduce(
                (event) => {
                  ReactEventRe.Form.preventDefault(event);
                  AddList
                }
              )
            )
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
          />
        </div>
        (
          switch state.drag {
          | Some(drag) =>
            <CardList list=drag.list drag=state.drag>
              <NewCardForm
                listCid=drag.list.cid
                newCardForm=state.newCardForm
                changeNewCardName=((_event) => ())
                addCard=((_event) => ())
                openForm=((_event) => ())
                closeForm=((_event) => ())
              />
            </CardList>
          | None => ReasonReact.nullElement
          }
        )
      </Container>
    )
};