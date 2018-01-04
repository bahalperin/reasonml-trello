open State;

type inputObj = {. 
  [@bs.meth] "focus": unit => unit,
  [@bs.set] "selectionStart": int,
  [@bs.set] "selectionEnd": int,
  "value": string
};

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
  editListInputRef: ref(None)
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
          (self) =>
            Js.Global.setTimeout(
              () =>
                switch self.state.newCardForm {
                | Some(newCardForm) =>
                  switch newCardForm.inputRef^ {
                  | Some(inputRef) => ReactDOMRe.domElementToObj(inputRef)##focus()
                  | None => ()
                  }
                | None => ()
                },
              200
            )
            |> ignore
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
        (self) =>
          Js.Global.setTimeout(
            () =>
              switch self.state.newCardForm {
              | Some(newCardForm) =>
                switch newCardForm.inputRef^ {
                | Some(inputRef) => ReactDOMRe.domElementToObj(inputRef)##focus()
                | None => ()
                }
              | None => ()
              },
            200
          )
          |> ignore
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
        let (first, rest) = splitAt(index, state.board.lists);
        ReasonReact.Update({
          ...state,
          board: {...state.board, lists: List.concat([first, [list], rest])},
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
                    {
                      ...list,
                      cards:
                        List.concat([
                          fst(splitAt(index, list.cards)),
                          [card],
                          snd(splitAt(index, list.cards))
                        ])
                    } :
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
      (
        (self) => {
          Js.Global.setTimeout(
            () =>
              switch self.state.newListForm.inputRef^ {
              | Some(inputRef) => ReactDOMRe.domElementToObj(inputRef)##focus()
              | None => ()
              },
            200
          )
          |> ignore;
          ()
        }
      )
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
      (
        (self) => {
          Js.Global.setTimeout(
            () =>
              switch self.state.editListInputRef^ {
              | Some(inputRef) =>
                let inputObj: inputObj = ReactDOMRe.domElementToObj(inputRef);
                inputObj##focus();
                inputObj##selectionStart#=0;
                inputObj##selectionEnd#=(String.length(inputObj##value))
              | None => ()
              },
            200
          )
          |> ignore;
          ()
        }
      )
    )
  | StopEditingListName => ReasonReact.Update({...state, editListCid: None})
  | NoOp => ReasonReact.NoUpdate
  };

let component = ReasonReact.reducerComponent("App");

let lists = (state) =>
  switch state.drag {
  | Some(drag) =>
    switch drag.target {
    | List(list, index) =>
      let (first, rest) = splitAt(index, state.board.lists);
      List.concat([first, [list], rest])
    | Card(card, listCid, index) =>
      List.map(
        (list) =>
          list.cid === listCid ?
            {
              ...list,
              cards:
                List.concat([
                  fst(splitAt(index, list.cards)),
                  [card],
                  snd(splitAt(index, list.cards))
                ])
            } :
            list,
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
        onMouseMove=(
          reduce(
            (event) =>
              UpdateMousePosition(ReactEventRe.Mouse.pageX(event), ReactEventRe.Mouse.pageY(event))
          )
        )
        onMouseUp=(reduce((_event) => StopDragging))>
        <AppHeader />
        <BoardHeader boardName=state.board.name />
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
                     onMouseEnter=(reduce((_event) => switch state.drag {
                     | Some(drag) =>
                        switch drag.target {
                        | List(_,_) =>
                          SetDropTargetForList(index)
                        | Card(_,_,_) =>
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
      </Container>
    )
};
