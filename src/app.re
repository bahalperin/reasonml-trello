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

type action =
  | AddList
  | AddListHelper(string)
  | SetNewListName(string)
  | AddCardToList(string)
  | AddCardToListHelper(string, string)
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
  newListName: "",
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
      newListName: "",
      board: {
        ...state.board,
        lists: List.append(state.board.lists, [{cid, name: state.newListName, cards: []}])
      }
    })
  | SetNewListName(newListName) => ReasonReact.Update({...state, newListName})
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
    <div
      className="h-100 flex flex-column bg-green"
      onMouseMove=(
        reduce(
          (event) =>
            UpdateMousePosition(ReactEventRe.Mouse.pageX(event), ReactEventRe.Mouse.pageY(event))
        )
      )
      onMouseUp=(reduce((_event) => DropList))>
      <div
        className="relative h-40px flex-none flex items-center justify-center bg-dark-green shadow-1">
        <span className="f3 near-white"> (ReasonReact.stringToElement("Reason Trello")) </span>
      </div>
      <div className="relative h3 flex-none flex items-center justify-start pa2">
        <span className="f4 fw7 helvetica near-white">
          (ReasonReact.stringToElement(state.board.name))
        </span>
      </div>
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
                 list.cid === draggedListCid ?
                   <div
                     key=list.cid
                     className="flex flex-column ml2 mr1 w5 bg-dark-green flex-none"
                   /> :
                   <div
                     key=list.cid
                     className="flex flex-column ml2 mr1 w5 flex-none"
                     onMouseEnter=(reduce((_event) => SetDropTarget(index)))>
                     <div className="flex flex-column">
                       <div
                         className="flex-none br2 br--top pa1 ma0 pa2 bg-moon-gray pointer"
                         onMouseDown=(
                           reduce(
                             (event) => {
                               let nativeEvent = ReactEventRe.Mouse.nativeEvent(event);
                               StartDraggingList(
                                 list,
                                 index,
                                 (
                                   ReactEventRe.Mouse.pageX(event),
                                   ReactEventRe.Mouse.pageY(event)
                                 ),
                                 /* TODO: this may not work on all browsers, so should probably be treated as an option type */
                                 (nativeEvent##offsetX, nativeEvent##offsetY)
                               )
                             }
                           )
                         )>
                         <h3 className="f5 helvetica ma0 pa0 dark-gray">
                           (ReasonReact.stringToElement(list.name))
                         </h3>
                       </div>
                       <div
                         className="flex-auto overflow-y-scroll flex flex-column-reverse bg-moon-gray br2 br--bottom">
                         <div>
                           (
                             list.cards
                             |> List.map(
                                  (card: card) =>
                                    <div
                                      key=card.cid
                                      className="bg-white-90 br2 mb2 ml1 mr1 mt0 pa2 helvetica f6 dark-gray bb b--silver">
                                      (ReasonReact.stringToElement(card.name))
                                    </div>
                                )
                             |> Array.of_list
                             |> ReasonReact.arrayToElement
                           )
                           (
                             switch state.newCardForm {
                             | Some(newCardForm) when newCardForm.listCid === list.cid =>
                               <form
                                 className="flex-none flex flex-column"
                                 onSubmit=(
                                   reduce(
                                     (event) => {
                                       ReactEventRe.Form.preventDefault(event);
                                       AddCardToList(list.cid)
                                     }
                                   )
                                 )>
                                 <div className="bg-white-90 br2 mb2 ml1 mr1 mt0 pa2 h3">
                                   <input
                                     value=newCardForm.name
                                     className="bn bg-transparent input-reset outline-0"
                                     onChange=(
                                       reduce(
                                         (event) =>
                                           SetNewCardName(
                                             ReactDOMRe.domElementToObj(
                                               ReactEventRe.Form.target(event)
                                             )##value
                                           )
                                       )
                                     )
                                   />
                                 </div>
                                 <div className="flex flex-row items-center justify-start pl2 pb2">
                                   <button
                                     className="h2 w3 pointer button-reset bg-green bn near-white fw7 br2 hover-bg-dark-green mr1"
                                     _type="submit"
                                     disabled=(
                                       Js.Boolean.to_js_boolean(
                                         String.length(newCardForm.name) === 0
                                       )
                                     )>
                                     (ReasonReact.stringToElement("Add"))
                                   </button>
                                   <button
                                     className="bg-transparent bn pointer button-reset light-silver hover-dark-gray f3"
                                     _type="button"
                                     onClick=(reduce((_event) => CloseNewCardForm))>
                                     (ReasonReact.stringToElement("X"))
                                   </button>
                                 </div>
                               </form>
                             | Some(_)
                             | None =>
                               <div className="flex-none flex">
                                 <button
                                   className="bg-transparent b--transparent button-reset pointer flex flex-auto hover-bg-black-10 pa2 f6"
                                   onClick=(reduce((_event) => OpenNewCardForm(list.cid)))>
                                   (ReasonReact.stringToElement("Add a card..."))
                                 </button>
                               </div>
                             }
                           )
                         </div>
                       </div>
                     </div>
                   </div>
               }
             )
          |> Array.of_list
          |> ReasonReact.arrayToElement
        )
        <form
          onSubmit=(
            reduce(
              (event) => {
                ReactEventRe.Form.preventDefault(event);
                AddList
              }
            )
          )>
          <input
            value=state.newListName
            placeholder="Add a list..."
            className="bg-dark-green bn input-reset br2 h2 pt1 pb1 pl2 white-80 w5"
            onChange=(
              reduce(
                (event) =>
                  SetNewListName(
                    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
                  )
              )
            )
          />
          <button
            _type="submit"
            className="dn"
            disabled=(Js.Boolean.to_js_boolean(String.length(state.newListName) === 0))
          />
        </form>
      </div>
      (
        switch state.drag {
        | Some(drag) =>
          <div
            className="flex flex-column ml2 mr1 w5 rotate-5 absolute pointer"
            style=(
              ReactDOMRe.Style.make(
                ~left=
                  string_of_int(fst(drag.mousePosition) - fst(drag.initialClickOffset)) ++ "px",
                ~top=string_of_int(snd(drag.mousePosition) - snd(drag.initialClickOffset)) ++ "px",
                ~pointerEvents="none",
                ()
              )
            )>
            <div className="flex flex-column flex-none">
              <div className="flex-none br2 br--top pa1 ma0 pa2 bg-moon-gray pointer">
                <h3 className="f5 helvetica ma0 pa0 dark-gray">
                  (ReasonReact.stringToElement(drag.list.name))
                </h3>
              </div>
              <div
                className="flex-auto overflow-y-scroll flex flex-column-reverse bg-moon-gray br2 br--bottom">
                <div>
                  (
                    drag.list.cards
                    |> List.map(
                         (card: card) =>
                           <div
                             key=card.cid
                             className="bg-white-90 br2 mb2 ml1 mr1 mt0 pa2 helvetica f6 dark-gray bb b--silver">
                             (ReasonReact.stringToElement(card.name))
                           </div>
                       )
                    |> Array.of_list
                    |> ReasonReact.arrayToElement
                  )
                  (
                    switch state.newCardForm {
                    | Some(newCardForm) when newCardForm.listCid === drag.list.cid =>
                      <form
                        className="flex-none flex flex-column"
                        onSubmit=(
                          reduce(
                            (event) => {
                              ReactEventRe.Form.preventDefault(event);
                              AddCardToList(drag.list.cid)
                            }
                          )
                        )>
                        <div className="bg-white-90 br2 mb2 ml1 mr1 mt0 pa2 h3">
                          <input
                            value=newCardForm.name
                            className="bn bg-transparent input-reset outline-0"
                            onChange=(
                              reduce(
                                (event) =>
                                  SetNewCardName(
                                    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
                                  )
                              )
                            )
                          />
                        </div>
                        <div className="flex flex-row items-center justify-start pl2 pb2">
                          <button
                            className="h2 w3 pointer button-reset bg-green bn near-white fw7 br2 hover-bg-dark-green mr1"
                            _type="submit"
                            disabled=(
                              Js.Boolean.to_js_boolean(String.length(newCardForm.name) === 0)
                            )>
                            (ReasonReact.stringToElement("Add"))
                          </button>
                          <button
                            className="bg-transparent bn pointer button-reset light-silver hover-dark-gray f3"
                            _type="button"
                            onClick=(reduce((_event) => CloseNewCardForm))>
                            (ReasonReact.stringToElement("X"))
                          </button>
                        </div>
                      </form>
                    | Some(_)
                    | None =>
                      <div className="flex-none flex">
                        <button
                          className="bg-transparent b--transparent button-reset pointer flex flex-auto hover-bg-black-10 pa2 f6"
                          onClick=(reduce((_event) => OpenNewCardForm(drag.list.cid)))>
                          (ReasonReact.stringToElement("Add a card..."))
                        </button>
                      </div>
                    }
                  )
                </div>
              </div>
            </div>
          </div>
        | None => ReasonReact.nullElement
        }
      )
    </div>
};