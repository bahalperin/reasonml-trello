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

type state = {
  board,
  newListName: string,
  newCardForm: option(newCardForm)
};

type action =
  | AddList
  | AddListHelper(string)
  | SetNewListName(string)
  | AddCardToList(string)
  | AddCardToListHelper(string, string)
  | OpenNewCardForm(string)
  | CloseNewCardForm
  | SetNewCardName(string);

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
  newCardForm: None
};

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
  };

let component = ReasonReact.reducerComponent("App");

let make = (_children) => {
  ...component,
  initialState,
  reducer,
  render: ({state, reduce}) =>
    <div className="h-100 flex flex-column bg-green">
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
          state.board.lists
          |> List.map(
               (list: cardList) =>
                 <div key=list.cid className="flex flex-column ml2 mr1 w5">
                   <div className="flex flex-column">
                     <div className="flex-none br2 br--top pa1 ma0 pa2 bg-moon-gray">
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
    </div>
};