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
    <div className="h-100 flex flex-column">
      <div className="relative h3 flex-none"> (ReasonReact.stringToElement("Reason Trello")) </div>
      <div className="relative h3 flex-none">
        (ReasonReact.stringToElement(state.board.name))
      </div>
      <div className="flex-auto flex flex-row overflow-x-scroll">
        (
          state.board.lists
          |> List.map(
               (list: cardList) =>
                 <div key=list.cid className="flex flex-column">
                   <div className="flex flex-column">
                     <h3 className="h3 flex-none"> (ReasonReact.stringToElement(list.name)) </h3>
                     <div className="flex-auto overflow-y-scroll flex flex-column-reverse">
                       <div>
                         (
                           list.cards
                           |> List.map(
                                (card: card) =>
                                  <div key=card.cid>
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
                               className="h3 flex-none"
                               onSubmit=(
                                 reduce(
                                   (event) => {
                                     ReactEventRe.Form.preventDefault(event);
                                     AddCardToList(list.cid)
                                   }
                                 )
                               )>
                               <input
                                 value=newCardForm.name
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
                               <button _type="submit">
                                 (ReasonReact.stringToElement("Add"))
                               </button>
                               <button
                                 _type="button" onClick=(reduce((_event) => CloseNewCardForm))>
                                 (ReasonReact.stringToElement("X"))
                               </button>
                             </form>
                           | Some(_)
                           | None =>
                             <div className="h3 flex-none">
                               <button onClick=(reduce((_event) => OpenNewCardForm(list.cid)))>
                                 (ReasonReact.stringToElement("Add a card"))
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
            onChange=(
              reduce(
                (event) =>
                  SetNewListName(
                    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
                  )
              )
            )
          />
        </form>
      </div>
    </div>
};