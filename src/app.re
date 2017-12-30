type card = {name: string};

type cardList = {
  name: string,
  cards: list(card)
};

type board = {
  name: string,
  lists: list(cardList)
};

type state = {board};

type action =
  | AddList(string);

let initialState = () => {
  board: {
    name: "Welcome board",
    lists: [
      {name: "This is a list", cards: [{name: "This is a card"}, {name: "This is also a card"}]}
    ]
  }
};

let reducer = (action, state) =>
  switch action {
  | AddList(name) =>
    ReasonReact.Update({
      board: {...state.board, lists: List.append(state.board.lists, [{name, cards: []}])}
    })
  };

module Card = {
  let component = ReasonReact.statelessComponent("Card");
  let make = (~card: card, _children) => {
    ...component,
    render: (_self) => <div> <span> (ReasonReact.stringToElement(card.name)) </span> </div>
  };
};

module CardList = {
  let component = ReasonReact.statelessComponent("CardList");
  let make = (~list: cardList, _children) => {
    ...component,
    render: (_self) =>
      <div>
        <h2> (ReasonReact.stringToElement(list.name)) </h2>
        <div>
          (
            list.cards
            |> List.map((card) => <Card card />)
            |> Array.of_list
            |> ReasonReact.arrayToElement
          )
        </div>
      </div>
  };
};

module Board = {
  let component = ReasonReact.statelessComponent("Board");
  let make = (~board: board, _children) => {
    ...component,
    render: (_self) =>
      <div>
        <div> <span> (ReasonReact.stringToElement(board.name)) </span> </div>
        <div>
          (
            board.lists
            |> List.map((list) => <CardList list />)
            |> Array.of_list
            |> ReasonReact.arrayToElement
          )
        </div>
      </div>
  };
};

let component = ReasonReact.reducerComponent("App");

let make = (_children) => {
  ...component,
  initialState,
  reducer,
  render: ({state}) =>
    <div className="App">
      <div className="App-header"> <h2> (ReasonReact.stringToElement("Reason Trello")) </h2> </div>
      <Board board=state.board />
    </div>
};