module Form = {
  let component = ReasonReact.statelessComponent("Form");
  let make = (~onSubmit, ~className="", children) => {
    ...component,
    render: (_self) =>
      ReasonReact.createDomElement(
        "form",
        ~props={
          "onSubmit": (event) => {
            ReactEventRe.Form.preventDefault(event);
            onSubmit(event)
          },
          "className": className
        },
        children
      )
  };
};

module Button = {
  module Submit = {
    let component = ReasonReact.statelessComponent("Button.Submit");
    let make = (~disabled, ~className="", children) => {
      ...component,
      render: (_self) =>
        ReasonReact.createDomElement(
          "button",
          ~props={
            "_type": "submit",
            "disabled": Js.Boolean.to_js_boolean(disabled),
            "className":
              "pointer button-reset bg-green bn near-white br2 hover-bg-dark-green " ++ className
          },
          children
        )
    };
  };
};

module AppHeader = {
  let component = ReasonReact.statelessComponent("Header");
  let make = (_children) => {
    ...component,
    render: (_self) =>
      <div
        className="relative h-40px flex-none flex items-center justify-center bg-dark-green shadow-1">
        <span className="f3 near-white"> (ReasonReact.stringToElement("Reason Trello")) </span>
      </div>
  };
};

module BoardHeader = {
  let component = ReasonReact.statelessComponent("BoardHeader");
  let make = (~boardName, ~openForm, _children) => {
    ...component,
    render: (_self) =>
      <div className="relative h-30px flex-none flex items-center justify-start pa2">
        <span
          className="f4 fw7 helvetica near-white hover-bg-dark-green pa2 br2 pointer"
          onClick=openForm>
          (ReasonReact.stringToElement(boardName))
        </span>
      </div>
  };
};

module Container = {
  let component = ReasonReact.statelessComponent("Container");
  let make =
      (
        ~onMouseMove,
        ~onMouseUp,
        ~drag: option(State.dragState),
        ~onClick,
        ~onKeyDown,
        children: array(ReasonReact.reactElement)
      ) => {
    ...component,
    render: (_self) =>
      ReasonReact.createDomElement(
        "div",
        ~props={
          "className": "h-100 flex flex-column bg-green" ++ (Option.isSome(drag) ? " pointer" : ""),
          "onMouseMove": onMouseMove,
          "onMouseUp": onMouseUp,
          "onKeyDown": onKeyDown,
          "onClick": onClick
        },
        children
      )
  };
};

module CardList = {
  let component = ReasonReact.statelessComponent("CardList");
  let make =
      (
        ~list: State.cardList,
        ~showPlaceholderOnly=false,
        ~drag=None,
        ~onMouseEnter=(_event) => (),
        ~onMouseDown=(_event) => (),
        ~isEditingName,
        ~changeListName,
        ~openForm,
        ~closeForm,
        ~viewCard,
        ~setInputRef,
        children: array(ReasonReact.reactElement)
      ) => {
    ...component,
    render: (_self) => {
      let (draggedClasses, draggedStyles) =
        switch drag {
        | Some((drag: State.dragState)) =>
          switch (drag.movement, drag.target) {
          | (State.Moving, List(_)) => (
              "rotate-5 absolute pointer",
              ReactDOMRe.Style.make(
                ~left=
                  string_of_int(fst(drag.mousePosition) - fst(drag.initialClickOffset)) ++ "px",
                ~top=string_of_int(snd(drag.mousePosition) - snd(drag.initialClickOffset)) ++ "px",
                ~pointerEvents="none",
                ()
              )
            )
          | (State.Moving, _)
          | (State.Started, _) => ("", ReactDOMRe.Style.make())
          }
        | None => ("", ReactDOMRe.Style.make())
        };
      <div
        key=list.cid
        className=("flex flex-column ml2 mr1 w5 flex-none" ++ " " ++ draggedClasses)
        onMouseEnter
        style=draggedStyles>
        <div className="bg-dark-green br2 flex flex-column">
          <div
            className="flex flex-column"
            style=(
              ReactDOMRe.Style.make(~visibility=showPlaceholderOnly ? "hidden" : "inherit", ())
            )>
            <div
              className="flex-none br2 br--top pa1 ma0 pa2 bg-moon-gray pointer"
              onClick=((_event) => openForm())>
              (
                isEditingName ?
                  <Form className="flex" onSubmit=((_event) => closeForm())>
                    <input
                      value=list.name
                      onChange=changeListName
                      className="br2 ba b--gray input-reset flex-auto"
                      onBlur=((_event) => closeForm())
                      ref=setInputRef
                    />
                    <button _type="submit" className="dn" />
                  </Form> :
                  <h3 className="f5 helvetica ma0 pa0 dark-gray user-select-none" onMouseDown>
                    (ReasonReact.stringToElement(list.name))
                  </h3>
              )
            </div>
            <div
              className="flex-auto overflow-y-scroll flex flex-column-reverse bg-moon-gray br2 br--bottom">
              (
                ReasonReact.createDomElement(
                  "div",
                  ~props=Js.Obj.empty(),
                  list.cards
                  |> List.mapi(viewCard)
                  |> Array.of_list
                  |> ((arr) => Array.append(arr, children))
                )
              )
            </div>
          </div>
        </div>
      </div>
    }
  };
};

module Card = {
  let component = ReasonReact.statelessComponent("Card");
  let make =
      (
        ~card: State.card,
        ~onDragStart,
        ~onMouseEnter,
        ~drag,
        ~showPlaceholderOnly=false,
        _children
      ) => {
    ...component,
    render: (_self) => {
      let (draggedClasses, draggedStyles) =
        switch drag {
        | Some((drag: State.dragState)) =>
          switch (drag.movement, drag.target) {
          | (State.Moving, Card(_)) => (
              "rotate-5 absolute pointer",
              ReactDOMRe.Style.make(
                ~left=
                  string_of_int(fst(drag.mousePosition) - fst(drag.initialClickOffset)) ++ "px",
                ~top=string_of_int(snd(drag.mousePosition) - snd(drag.initialClickOffset)) ++ "px",
                ~width="245px",
                ~pointerEvents="none",
                ()
              )
            )
          | (State.Moving, _)
          | (State.Started, _) => ("", ReactDOMRe.Style.make())
          }
        | None => ("", ReactDOMRe.Style.make())
        };
      <div className="bg-gray br2 mb2 ml1 mr1 mt0">
        <div
          key=card.cid
          className=(
            "bg-white-90 br2 pa2 helvetica f6 dark-gray bb b--silver user-select-none pointer"
            ++ " "
            ++ draggedClasses
          )
          style=(
            ReactDOMRe.Style.combine(
              draggedStyles,
              ReactDOMRe.Style.make(~visibility=showPlaceholderOnly ? "hidden" : "inherit", ())
            )
          )
          onMouseDown=onDragStart
          onMouseEnter>
          (ReasonReact.stringToElement(card.name))
        </div>
      </div>
    }
  };
};

module NewCardForm = {
  module OpenButton = {
    let component = ReasonReact.statelessComponent("NewCardForm.OpenButton");
    let make = (~openForm, _children) => {
      ...component,
      render: (_self) =>
        <div className="flex-none flex">
          <button
            className="bg-transparent b--transparent button-reset pointer flex flex-auto hover-bg-black-10 pa2 f6 user-select-none"
            onClick=openForm>
            (ReasonReact.stringToElement("Add a card..."))
          </button>
        </div>
    };
  };
  module OpenedForm = {
    let component = ReasonReact.statelessComponent("NewCardForm.OpenedForm");
    let make = (~newCardName, ~changeNewCardName, ~addCard, ~closeForm, ~setInputRef, _children) => {
      ...component,
      render: (_self) =>
        <Form className="flex-none flex flex-column" onSubmit=addCard>
          <div className="bg-white-90 br2 mb2 ml1 mr1 mt0 pa2 h3">
            <input
              value=newCardName
              className="bn bg-transparent input-reset outline-0"
              onChange=changeNewCardName
              ref=setInputRef
            />
          </div>
          <div className="flex flex-row items-center justify-start pl1 pb2">
            <Button.Submit
              disabled=(String.length(newCardName) === 0) className="h2 w3 fw7 ml0 mr1">
              (ReasonReact.stringToElement("Add"))
            </Button.Submit>
            <button
              className="bg-transparent bn pointer button-reset light-silver hover-dark-gray f3"
              _type="button"
              onClick=closeForm>
              (ReasonReact.stringToElement("X"))
            </button>
          </div>
        </Form>
    };
  };
  let component = ReasonReact.statelessComponent("NewCardForm");
  let make =
      (
        ~newCardForm,
        ~changeNewCardName,
        ~listCid,
        ~addCard,
        ~openForm,
        ~closeForm,
        ~setInputRef,
        _children
      ) => {
    ...component,
    render: (_self) =>
      switch newCardForm {
      | Some((newCardForm: State.newCardForm)) when listCid === newCardForm.listCid =>
        <OpenedForm newCardName=newCardForm.name changeNewCardName addCard closeForm setInputRef />
      | Some(_)
      | None => <OpenButton openForm />
      }
  };
};

module AddListForm = {
  module OpenButton = {
    let component = ReasonReact.statelessComponent("AddListForm.OpenButton");
    let make = (~openForm, _children) => {
      ...component,
      render: (_self) =>
        <button
          placeholder="Add a list..."
          className="bg-dark-green bn button-reset pointer br2 h2 pt1 pb1 pl2 white-80 w5 flex"
          onClick=openForm>
          (ReasonReact.stringToElement("Add a list..."))
        </button>
    };
  };
  module OpenedForm = {
    let component = ReasonReact.statelessComponent("AddListForm.OpenedForm");
    let make = (~addList, ~newListName, ~changeNewListName, ~closeForm, ~setInputRef, _children) => {
      ...component,
      render: (_self) =>
        <Form
          onSubmit=addList className="flex flex-column self-start br2 pa1 ma0 pa2 bg-moon-gray">
          <input
            value=newListName
            placeholder="Add a list..."
            className="outline-0 input-reset ba b--gray br2 h1 pt1 pb1 pl2 mb2 w5"
            onChange=changeNewListName
            ref=setInputRef
          />
          <div className="flex flex-row justify-start items-center">
            <button
              _type="submit"
              disabled=(Js.Boolean.to_js_boolean(String.length(newListName) === 0))
              className="h2 w3 pointer button-reset bg-green bn near-white fw7 br2 hover-bg-dark-green mr1">
              (ReasonReact.stringToElement("Save"))
            </button>
            <button
              _type="button"
              onClick=closeForm
              className="bg-transparent bn pointer button-reset light-silver hover-dark-gray f3">
              (ReasonReact.stringToElement("X"))
            </button>
          </div>
        </Form>
    };
  };
  let component = ReasonReact.statelessComponent("AddListForm");
  let make =
      (
        ~newListForm: State.newListForm,
        ~addList,
        ~changeNewListName,
        ~openForm,
        ~closeForm,
        ~setInputRef,
        _children
      ) => {
    ...component,
    render: (_self) =>
      newListForm.isOpen ?
        <OpenedForm
          newListName=newListForm.name
          addList
          changeNewListName
          closeForm
          setInputRef
        /> :
        <OpenButton openForm />
  };
};

module ClickOutsideWrapper = {
  type state = {containerRef: ref(option(Dom.element))};
  let initialState = () => {containerRef: ref(None)};
  type action =
    | NoOp;
  let reducer = (action, _state) =>
    switch action {
    | NoOp => ReasonReact.NoUpdate
    };
  let handleClickOutside =
      (~self: ReasonReact.self(state, ReasonReact.noRetainedProps, action), ~callback, ~domEvent) =>
    switch self.state.containerRef^ {
    | Some(r) =>
      let container = ReactDOMRe.domElementToObj(r);
      let target = Utils.Dom.domEventToObj(domEvent)##target;
      ! Js.to_bool(container##contains(target)) ? callback() : ()
    | None => ()
    };
  let component = ReasonReact.reducerComponent("ClickOutsideWrapper");
  let make = (~onClickOutside, children) => {
    ...component,
    initialState,
    didMount: (self) => {
      Utils.Dom.addEventListener(
        "click",
        (event) => handleClickOutside(~self, ~callback=onClickOutside, ~domEvent=event)
      );
      ReasonReact.NoUpdate
    },
    willUnmount: (self) =>
      Utils.Dom.removeEventListener(
        "click",
        (event) => handleClickOutside(~self, ~callback=onClickOutside, ~domEvent=event)
      ),
    reducer,
    render: ({handle}) =>
      ReasonReact.createDomElement(
        "div",
        ~props={
          "ref": handle((theRef, {state}) => state.containerRef := Js.Nullable.to_opt(theRef))
        },
        children
      )
  };
};

module EditBoardNamePopup = {
  type state = {
    inputValue: string,
    containerRef: ref(option(Dom.element)),
    inputRef: ref(option(Dom.element))
  };
  let initialState = (boardName) => {
    inputValue: boardName,
    containerRef: ref(None),
    inputRef: ref(None)
  };
  type action =
    | SetInputValue(string)
    | FocusInput;
  let reducer = (action, state) =>
    switch action {
    | SetInputValue(inputValue) => ReasonReact.Update({...state, inputValue})
    | FocusInput =>
      ReasonReact.SideEffects((({state}) => Utils.Dom.focusAndHighlightElement(state.inputRef)))
    };
  let component = ReasonReact.reducerComponent("EditBoardNamePopup");
  let make = (~boardName, ~closeForm, ~onSubmit, _children) => {
    ...component,
    initialState: () => initialState(boardName),
    didMount: ({reduce}) => {
      reduce(() => FocusInput, ());
      ReasonReact.NoUpdate
    },
    reducer,
    render: ({state, reduce, handle}) =>
      <div
        ref=(handle((theRef, {state}) => state.containerRef := Js.Nullable.to_opt(theRef)))
        style=(
          ReactDOMRe.Style.make(~position="absolute", ~left="8px", ~top="90px", ~width="300px", ())
        )
        className="br2 bg-near-white ba b--silver flex flex-column">
        <ClickOutsideWrapper onClickOutside=((_event) => closeForm())>
          <div>
            <div className="flex flex-row justify-between items-center ml1 mr1 bb b--moon-gray h2">
              <span style=(ReactDOMRe.Style.make(~visibility="hidden", ()))>
                (ReasonReact.stringToElement("X"))
              </span>
              <span className="helvetica f6 gray">
                (ReasonReact.stringToElement("Rename Board"))
              </span>
              <button
                _type="button"
                onClick=((_event) => closeForm())
                className="bg-transparent bn pointer button-reset light-silver hover-dark-gray f7">
                (ReasonReact.stringToElement("X"))
              </button>
            </div>
          </div>
          <Form className="flex flex-column ma2" onSubmit=((_event) => onSubmit(state.inputValue))>
            <label className="pb1 fw7 helvetica f6 dark-gray">
              (ReasonReact.stringToElement("Name"))
            </label>
            <input
              value=state.inputValue
              onChange=(
                reduce(
                  (event) =>
                    SetInputValue(
                      ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
                    )
                )
              )
              ref=(handle((theRef, {state}) => state.inputRef := Js.Nullable.to_opt(theRef)))
              className="pa2 mb3 br2 input-reset ba b--silver"
            />
            <button
              _type="submit"
              className="h2 pointer button-reset bg-green bn near-white fw7 br2 hover-bg-dark-green mr1 self-start mb1 ml1 w4"
              disabled=(Js.Boolean.to_js_boolean(String.length(state.inputValue) === 0))>
              (ReasonReact.stringToElement("Rename"))
            </button>
          </Form>
        </ClickOutsideWrapper>
      </div>
  };
};