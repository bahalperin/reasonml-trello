[@bs.module "react-transition-group/Transition"]
external jsTransitionClass : ReasonReact.reactClass =
  "default";

type state =
  | Entering
  | Entered
  | Exiting
  | Exited
  | NotTransitioning;

let stateOfString = (str) =>
  switch str {
  | "entering" => Entering
  | "entered" => Entered
  | "exiting" => Exiting
  | "exited" => Exited
  | _ => NotTransitioning
  };

let make = (~in_: bool, ~timeout: int, children: state => ReasonReact.reactElement) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=jsTransitionClass,
    ~props={"in": Js.Boolean.to_js_boolean(in_), "timeout": timeout},
    (strState, _childProps) => children(stateOfString(strState))
  );