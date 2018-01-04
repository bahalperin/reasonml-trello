module type List = {let insertAt: (~index: int, ~elem: 'a, ~list: list('a)) => list('a);};

module List: List = {
  let rec splitAtHelper = (index, originalList, newList) =>
    switch originalList {
    | [] => (List.rev(newList), [])
    | [head, ...tail] when List.length(newList) < index =>
      splitAtHelper(index, tail, [head, ...newList])
    | [_head, ..._tail] => (List.rev(newList), originalList)
    };
  let splitAt = (index, list) => splitAtHelper(index, list, []);
  let insertAt = (~index, ~elem, ~list) => {
    let (first, rest) = splitAt(index, list);
    List.concat([first, [elem], rest])
  };
};

module Dom = {
  let focusElement = (element) =>
    Js.Global.setTimeout(
      () => Option.run((elem) => ReactDOMRe.domElementToObj(elem)##focus(), element^),
      200
    )
    |> ignore;
  let focusAndHighlightElement = (element) =>
    Js.Global.setTimeout(
      () =>
        Option.run(
          (elem) => {
            let inputObj: BsObj.inputObj = ReactDOMRe.domElementToObj(elem);
            inputObj##focus();
            inputObj##selectionStart#=0;
            inputObj##selectionEnd#=(String.length(inputObj##value))
          },
          element^
        ),
      200
    )
    |> ignore;
};