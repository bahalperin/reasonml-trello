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