/* 
refmt doesn't seem to like type definitions with
[@bs.meth] and [@bs.set] so throwing this in a
separate file for now
*/
type inputObj = {.
    [@bs.meth] "focus": unit => unit,
    [@bs.set] "selectionStart": int,
    [@bs.set] "selectionEnd": int,
    "value": string
};
