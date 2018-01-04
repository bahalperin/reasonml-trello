type inputObj = {.
    [@bs.meth] "focus": unit => unit,
    [@bs.set] "selectionStart": int,
    [@bs.set] "selectionEnd": int,
    "value": string
};
