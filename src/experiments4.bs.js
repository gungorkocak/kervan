// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Hyperapp_hacked = require("./hyperapp_hacked");

function h(prim, prim$1, prim$2) {
  return Hyperapp_hacked.h(prim, prim$1, prim$2);
}

function app(prim) {
  Hyperapp_hacked.app(prim);
  return /* () */0;
}

function update(state, param) {
  if (typeof param === "number") {
    if (param === 0) {
      return state + 1 | 0;
    } else {
      return state - 1 | 0;
    }
  } else if (param.tag) {
    return Caml_int32.imul(state, param[0]);
  } else {
    return param[0];
  }
}

function hh(tag, props, children) {
  return Hyperapp_hacked.h(tag, $$Array.of_list(props), $$Array.of_list(children));
}

function view(state) {
  return hh("div", /* :: */[
              /* Attr */Block.__(0, [/* tuple */[
                    "id",
                    "garip"
                  ]]),
              /* [] */0
            ], /* :: */[
              hh("div", /* :: */[
                    /* Attr */Block.__(0, [/* tuple */[
                          "id",
                          "bibaskagarip"
                        ]]),
                    /* [] */0
                  ], /* :: */[
                    /* Text */[String(state)],
                    /* [] */0
                  ]),
              /* :: */[
                hh("button", /* :: */[
                      /* Handler */Block.__(1, [/* tuple */[
                            "onclick",
                            /* Increment */0
                          ]]),
                      /* [] */0
                    ], /* :: */[
                      /* Text */["+"],
                      /* [] */0
                    ]),
                /* :: */[
                  hh("button", /* :: */[
                        /* Attr */Block.__(0, [/* tuple */[
                              "id",
                              "gulizar"
                            ]]),
                        /* :: */[
                          /* Handler */Block.__(1, [/* tuple */[
                                "onmouseover",
                                /* Decrement */1
                              ]]),
                          /* :: */[
                            /* Handler */Block.__(1, [/* tuple */[
                                  "onclick",
                                  /* Decrement */1
                                ]]),
                            /* [] */0
                          ]
                        ]
                      ], /* :: */[
                        /* Text */["-"],
                        /* [] */0
                      ]),
                  /* :: */[
                    hh("button", /* :: */[
                          /* Handler */Block.__(1, [/* tuple */[
                                "onclick",
                                /* Set */Block.__(0, [24])
                              ]]),
                          /* [] */0
                        ], /* :: */[
                          /* Text */["Set to 42"],
                          /* [] */0
                        ]),
                    /* :: */[
                      hh("button", /* :: */[
                            /* Handler */Block.__(1, [/* tuple */[
                                  "onclick",
                                  /* Mult */Block.__(1, [2])
                                ]]),
                            /* [] */0
                          ], /* :: */[
                            /* Text */["mult * 2"],
                            /* [] */0
                          ]),
                      /* [] */0
                    ]
                  ]
                ]
              ]
            ]);
}

var prim = {
  init: 0,
  update: update,
  view: view,
  node: document.getElementById("app")
};

Hyperapp_hacked.app(prim);

var init = 0;

exports.h = h;
exports.app = app;
exports.init = init;
exports.update = update;
exports.hh = hh;
exports.view = view;
/* prim Not a pure module */
