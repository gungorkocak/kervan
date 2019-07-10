(* Main kervan modules *)

module App =
struct
  type tagname = string

  type 'msg prop =
    | Attr of (string * string)
    | Handler of (string * 'msg)

  type vdom =
    | VDom
    | Text of string

  type ('state, 'msg, 'fx, 'subs, 'node) program =
    { init : unit -> 'state
    ; update : 'state -> 'msg -> 'state * 'fx
    ; view : 'state -> vdom
    ; subscriptions : 'state -> 'subs array
    ; node : 'node
    }
  [@@bs.deriving {jsConverter = newType}]

  external h : tagname -> 'msg prop array -> vdom array -> vdom = "h" [@@bs.module "./hyperapp_hacked"]
  external app_ext : ('state, 'msg, 'fx, 'subs, 'node) abs_program -> unit = "app" [@@bs.module "./hyperapp_hacked"]

  let app program = app_ext (programToJs program)
end


(* Effect and Sub Tests *)

module EffectTest = struct
  type 'msg t = [
    | `Log of (string -> unit) * string
    | `Delay of (int * 'msg -> ('msg -> unit -> unit) -> unit) * (int * 'msg)
  ]

  let log_effect str =
    Js.log str

  let log str = `Log (log_effect, str)

  let delay_effect (ms, msg) dispatch =
    let _ = Js.Global.setTimeout (dispatch msg) ms in
    ()

  let delay ms msg = `Delay (delay_effect, (ms, msg))
end

module SubTest = struct
  type 'msg t = [
    | `Every of (('msg -> unit -> unit) -> int * 'msg ->  (unit -> unit)) * (int * 'msg)
  ]

  let every_effect dispatch (ms, msg) =
    let id = Js.Global.setInterval (dispatch msg) ms in
    fun () -> Js.Global.clearInterval id


  let every ms msg = `Every (every_effect, (ms, msg))
end

(* Example App *)

type dom
type id

external dom : dom = "document" [@@bs.val]
external get_by_id : dom -> string -> id = "getElementById" [@@bs.send]


type state = int

type msg =
  | Increment
  | Decrement
  | Set of int
  | Mult of int
  | DelayedIncrement

type fx = [
  | `NoFx
  | msg EffectTest.t
]

type subscriptions = [
  | `NoSub
  | msg SubTest.t
]

let init () = 0

let subscriptions state =
  [|
    SubTest.every 1000 Increment
  |]

let update state = function
  | Increment -> state + 1, `NoFx
  | Decrement -> state - 1, `NoFx
  | Set num -> num, `NoFx
  | Mult num -> state * num, EffectTest.delay 1000 (Set 99)
  | DelayedIncrement -> state, EffectTest.log "ahmet naber?"


let hh tag props children =
  App.h tag (Array.of_list props) (Array.of_list children)

let view (state) =
  hh "div"
    [
      Attr("id", "garip")
    ]
    [
      hh "div"
        [
          Attr("id", "bibaskagarip")
        ]
        [ Text(string_of_int state)]
    ; hh "button"
        [
          Handler("onclick", Increment)
        ]
        [ Text("+")]
    ; hh "button"
        [
          Handler("onclick", DelayedIncrement)
        ]
        [ Text("~1000 |> +")]
    ; hh "button"
        [
          Attr("id", "gulizar")
        ; Handler("onmouseover", Decrement)
        ; Handler("onclick", Decrement)
        ]
        [ Text("-")]
    ; hh "button"
        [
          Handler("onclick", (Set 24))
        ]
        [ Text("Set to 42") ]
    ; hh "button"
        [
          Handler("onclick", (Mult 2))
        ]
        [ Text("mult * 2") ]
    ]


let () =
  App.app
    { init=init
    ; update=update
    ; view=view
    ; subscriptions=subscriptions
    ; node=(get_by_id dom "app")
    }
