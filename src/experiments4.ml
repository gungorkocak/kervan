
type state = int

type dom
type id
type tagname = string

module Cmd = struct
  type effect_fn = string -> unit
  type effect = effect_fn * string

  type t =
    | NoCmd
    | One of effect

  let none = NoCmd
  let one effect = One effect

end

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


type prop =
  | Attr of (string * string)
  | Handler of (string * msg)

type props = prop list

type vdom =
  | VDom
  | Text of string

type program =
  { init : state
  ; update : state -> msg -> state * fx
  ; view : state -> vdom
  ; subscriptions : state -> subscriptions array
  ; node : id
  } [@@bs.deriving abstract]

type action


external dom : dom = "document" [@@bs.val]
external get_by_id : dom -> string -> id = "getElementById" [@@bs.send]
external h : tagname -> prop array -> vdom array -> vdom = "h" [@@bs.module "./hyperapp_hacked"]
external app : program -> unit = "app" [@@bs.module "./hyperapp_hacked"]

let init = 0

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
  h tag (Array.of_list props) (Array.of_list children)

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
  app (program ~init:init ~update:update ~view:view ~subscriptions:subscriptions ~node:(get_by_id dom "app"))
