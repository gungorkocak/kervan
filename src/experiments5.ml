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
  type 'msg delay = (int * 'msg -> ('msg -> unit -> unit) -> unit)
  type log = string -> unit

  let log str =
    Js.log str

  let delay (ms, msg) dispatch =
    let _ = Js.Global.setTimeout (dispatch msg) ms in
    ()
end

module SubTest = struct
  type 'msg every = (('msg -> unit -> unit) -> int * 'msg ->  (unit -> unit))

  let every dispatch (ms, msg) =
    let id = Js.Global.setInterval (dispatch msg) ms in
    fun () -> Js.Global.clearInterval id
end

type msg =
  | Increment
  | Decrement
  | Set of int
  | Mult of int
  | DelayedIncrement
  | Log of string

type fx =
  | NoFx
  | Delay of (msg EffectTest.delay) * (int * msg)
  | Log of EffectTest.log * string

type subscriptions =
  | NoSub
  | Every of (msg SubTest.every) * (int * msg)


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

let subscriptions _state =
  [|
    Every (SubTest.every, (1000, Increment))
  |]

let update state = function
  | Increment -> state + 1, NoFx
  | Decrement -> state - 1, NoFx
  | Set num -> num, NoFx
  | Mult num -> state * num, Delay (EffectTest.delay, (1000, (Set 42)))
  | Log str -> state, Log (EffectTest.log, str)


let hh tag props children =
  h tag (Array.of_list props) (Array.of_list children)

let view state =
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
          Handler("onclick", (Log (string_of_int state)))
        ]
        [ Text("Log")]
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
