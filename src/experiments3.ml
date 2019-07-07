
type state = int

type dom
type id
type tagname = string

type msg =
  | Increment
  | Decrement
  | Set of int
  | Mult of int

type props =
  {
    id: string
  ; onclick: msg [@bs.optional]
  } [@@bs.deriving abstract]

type vdom =
  | VDom
  | Text of string

type program =
  {
    init : int
  ; update : state -> msg -> state
  ; view : (state -> vdom)
  ; node : id
  } [@@bs.deriving abstract]

type action


external dom : dom = "document" [@@bs.val]
external get_by_id : dom -> string -> id = "getElementById" [@@bs.send]
external h : tagname -> props -> vdom array -> vdom = "h" [@@bs.module "./hyperapp_hacked"]
external app : program -> unit = "app" [@@bs.module "./hyperapp_hacked"]

let init = 0

let update state = function
  | Increment -> state + 1
  | Decrement -> state - 1
  | Set num -> num
  | Mult num -> state * num


let hh tag props children =
  h tag props (Array.of_list children)

let view (state) =
  hh "div"
    (props ~id:"naber" ())
    [
      hh "div"
        (props ~id:"ahmet" ())
        [ Text(string_of_int state)]
    ; hh "button"
        (props ~id:"mehmet" ~onclick:Increment ())
        [ Text("+")]
    ; hh "button"
        (props ~id:"ayse" ~onclick:Decrement ())
        [ Text("-")]
    ; hh "button"
        (props ~id:"fatme" ~onclick:(Set 42) ())
        [ Text("Set to 42") ]
    ; hh "button"
        (props ~id:"geysa" ~onclick:(Mult 2) ())
        [ Text("mult * 2") ]
    ]


let () =
  app (program ~init:init ~update:update ~view:view ~node:(get_by_id dom "app"))
