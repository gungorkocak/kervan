type state = int

type num_record =
  {
    num: int
  }

type 'a msg = (state -> 'a option -> state) * 'a option

type dom
type id
type action
type tagname = string

type 'a props =
  {
    id: string
  ; onclick: 'a msg [@bs.optional]
  } [@@bs.deriving abstract]

type vdom =
  | VDom
  | Text of string

type program =
  {
    init : int
  ; view : (state -> vdom)
  ; node : id
  } [@@bs.deriving abstract]

external dom : dom = "document" [@@bs.val]
external get_by_id : dom -> string -> id = "getElementById" [@@bs.send]
external h : tagname -> 'a props -> vdom array -> vdom = "h" [@@bs.module "hyperapp"]
external app : program -> unit = "app" [@@bs.module "hyperapp"]

let init = 0

let increment state _ = state + 1

let decrement state _ = state - 1

let set (state : state) (num : num_record option) = match num with
  | Some num -> num.num
  | None -> state

let multiply state = function
  | Some num -> state * num
  | None -> state


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
        (props ~id:"mehmet" ~onclick:(increment, None) ())
        [ Text("+")]
    ; hh "button"
        (props ~id:"ayse" ~onclick:(decrement, None) ())
        [ Text("-")]
    ; hh "button"
        (props ~id:"fatme" ~onclick:(set, Some { num=42 }) ())
        [ Text("Set to 42") ]
    ; hh "button"
        (props ~id:"geysa" ~onclick:(multiply, Some 2) ())
        [ Text("mult * 2") ]
    ]


let () =
  app (program ~init:init ~view:view ~node:(get_by_id dom "app"))
