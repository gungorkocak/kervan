
type state = int

type dom
type id
type tagname = string

type msg =
  | Increment
  | Decrement
  | Set of int
  | Mult of int

type prop =
  | Attr of (string * string)
  | Handler of (string * msg)

type props = prop list

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
external h : tagname -> prop array -> vdom array -> vdom = "h" [@@bs.module "./hyperapp_hacked"]
external app : program -> unit = "app" [@@bs.module "./hyperapp_hacked"]

let init = 0

let update state = function
  | Increment -> state + 1
  | Decrement -> state - 1
  | Set num -> num
  | Mult num -> state * num


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
  app (program ~init:init ~update:update ~view:view ~node:(get_by_id dom "app"))
