
module App =
struct
  type tagname = string

  type 'msg prop =
    | Attr of (string * string)
    | Handler of (string * 'msg)

  type vdom =
    | VDom
    | Text of string


  external h : tagname -> 'msg prop array -> vdom array -> vdom   = "h" [@@bs.module "./superfine1"]
  external patch : 'node -> vdom -> ('msg -> (unit -> unit)) -> unit = "patch" [@@bs.module "./superfine1"]


  let hh tag props children =
    h tag (Array.of_list props) (Array.of_list children)

  type ('state, 'msg, 'node) app =
    { init: unit -> 'state
    ; update: 'state -> 'msg -> 'state
    ; view: 'state -> vdom
    ; node: 'node
    }

  let app { init ; update ; view ; node } =
    let state = ref (init ()) in
    let rec set_state next_state =
      let () = state := next_state in
      let dispatch msg =
        let dispatch_fn () = set_state (update !state msg) in
        dispatch_fn
      in
      let () = patch node (view !state) dispatch in
      ()
    in
    set_state !state

end

(* Example App *)

open App

type dom
type id

external dom : dom = "document" [@@bs.val]
external get_by_id : dom -> string -> id = "getElementById" [@@bs.send]

type state = int

type msg =
  | Increment


let init () = 0


let update state = function
  | Increment -> state + 1


let view state =
  hh "div"
    [ Attr("id", "hello") ]
    [ hh "div"
        [
          Attr("id", "naber")
        ]
        [ Text(string_of_int state) ]
    ; hh "button"
        [ Attr("id", "btn-inc")
        ; Handler("onclick", Increment)
        ]
        [ Text("+") ]
    ]

let () =
  app { init=init ; view=view ; update=update ; node=(get_by_id dom "app") }
