
module Fx =
struct
  type ('props, 'dispatch) fx_fn = 'props -> 'dispatch -> unit

  type ('props, 'dispatch) t =
    | FxNone
    | FxOne of (('props, 'dispatch) fx_fn) * 'props

  let none = FxNone
  let one (fn, props) = FxOne (fn, props)

  let run fx dispatch = match fx with
    | FxNone -> ()
    | FxOne (fn, props) -> fn props dispatch
end


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

  type ('state, 'msg, 'fx_props, 'dispatch, 'node) app =
    { init: unit -> 'state
    ; update: 'state -> 'msg -> 'state * (('fx_props, 'dispatch) Fx.t)
    ; view: 'state -> vdom
    ; node: 'node
    }

  let app { init ; update ; view ; node } =
    let state = ref (init ()) in
    let dispatch_fn = ref (fun msg () -> ()) in
    let dispatch = ref (fun msg -> !dispatch_fn msg) in

    let render state dispatch =
      let () = patch node (view state) dispatch in
      ()
    in

    let set_state next_state =
      let () = state := next_state in
      render !state !dispatch
    in

    let () = dispatch_fn := (
      fun msg () ->
        let next_state, fx = update !state msg in
        let () = Fx.run fx !dispatch in
        set_state next_state
    )
    in
    let () = patch node (view !state) !dispatch in

    render !state !dispatch
end

(* Example App *)

open App

type dom
type id

external dom : dom = "document" [@@bs.val]
external get_by_id : dom -> string -> id = "getElementById" [@@bs.send]

module EffectTest =
struct
  let log_effect str _ = Js.log str

  let log str = log_effect, str
end

type state = int

type msg =
  | Increment
  | Decrement


let init () = 0


let update state = function
  | Increment -> state + 1, Fx.none
  | Decrement -> state - 1, Fx.one (EffectTest.log state)


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
    ; hh "button"
        [ Attr("id", "btn-dec")
        ; Handler("onclick", Decrement)
        ]
        [ Text("-") ]
    ]

let () =
  app { init=init ; view=view ; update=update ; node=(get_by_id dom "app") }
