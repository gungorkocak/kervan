
module Fx =
struct
  type 'msg fx_fn = ('msg -> unit -> unit) -> unit

  type 'msg t =
    | FxNone
    | FxOne of 'msg fx_fn

  let none = FxNone
  let one fn = FxOne fn

  let run fx dispatch = match fx with
    | FxNone -> ()
    | FxOne fn -> fn dispatch
end


module Sub =
struct
  type 'msg sub_fn = ('msg -> unit -> unit) -> (unit -> unit)

  type 'msg sub =
    | SubNone
    | SubOne of 'msg sub_fn

  type ('state, 'msg) t = 'state -> 'msg sub list

  let none = SubNone
  let one fn = SubOne fn

  let rec patch old_subs subs dispatch next_subs = match old_subs, subs with
    | [], [] ->
      next_subs

    | [], sub :: subs ->
      patch [] subs dispatch (sub :: next_subs)

    | old_sub :: old_subs, [] ->
      patch  old_subs subs dispatch (old_sub :: next_subs)

    | old_sub :: old_subs, sub :: subs ->
      patch old_subs subs dispatch (sub :: next_subs)


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
  external patch : 'node -> vdom -> ('msg -> unit -> unit) -> unit = "patch" [@@bs.module "./superfine1"]


  let hh tag props children =
    h tag (Array.of_list props) (Array.of_list children)

  type ('state, 'msg, 'node) app =
    { init: unit -> 'state
    ; update: 'state -> 'msg -> 'state * ('msg Fx.t)
    ; view: 'state -> vdom
    ; subscriptions: ('state, 'msg) Sub.t
    ; node: 'node
    }

  type 'msg dispatch = 'msg -> unit

  let app { init ; update ; view ; subscriptions; node } =
    let state = ref (init ()) in
    let subs = ref [] in
    let dispatch_fn = ref (fun msg () -> ()) in
    let dispatch msg = !dispatch_fn msg in

    let render state dispatch =
      let () = patch node (view state) dispatch in
      ()
    in

    let set_state next_state =
      let () = state := next_state in
      let () = subs := Sub.patch !subs (subscriptions !state) dispatch [] in
      let () = Js.log2 "haaaa" !subs in
      render !state dispatch
    in

    let () = dispatch_fn := (
      fun msg () ->
        let next_state, fx = update !state msg in
        let () = Fx.run fx dispatch in
        set_state next_state
    )
    in
    let () = patch node (view !state) dispatch in

    render !state dispatch
end

(* Example App *)

open App

type dom
type id

external dom : dom = "document" [@@bs.val]
external get_by_id : dom -> string -> id = "getElementById" [@@bs.send]

module EffectTest =
struct
  let log str =
    (fun _ -> Js.log str)


  let delay ms msg =
    (fun dispatch ->
       let _ = Js.Global.setTimeout (fun () -> dispatch msg ()) ms in
       ()
    )
end

module SubTest =
struct
  let every ms msg =
    (fun dispatch ->
       let id = Js.Global.setInterval (fun () -> dispatch msg ()) ms in
       (fun () -> Js.Global.clearInterval id)
    )
end

type state = int

type msg =
  | Increment
  | Decrement
  | Power
  | Boost


let init () = 0


let subscriptions state =
  [ Sub.none
  ; Sub.one (SubTest.every 3000 Increment)
  ]


let update state = function
  | Increment -> state + 1, Fx.none
  | Decrement -> state - 1, Fx.one (EffectTest.log state)
  | Power -> state * state, Fx.none
  | Boost -> state + 1, Fx.one (EffectTest.delay 1000 Power)



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
        ; Handler("onclick", Boost)
        ]
        [ Text("++") ]
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
  app
    { init=init
    ; view=view
    ; update=update
    ; subscriptions=subscriptions
    ; node=(get_by_id dom "app")
    }
