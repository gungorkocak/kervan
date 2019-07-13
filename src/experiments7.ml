
module Fx =
struct
  type 'msg fx_fn = ('msg -> unit) -> unit

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
  type sub_stop_fn = unit -> unit
  type 'msg sub_start_fn = ('msg -> unit) -> sub_stop_fn

  type 'msg sub =
    | SubNone
    | SubStart of string * 'msg sub_start_fn
    | SubStop of string * sub_stop_fn

  type ('state, 'msg) t = 'state -> 'msg sub list

  let none = SubNone
  let start key start_fn = SubStart (key, start_fn)


  let call dispatch old_sub sub = match old_sub, sub with
    | SubNone, SubNone ->
      SubNone

    | SubStop (key, stop_fn), SubStop _ ->
      SubStop (key, stop_fn)

    | SubNone, SubStart (key, start_fn) ->
      SubStop (key, start_fn dispatch)

    | SubStop (_, stop_fn), SubNone ->
      let () = stop_fn () in
      SubNone

    | SubStop (old_key, stop_fn), SubStart (key, _) when old_key = key ->
      SubStop (old_key, stop_fn)

    | SubStop (old_key, stop_fn), SubStart (key, start_fn) ->
      let () = stop_fn () in
      SubStop (key, start_fn dispatch)

  let rec patch old_subs subs next_subs dispatch = match old_subs, subs with
    | [], [] ->
      List.rev next_subs

    | [], sub :: subs ->
      let next_sub = call dispatch SubNone sub in
      patch [] subs (next_sub :: next_subs) dispatch

    | old_sub :: old_subs, [] ->
      let _ = call dispatch old_sub SubNone in
      patch  old_subs subs next_subs dispatch

    | old_sub :: old_subs, sub :: subs ->
      let next_sub = call dispatch old_sub sub in
      patch old_subs subs (next_sub :: next_subs) dispatch

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
    (* figure out how to prevent double init () on state ref *)
    let state = ref (init ()) in
    let subs = ref [] in
    let dispatch_fn = ref (fun msg -> ()) in

    (* TODO: add event argument to handler function *)
    (* TODO: find a way to force named function for event handling *)
    let event_handler_fn msg () = !dispatch_fn msg in
    let event_handler msg = event_handler_fn msg in

    let dispatch msg = !dispatch_fn msg in

    let render state event_handler =
      let () = patch node (view state) event_handler in
      ()
    in

    let set_state next_state =
      let () = state := next_state in
      let () = subs := Sub.patch !subs (subscriptions !state) [] dispatch in
      render !state event_handler
    in

    let () = dispatch_fn := (
      fun msg ->
        let next_state, fx = update !state msg in
        let () = Fx.run fx dispatch in
        set_state next_state
    )
    in
    (* let () = patch node (view !state) dispatch in *)

    set_state (init ())
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
       let _ = Js.Global.setTimeout (fun () -> dispatch msg) ms in
       ()
    )
end

module SubTest =
struct
  let every ~key ms msg =
    let every_effect =
      (fun dispatch ->
        let () = Js.log "every start..." in
        let id = Js.Global.setInterval (fun () -> dispatch msg) ms in
        (fun () ->
           let () = Js.log "every stop..." in
           Js.Global.clearInterval id
        )
      )
    in
    Sub.start key every_effect
end

type state = int

type msg =
  | Increment
  | Decrement
  | Power
  | Boost


let init () = 0

let sub_key count = if count >= 20 then "ahmet5" else "ahmet"

let subscriptions state =
  [ Sub.none
  ; SubTest.every ~key:(sub_key state) 1000 Increment
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
