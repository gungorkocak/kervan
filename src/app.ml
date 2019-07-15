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

    | SubStop (_, stop_fn), SubStart (key, start_fn) ->
      let () = stop_fn () in
      SubStop (key, start_fn dispatch)

    | _, _ ->
      SubNone


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


type tagname = string

type 'msg prop =
  | Attr of (string * string)
  | Handler of (string * 'msg)

type vdom =
  | VDom
  | Text of string


external h : tagname -> 'msg prop array -> vdom array -> vdom   = "h" [@@bs.module "./superfine1"]
external patch : 'node -> vdom -> ('msg -> unit -> unit) -> unit = "patch" [@@bs.module "./superfine1"]


let node tag props children =
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
  (* TODO: figure out how to prevent double init () on state ref *)
  let state = ref (init ()) in
  let subs = ref [] in
  let dispatch_fn = ref (fun _ -> ()) in

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
  set_state (init ())
