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

module View =
struct
  type tagname = string

  type 'msg prop =
    | Attr of (string * string)
    | Handler of (string * 'msg)

  type 'msg vnode =
    | VNodeNone
    | VNode of 'msg vnode_rec
    | Text of string

  and 'msg vnode_rec =
    { key : string
    ; name : string
    ; props : 'msg prop list
    ; children : 'msg vnode list
    ; node : Dom.node option
    }

  external parent_node : Dom.node -> Dom.node = "parentNode" [@@bs.get]
  external node_type : Dom.node -> int = "nodeType" [@@bs.get]
  external node_value : Dom.node -> string = "nodeValue" [@@bs.get]
  external node_name : Dom.node -> string = "nodeName" [@@bs.get]
  external child_nodes : Dom.node -> Dom.node array = "childNodes" [@@bs.get]

  external vdom : Dom.node -> 'msg vnode option = "vdom" [@@bs.get]
  external set_vdom : Dom.node -> 'msg vnode -> unit = "vdom" [@@bs.set]



  (* external h : tagname -> 'msg prop array -> vnode array -> vnode   = "h" [@@bs.module "./superfine1"] *)
  (* external patch : 'node -> ('msg, 'vdom) vnode -> ('msg -> unit -> unit) -> unit = "patch" [@@bs.module "./superfine1"] *)

  let rec recycle_node node = match node_type node with
    | 1 (* TEXT_NODE *)     ->
      Text (node_value node)

    | _ (* RECYCLED_NODE *) ->
      VNode
        { name=(node_name node)
        ; key=""
        ; props=[]
        ; children=(Array.to_list (Js.Array.map recycle_node (child_nodes node)))
        ; node=(Some node)
        }

  let vdom_of_node node = match vdom node with
    | None -> recycle_node node
    | Some vdom -> vdom

  let patch_node parent node old_vnode vnode dispatch isSvg = match old_vnode, vnode with
    | VNodeNone, VNodeNone ->
      node
    | VNodeNone, Text str ->
      (* create new text node *)
      node
    | VNodeNone, VNode new_rec ->
      (* create new vnode *)
      node
    | Text old_str, VNodeNone ->
      (* remove text node *)
      node
    | Text old_str, Text str ->
      (* check string equality update node if necessary *)
      node
    | Text old_str, VNode new_rec ->
      (* remove text node , create new vnode *)
      node
    | VNode old_rec, VNode new_rec ->
      (* check vnode equality, update node if necessary *)
      node
    | VNode old_rec, VNodeNone ->
      (* remove old vnode *)
      node
    | VNode old_rec, Text str ->
      (* remove old vnode, create new text *)
      node

  let patch node vdom dispatch =
    let node = patch_node (parent_node node) node (vdom_of_node node) vdom dispatch false in
    set_vdom node vdom

  let vnode name ?(key="") props children =
    VNode { name=name ; key=key ; props=props ; children=children ; node=None }

  let render node view state event_handler =
    let () = patch node (view state) event_handler in
    ()
end


type ('state, 'msg, 'node) app =
  { init: unit -> 'state
  ; update: 'state -> 'msg -> 'state * ('msg Fx.t)
  ; view: 'state -> 'msg View.vnode
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

  let set_state next_state =
    let () = state := next_state in
    let () = subs := Sub.patch !subs (subscriptions !state) [] dispatch in
    View.render node view !state event_handler
  in

  let () = dispatch_fn := (
    fun msg ->
      let next_state, fx = update !state msg in
      let () = Fx.run fx dispatch in
      set_state next_state
  )
  in
  set_state (init ())

let vnode = View.vnode
