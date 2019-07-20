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
  type 'msg prop =
    | PropNone
    | Attr of (string * string)
    | Handler of (string * 'msg)

  type 'msg vnode =
    | VNodeNone
    | VNode of 'msg vnode_rec
    | VText of vtext_rec

  and vtext_rec =
    { key : string
    ; str : string
    ; mutable node : Dom.node option
    }

  and 'msg vnode_rec =
    { key : string
    ; name : string
    ; props : 'msg prop list
    ; children : 'msg vnode list
    ; mutable node : Dom.node option
    }

  type 'msg patch_op =
    | Create of ('msg vnode) * int
    | Update of ('msg vnode) * ('msg vnode)
    | Move of ('msg vnode) * ('msg vnode) * int
    | Delete of 'msg vnode

  external dom : Dom.document = "document" [@@bs.val]

  external create_text_node : Dom.document -> string -> Dom.node = "createTextNode" [@@bs.send]
  external create_element : Dom.document -> string -> Dom.node = "createElement" [@@bs.send]

  external set_node_value   : string -> Dom.node -> Dom.node = "setNodeValue" [@@bs.module "./js_utils"]
  external set_attribute    : string -> string -> Dom.node -> Dom.node = "setAttribute" [@@bs.module "./js_utils"]
  external remove_attribute : string -> Dom.node -> Dom.node = "removeAttribute" [@@bs.module "./js_utils"]

  external insert_before : Dom.node -> Dom.node -> Dom.node -> Dom.node = "insertBefore" [@@bs.module "./js_utils"]
  external remove_child  : Dom.node -> Dom.node -> Dom.node = "removeChild" [@@bs.module "./js_utils"]

  external parent_node : Dom.node -> Dom.node = "parentNode" [@@bs.module "./js_utils"]
  external child_node_at : int -> Dom.node -> Dom.node = "childNodeAt" [@@bs.module "./js_utils"]

  external set_event_handler : string -> (('msg -> Dom.event -> unit) * 'msg) option -> Dom.node -> Dom.node = "setEventHandler" [@@bs.module "./js_utils"]

  external get_vdom : Dom.node -> ('msg vnode) option = "getVdom" [@@bs.module "./js_utils"]
  external set_vdom : 'msg vnode -> Dom.node -> Dom.node = "setVdom" [@@bs.module "./js_utils"]

  let with_node = function
    | Some node -> node
    | _         -> failwith "node must be supplied"

  let node_of_vnode = function
    | VText vtext -> with_node vtext.node
    | VNode vnode -> with_node vnode.node
    | _           -> failwith "cannot get node of VNodeNone"

  let set_node_of_vtext (vtext : vtext_rec) node =
    let () = vtext.node <- Some node in node

  let set_node_of_vnode (vnode : 'msg vnode_rec) node =
    let () = vnode.node <- Some node in node


  let key_of_vnode = function
    | VText { key } -> key
    | VNode { key } -> key
    | VNodeNone     -> ""

  let keys_are_equal vn1 vn2 =
    if (key_of_vnode vn1) = (key_of_vnode vn2) then true else false

  let cache_vnode dict = function
    | vnode when (key_of_vnode vnode) <> "" ->
      let () = Js.Dict.set dict (key_of_vnode vnode) vnode in
      dict

    | _ ->
      dict


  let cached_vnodes_of vnodes =
    List.fold_left cache_vnode (Js.Dict.empty ()) vnodes


  let patch_prop event_handler prev_prop next_prop node = match prev_prop, next_prop with
    | PropNone, Attr (key, value) ->
      node
      |> set_attribute key value

    | PropNone, Handler (key, msg) ->
      node
      |> set_event_handler key (Some (event_handler, msg))

    | Attr (key, _), PropNone ->
      node
      |> remove_attribute key

    | Attr (prev_key, _), Handler (next_key, msg) ->
      node
      |> remove_attribute prev_key
      |> set_event_handler next_key (Some (event_handler, msg))

    | Attr (prev_key, _), Attr (next_key, next_value) when prev_key = next_key ->
      node
      |> set_attribute next_key next_value

    | Attr (prev_key, _), Attr (next_key, next_value) ->
      node
      |> remove_attribute prev_key
      |> set_attribute next_key next_value

    | Handler (key, _), PropNone ->
      node
      |> set_event_handler key None

    | Handler (prev_key, _), Attr (next_key, next_value) ->
      node
      |> set_event_handler prev_key None
      |> set_attribute next_key next_value

    | Handler (prev_key, prev_msg), Handler (next_key, next_msg) when prev_key = next_key ->
      node
      |> set_event_handler next_key (Some (event_handler, next_msg))

    | Handler (prev_key, _), Handler (next_key, next_msg) ->
      node
      |> set_event_handler prev_key None
      |> set_event_handler next_key (Some (event_handler, next_msg))

    | _, _ ->
      node


  let rec patch_props event_handler prev_props next_props node = match prev_props, next_props with
    | [], [] ->
      node

    | prev_prop :: prev_props, [] ->
      node
      |> patch_prop event_handler prev_prop PropNone
      |> patch_props event_handler prev_props []

    | [], next_prop :: next_props ->
      node
      |> patch_prop event_handler PropNone next_prop
      |> patch_props event_handler [] next_props

    | prev_prop :: prev_props, next_prop :: next_props ->
      node
      |> patch_prop event_handler prev_prop next_prop
      |> patch_props event_handler prev_props next_props


  let action_of_cached cached_vnodes next_vnode index = match Js.Dict.get cached_vnodes (key_of_vnode next_vnode) with
    | Some prev_vnode -> (Move (prev_vnode, next_vnode, index))
    | None            -> (Create (next_vnode, index))

  let rec patch_child_nodes event_handler index cached_vnodes prev_vnodes next_vnodes parent = match prev_vnodes, next_vnodes with
    | [], [] ->
      parent

    | prev_vnode :: prev_vnodes, [] ->
      parent
      |> patch_node event_handler (Delete prev_vnode)
      |> patch_child_nodes event_handler (index + 1) cached_vnodes prev_vnodes []

    | [], next_vnode :: next_vnodes when (key_of_vnode next_vnode) = "" ->
      parent
      |> patch_node event_handler (Create (next_vnode, index))
      |> patch_child_nodes event_handler (index + 1) cached_vnodes [] next_vnodes

    | [], next_vnode :: next_vnodes ->
      parent
      |> patch_node event_handler (action_of_cached cached_vnodes next_vnode index)
      |> patch_child_nodes event_handler (index + 1) cached_vnodes [] next_vnodes

    | prev_vnode :: prev_vnodes, next_vnode :: next_vnodes when (keys_are_equal prev_vnode next_vnode) ->
      parent
      |> patch_node event_handler (Update (prev_vnode, next_vnode))
      |> patch_child_nodes event_handler (index + 1) cached_vnodes prev_vnodes next_vnodes

    | prev_vnode :: prev_vnodes, next_vnode :: next_vnodes ->
      parent
      |> patch_node event_handler (Delete prev_vnode)
      |> patch_node event_handler (action_of_cached cached_vnodes next_vnode index)
      |> patch_child_nodes event_handler (index + 1) cached_vnodes prev_vnodes next_vnodes


  and patch_node event_handler op parent = match op with
    | Create (vnode, pos)                  -> create_node event_handler vnode pos parent
    | Update (prev_vnode, next_vnode)      -> update_node event_handler prev_vnode next_vnode parent
    | Move   (prev_vnode, next_vnode, pos) -> move_node event_handler prev_vnode next_vnode pos parent
    | Delete vnode                         -> delete_node vnode parent


  and create_node event_handler vnode pos parent = match vnode with
    | VText vtext ->
      let node =
        create_text_node dom vtext.str
        |> set_node_of_vtext vtext
      in
      parent
      |> insert_before node (child_node_at pos parent)

    | VNode vnode ->
      let node =
        create_element dom vnode.name
        |> set_node_of_vnode vnode
        |> patch_props event_handler [] vnode.props
        |> patch_child_nodes event_handler 0 (Js.Dict.empty ()) [] vnode.children
      in
      parent
      |> insert_before node (child_node_at pos parent)

    | VNodeNone ->
      parent



  and update_node event_handler prev_vnode next_vnode parent = match prev_vnode, next_vnode with
    | VText prev_vtext, VText next_vtext ->
      let _ =
        (with_node prev_vtext.node)
        |> set_node_of_vtext next_vtext
        |> set_node_value next_vtext.str
      in
      parent

    | VNode prev_vnode, VNode next_vnode ->
      let _ =
        (with_node prev_vnode.node)
        |> set_node_of_vnode next_vnode
        |> patch_props event_handler prev_vnode.props next_vnode.props
        |> patch_child_nodes event_handler 0 (cached_vnodes_of prev_vnode.children) prev_vnode.children next_vnode.children
      in
      parent

    | _, _ ->
      parent


  and move_node event_handler prev_vnode next_vnode pos parent =
    parent
    |> update_node event_handler prev_vnode next_vnode
    |> insert_before (node_of_vnode next_vnode) (child_node_at pos parent)


  and delete_node vnode parent =
    remove_child (node_of_vnode vnode) parent


  let render_op prev_vdom next_vdom = match prev_vdom with
    | None            -> (Create (next_vdom, 0))
    | Some prev_vdom  -> (Update (prev_vdom, next_vdom))

  let render next_vdom event_handler node =
    node
    |> set_vdom next_vdom
    |> patch_node event_handler (render_op (get_vdom node) next_vdom)

  let vnode ?(key = "") name props children =
    VNode { key ; name ; props ; children ; node=None }

  let text ?(key = "") str =
    VText { key ; str ; node=None }

  let none =
    VNodeNone

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
  let event_handler msg _event = !dispatch_fn msg in

  let dispatch msg = !dispatch_fn msg in

  let set_state next_state =
    let () = state := next_state in
    let () = subs := Sub.patch !subs (subscriptions !state) [] dispatch in
    let _  = View.render (view !state) event_handler node in
    ()
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
let text = View.text
let none = View.none
