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

  external create_text_node : Dom.document -> string -> Dom.node option = "createTextNode" [@@bs.send]
  external create_element : Dom.document -> string -> Dom.node option = "createElement" [@@bs.send]

  external set_node_value : Dom.node option -> string -> unit = "nodeValue" [@@bs.set]
  external set_attribute : Dom.node option -> string -> string -> unit = "setAttribute" [@@bs.send]
  external remove_attribute : Dom.node option -> string -> unit = "removeAttribute" [@@bs.send]

  external insert_before : Dom.node option -> Dom.node option -> Dom.node option -> unit = "insertBefore" [@@bs.send]
  external remove_child : Dom.node option -> Dom.node option -> unit = "removeChild" [@@bs.send]

  external parent_node : Dom.node -> Dom.node = "parentNode" [@@bs.get]
  external child_nodes : Dom.node option -> Dom.node option array = "childNodes" [@@bs.get]

  external remove_event_listener : Dom.node option -> string -> (Dom.event -> unit) -> unit = "removeEventListener" [@@bs.send]
  external add_event_listener : Dom.node option -> string -> (Dom.event -> unit) -> unit = "addEventListener" [@@bs.send]

  external listener : Dom.event -> unit = "listener" [@@bs.module "./js_utils"]
  external set_handler : Dom.node option -> string -> (Dom.event -> unit) option -> unit = "setHandler" [@@bs.module "./js_utils"]

  external vdom : Dom.node -> 'msg vnode option = "vdom" [@@bs.get]
  external set_vdom : Dom.node -> 'msg vnode -> unit = "vdom" [@@bs.set]


  let get_child_node node index =
    Js.Array.unsafe_get (child_nodes node) index


  let key_of_vnode = function
    | VText { key } -> key
    | VNode { key } -> key
    | VNodeNone     -> ""

  let keys_are_equal vn1 vn2 =
    if (key_of_vnode vn1) = (key_of_vnode vn2)
    then true
    else false


  let patch_prop event_handler node prev_prop next_prop =
    match prev_prop, next_prop with
    | PropNone, Attr (key, value) ->
      set_attribute node key value

    | PropNone, Handler (key, msg) ->
      let () = set_handler node key (Some (event_handler msg)) in
      add_event_listener node key listener

    | Attr (key, _), PropNone ->
      remove_attribute node key

    | Attr (prev_key, _), Handler (next_key, msg) ->
      let () = remove_attribute node prev_key in
      let () = set_handler node next_key (Some (event_handler msg)) in
      add_event_listener node next_key listener

    | Attr (prev_key, _), Attr (next_key, next_value) when prev_key = next_key ->
      set_attribute node next_key next_value

    | Attr (prev_key, _), Attr (next_key, next_value) ->
      let () = remove_attribute node prev_key in
      set_attribute node next_key next_value


    | Handler (key, _), PropNone ->
      let () = set_handler node key None in
      remove_event_listener node key listener

    | Handler (prev_key, _), Attr (next_key, next_value) ->
      let () = set_handler node prev_key None in
      let () = remove_event_listener node prev_key listener in
      set_attribute node next_key next_value

    | Handler (prev_key, prev_msg), Handler (next_key, next_msg) when prev_key = next_key ->
      set_handler node next_key (Some (event_handler next_msg))

    | Handler (prev_key, _), Handler (next_key, next_msg) ->
      let () = set_handler node prev_key None in
      let () = remove_event_listener node prev_key listener in
      let () = set_handler node next_key (Some (event_handler next_msg)) in
      add_event_listener node next_key listener

    | _, _ ->
      ()


  let rec patch_props event_handler node prev_props next_props =
    match prev_props, next_props with
    | [], [] ->
      ()

    | prev_prop :: prev_props, [] ->
      let () = patch_prop event_handler node prev_prop PropNone in
      patch_props event_handler node prev_props []

    | [], next_prop :: next_props ->
      let () = patch_prop event_handler node PropNone next_prop in
      patch_props event_handler node [] next_props

    | prev_prop :: prev_props, next_prop :: next_props ->
      let () = patch_prop event_handler node prev_prop next_prop in
      patch_props event_handler node prev_props next_props


  let cache_vnode dict =
    function
    | vnode when (key_of_vnode vnode) <> "" ->
      let () = Js.Dict.set dict (key_of_vnode vnode) vnode in
      dict

    | _ ->
      dict


  let cached_vnodes_of vnodes =
    List.fold_left cache_vnode (Js.Dict.empty ()) vnodes


  let rec patch_child_nodes event_handler parent index cached_vnodes prev_vnodes next_vnodes =
    match prev_vnodes, next_vnodes with
    | [], [] ->
      ()

    | prev_vnode :: prev_vnodes, [] ->
      let () = patch_node event_handler parent (Delete prev_vnode) in
      patch_child_nodes event_handler parent (index + 1) cached_vnodes prev_vnodes []

    | [], next_vnode :: next_vnodes when (key_of_vnode next_vnode) = "" ->
      let () = patch_node event_handler parent (Create (next_vnode, index)) in
      patch_child_nodes event_handler parent (index + 1) cached_vnodes [] next_vnodes

    | [], next_vnode :: next_vnodes ->
      let patch_action = match Js.Dict.get cached_vnodes (key_of_vnode next_vnode) with
        | Some prev_vnode -> (Move (prev_vnode, next_vnode, index))
        | None            -> (Create (next_vnode, index))
      in
      let () = patch_node event_handler parent patch_action in
      patch_child_nodes event_handler parent (index + 1) cached_vnodes [] next_vnodes

    | prev_vnode :: prev_vnodes, next_vnode :: next_vnodes when (keys_are_equal prev_vnode next_vnode) ->
      let () = patch_node event_handler parent (Update (prev_vnode, next_vnode)) in
      patch_child_nodes event_handler parent (index + 1) cached_vnodes prev_vnodes next_vnodes

    | prev_vnode :: prev_vnodes, next_vnode :: next_vnodes ->
      let () = patch_node event_handler parent (Delete prev_vnode) in
      let patch_action = match Js.Dict.get cached_vnodes (key_of_vnode next_vnode) with
        | Some prev_vnode -> (Move (prev_vnode, next_vnode, index))
        | None            -> (Create (next_vnode, index))
      in
      let () = patch_node event_handler parent patch_action in
      patch_child_nodes event_handler parent (index + 1) cached_vnodes prev_vnodes next_vnodes


  and patch_node event_handler parent = function
    | Create (vnode, pos)                  -> create_node event_handler parent pos vnode
    | Update (prev_vnode, next_vnode)      -> update_node event_handler prev_vnode next_vnode
    | Move   (prev_vnode, next_vnode, pos) -> move_node event_handler parent pos prev_vnode next_vnode
    | Delete vnode                         -> delete_node parent vnode


  and create_node event_handler parent pos = function
    | VText vtext ->
      let node = create_text_node dom vtext.str in
      let () = vtext.node <- node in
      let () = insert_before parent node (get_child_node parent pos) in
      ()

    | VNode vnode ->
      let node = create_element dom vnode.name in
      let () = vnode.node <- node in
      let () = insert_before parent node (get_child_node parent pos) in
      let () = patch_props event_handler node [] vnode.props in
      let () = patch_child_nodes event_handler node 0 (Js.Dict.empty ()) [] vnode.children in
      ()

    | VNodeNone ->
      ()


  and update_node event_handler prev_vnode next_vnode =
    match prev_vnode, next_vnode with
    | VText { node }, VText next_vtext ->
      let () = set_node_value node next_vtext.str in
      let () = next_vtext.node <- node in
      ()

    | VNode prev_vnode, VNode next_vnode ->
      let () = next_vnode.node <- prev_vnode.node in
      let () = patch_props event_handler next_vnode.node prev_vnode.props next_vnode.props in
      let () = patch_child_nodes event_handler next_vnode.node 0 (cached_vnodes_of prev_vnode.children) prev_vnode.children next_vnode.children in
      ()

    | _, _ ->
      ()


  and move_node event_handler parent pos prev_vnode next_vnode =
    let () = update_node event_handler prev_vnode next_vnode in
    match next_vnode with
    | VText vtext -> insert_before parent vtext.node (get_child_node parent pos)
    | VNode vnode -> insert_before parent vnode.node (get_child_node parent pos)
    | VNodeNone   -> ()


  and delete_node parent vnode =
    match vnode with
    | VText vtext -> remove_child parent vtext.node
    | VNode vnode -> remove_child parent vnode.node
    | VNodeNone   -> ()


  let render node view state event_handler =
    let next_vdom = view state in
    let patch_action =
      match (vdom node) with
      | None            -> (Create (next_vdom, 0))
      | Some prev_vdom -> (Update (prev_vdom, next_vdom))
    in
    let () = set_vdom node next_vdom in
    patch_node event_handler (Some node) patch_action

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
  let event_handler_fn msg _event = !dispatch_fn msg in
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
let text = View.text
let none = View.none
