
(* handle event object *)
type event_listener = unit -> unit

external dom : Dom.document = "document" [@@bs.val]
external get_by_id : Dom.document -> string -> Dom.node = "getElementById" [@@bs.send]

external parent_node : Dom.node -> Dom.node = "parentNode" [@@bs.get]
external child_nodes : Dom.node -> Dom.nodeList = "childNodes" [@@bs.get]
external node_name : Dom.node -> string = "nodeName" [@@bs.get]
external remove_event_listener : Dom.node -> string -> event_listener -> unit = "removeEventListener" [@@bs.send]
external add_event_listener : Dom.node -> string -> event_listener -> unit = "addEventListener" [@@bs.send]
external handlers : Dom.node -> event_listener Js.Dict.t option = "" [@@bs.get]

external has_attribute : Dom.node -> string -> bool = "hasAttribute" [@@bs.send]
external get_attribute : Dom.node -> string -> string = "getAttribute" [@@bs.send]
external set_attribute : Dom.node -> string -> string -> unit = "setAttribute" [@@bs.send]
external remove_attribute : Dom.node -> string -> unit = "removeAttribute" [@@bs.send]

external create_text_node : Dom.document -> string -> Dom.node = "createTextNode" [@@bs.send]

external create_element_ns : Dom.document -> string -> string -> Dom.node = "createElementNS" [@@bs.send]

external append_child : Dom.node -> Dom.node -> unit = "appendChild" [@@bs.send]

external first_child : Dom.node -> Dom.node = "firstChild" [@@bs.get]

external get_node_value : Dom.node -> string option = "nodeValue" [@@bs.get]
external set_node_value : Dom.node -> string -> unit = "nodeValue" [@@bs.set]

external insert_before : Dom.node -> Dom.node -> Dom.node -> unit = "insertBefore" [@@bs.send]
external remove_child : Dom.node -> Dom.node -> unit = "removeChild" [@@bs.send]

let get_handlers node = match handlers node with
  | None -> Js.Dict.empty ()
  | Some handlers -> handlers

let event_1 () = Js.log "clicked!"

let () =
  let node = get_by_id dom "app" in
  let () = Js.log (parent_node node) in
  let () = Js.log (Js.String.toLowerCase (node_name node)) in
  let () = Js.log (child_nodes node) in
  let btn_1 = get_by_id dom "increment" in
  (* let () = add_event_listener btn_1 "click" event_1 in
   * let () = remove_event_listener btn_1 "click" event_1 in *)
  let btn_handlers = get_handlers btn_1 in
  let () = Js.Dict.set btn_handlers "click" event_1 in
  let () = match Js.Dict.get btn_handlers "click" with
    | None -> Js.log "No evt handler found"
    | Some handler -> Js.log2 "Here is evt handler" handler
  in

  let btn_2 = get_by_id dom "decrement" in
  let () =
    if has_attribute btn_2 "disabled"
    then Js.log2 "attr disabled found:::" (get_attribute btn_2 "disabled")
    else Js.log "attr disabled not found"
  in
  let () =
    if has_attribute btn_1 "disabled"
    then Js.log "disabled found for btn_1"
    else Js.log2 "setting disabled for btn_1" (set_attribute btn_1 "disabled" "")
  in

  let btn_3 = get_by_id dom "not-focus" in
  let () = Js.log2 "removing autofocus attr" (remove_attribute btn_3 "autofocus") in

  let text_node = create_text_node dom "this is text" in
  let () = Js.log2 "text node here" text_node in

  let svg_node = create_element_ns dom "http://www.w3.org/2000/svg" "svg" in
  let () = Js.log2 "here is svg element" svg_node in

  let () = Js.log2 "appending child to node" (append_child node svg_node) in

  let heading_1 = get_by_id dom "heading1" in
  let text_1 = first_child heading_1 in

  let () = Js.log2 "getting node value" (get_node_value text_1) in
  let () = Js.log2 "setting node value" (set_node_value text_1 "naber ahmet?") in

  let () = Js.log2 "inserting before" (insert_before heading_1 text_node text_1) in

  let () = Js.log2 "removing child" (remove_child heading_1 text_1) in

  ()
