
open Jest
open Expect
open JestDom
open Webapi.Dom

open App

type event_test_msg =
  | MsgNone
  | Clicked of int

external child_node_at : Dom.nodeList -> int -> Dom.element = "" [@@bs.get_index]


let unwrap = function
  | Some el -> el
  | None    -> raise (Failure "Element not found")

let evt_handler _ _ = ()

let app_node () =
  let container = document |> Document.createElement "div" in
  let () = container |> Element.setAttribute "id" "app" in
  container

let get_app_node container =
  container
  |. Element.childNodes
  |. child_node_at 0


let app_child_at index container =
  container
  |. get_app_node
  |. Element.childNodes
  |. child_node_at index


let () =

describe "render creates" begin fun () ->
  test "text" begin fun () ->
    let container = app_node () in

    let vdom =
      text "hello"
    in

    let _ = View.render vdom evt_handler container in

    container
    |> expect
    |> toHaveTextContent "hello"
  end;

  test "element" begin fun () ->
    let container = app_node () in

    let vdom =
      vnode "div" [] []
    in

    let _ = View.render vdom evt_handler container in

    container
    |> expect
    |> toContainHTML {j|<div></div>|j}
  end;

  test "element with props" begin fun () ->
    let container = app_node () in

    let vdom =
      vnode "div"
        [ Attr("id", "my-id")
        ; Attr("disabled", "disabled")]
        []
    in

    let _ = View.render vdom evt_handler container in

    container
    |> expect
    |> toContainHTML {j|<div id="my-id" disabled="disabled"></div>|j}
  end;

  test "nested element" begin fun () ->
    let container = app_node () in

    let vdom =
      vnode "div"
        [ Attr("id", "parent") ]
        [
          vnode "div"
          [ Attr("id", "child") ]
          []
        ]
    in

    let _ = View.render vdom evt_handler container in

    container
    |> expect
    |> toContainHTML {j|<div id="parent"><div id="child"></div></div>|j}
  end;

  test "nested multiple layers of elements" begin fun () ->
    let container = app_node () in

    let vdom =
      vnode "div"
        [ Attr("id", "parent") ]
        [ vnode "div"
            [ Attr("id", "child1") ]
            []
        ; vnode "div"
            [ Attr("id", "child2") ]
            [ vnode "span"
                [ Attr("id", "grandchild1") ]
                []
            ; vnode "a"
                [ Attr("id", "grandchild2")
                ; Attr("href", "http://caravan.org")
                ]
                []
            ]
        ]
    in

    let _ = View.render vdom evt_handler container in

    container
    |> expect
    |> toContainHTML {j|<div id="parent"><div id="child1"></div><div id="child2"><span id="grandchild1"></span><a id="grandchild2" href="http://caravan.org"></a></div></div>|j}
  end;
end;

describe "render sets" begin fun () ->
  test "props" begin fun () ->
    let container = app_node () in
    let dispatched_msg = ref MsgNone in

    let vdom =
      vnode "div"
        [ Attr("id", "some-id")
        ; Attr("data-prop", "a-prop")
        ]
        []
    in

    let _ = View.render vdom evt_handler container in

    container
    |> Element.querySelector "#some-id"
    |> unwrap
    |> expect
    |> toHaveAttribute "data-prop" ~value:"a-prop"
  end;

  test "event callback" begin fun () ->
    let container = app_node () in
    let dispatched_msg = ref MsgNone in
    let custom_evt_handler msg _ = dispatched_msg := msg in

    let vdom =
      vnode "div"
        [ Attr("id", "click-me")
        ; Handler("click", (Clicked 1))
        ]
        []
    in

    let _ = View.render vdom custom_evt_handler container in

    let () =
      container
      |> Element.querySelector "#click-me"
      |> unwrap
      |> Element.unsafeAsHtmlElement
      |> HtmlElement.click
    in

    !dispatched_msg
    |> expect
    |> toEqual (Clicked 1)
  end;
end;

describe "render moves" begin fun () ->
  test "children into position" begin fun () ->
    let container = app_node () in

    let vdom1 =
      vnode "div"
        []
        [ vnode "div" [ Attr("id", "c1") ] []
        ; vnode "div" [ Attr("id", "c2") ] []
        ; text  "c3"
        ; vnode "div" [ Attr("id", "c4") ] []
        ]
    in
    let vdom2 =
      vnode "div"
        []
        [ vnode "div" [ Attr("id", "c1") ] []
        ; vnode "div" [ Attr("id", "c4") ] []
        ; text  "c3"
        ; vnode "div" [ Attr("id", "c2") ] []
        ]
    in

    let _ = View.render vdom1 evt_handler container in
    let _ = View.render vdom2 evt_handler container in

    container
    |> expect
    |> toContainHTML {j|<div><div id="c1"></div><div id="c4"></div>c3<div id="c2"></div></div>|j}
  end;

  test "single keyed element into position" begin fun () ->
    let container = app_node () in

    let vdom1 =
      vnode "div"
        []
        [ vnode ~key:"whoami" "div" [ Attr("id", "c1") ] []
        ; vnode "div" [ Attr("id", "c2") ] []
        ; text  "c3"
        ; vnode "div" [ Attr("id", "c4") ] []
        ]
    in
    let vdom2 =
      vnode "div"
        []
        [ vnode "div" [ Attr("id", "c2") ] []
        ; vnode ~key:"whoami" "div" [ Attr("id", "c1") ] []
        ; text  "c3"
        ; vnode "div" [ Attr("id", "c4") ] []
        ]
    in

    let _ = View.render vdom1 evt_handler container in
    let keyed_node = app_child_at 0 container in

    let _ = View.render vdom2 evt_handler container in
    let expected_node = app_child_at 1 container in

    keyed_node
    |> expect
    |> toBe expected_node
  end;

  test "multiple keyed elements into position" begin fun () ->
    let container = app_node () in

    let vdom1 =
      vnode "div"
        []
        [ vnode ~key:"whoami" "div" [ Attr("id", "c1") ] []
        ; vnode "div" [ Attr("id", "c2") ] []
        ; text  "c3"
        ; vnode ~key:"whoami-either" "div" [ Attr("id", "c4") ] []
        ]
    in
    let vdom2 =
      vnode "div"
        []
        [ vnode ~key:"whoami-either" "div" [ Attr("id", "c4") ] []
        ; vnode "div" [ Attr("id", "c2") ] []
        ; vnode ~key:"whoami" "div" [ Attr("id", "c1") ] []
        ; text  "c3"
        ]
    in

    let _ = View.render vdom1 evt_handler container in
    let keyed_node_1 = app_child_at 0 container in
    let keyed_node_2 = app_child_at 3 container in

    let _ = View.render vdom2 evt_handler container in
    let expected_node_1 = app_child_at 2 container in
    let expected_node_2 = app_child_at 0 container in

    [ keyed_node_1 ; keyed_node_2 ]
    |> expect
    |> toEqual [ expected_node_1 ; expected_node_2  ]
  end;
end;

describe "render removes" begin fun () ->
  test "top level node" begin fun () ->
    let container = app_node () in

    let vdom1 = vnode "div" [] [] in
    let vdom2 = View.none in

    let _ = View.render vdom1 evt_handler container in
    let node = get_app_node container in

    let _ = View.render vdom2 evt_handler container in


    container
    |> expect
    |> not_
    |> toContainElement node
  end;

  test "node none" begin fun () ->
    let container = app_node () in

    let vdom1 =
      vnode "div"
        []
        [ vnode "div" [] [] ]
    in
    let vdom2 =
      vnode "div"
        []
        [ View.none ]
    in

    let _ = View.render vdom1 evt_handler container in
    let node = app_child_at 0 container in

    let _ = View.render vdom2 evt_handler container in

    container
    |> get_app_node
    |> expect
    |> not_
    |> toContainElement node
  end;
end;
