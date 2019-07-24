
open Jest
open Expect
open JestDom
open Webapi.Dom

open App


let unwrap = function
  | Some el -> el
  | None    -> raise (Failure "Element not found")

let evt_handler _ _ = ()

let app_node () =
  let container = document |> Document.createElement "div" in
  let () = container |> Element.setAttribute "id" "app" in
  container


let () = test "render text" begin fun () ->
  let container = app_node () in

  let vdom =
    text "hello"
  in

  let _ = View.render vdom evt_handler container in

  container
  |> expect
  |> toHaveTextContent "hello"
end

let () = test "render element" begin fun () ->
  let container = app_node () in

  let vdom =
    vnode "div" [] []
  in

  let _ = View.render vdom evt_handler container in

  container
  |> expect
  |> toContainHTML {j|<div></div>|j}
end

let () = test "render element with prop" begin fun () ->
  let container = app_node () in

  let vdom =
    vnode "div" [ Attr("id", "my-id") ] []
  in

  let _ = View.render vdom evt_handler container in

  container
  |> expect
  |> toContainHTML {j|<div id="my-id"></div>|j}
end

let () = test "render nested element" begin fun () ->
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
end

let () = test "render nested multiple layers of elements" begin fun () ->
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
end

type event_test_msg =
  | MsgNone
  | Clicked of int

let () = test "render sets event callbacks" begin fun () ->
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
end

let () = test "render rearranges children position" begin fun () ->
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
end
