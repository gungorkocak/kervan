
open Jest
open Expect
open JestDom
open Webapi.Dom
open Webapi.Dom.Document
open Webapi.Dom.Element

open App


let unwrap = function
  | Some el -> el
  | None    -> raise (Failure "Element not found")

let evt_handler _ _ = ()

let () = test "render text" begin fun () ->
  let container = document |> createElement "div" in

  let vdom =
    text "hello"
  in

  let _ = View.render vdom evt_handler container in

  container
  |> expect
  |> toHaveTextContent "hello"
end

let () = test "render element" begin fun () ->
  let container = document |> createElement "div" in
  let () = container |> setAttribute "id" "app" in

  let vdom =
    vnode "div" [] []
  in

  let _ = View.render vdom evt_handler container in

  container
  |> expect
  |> toContainHTML {j|<div></div>|j}
end

let () = test "render element with prop" begin fun () ->
  let container = document |> createElement "div" in
  let () = container |> setAttribute "id" "app" in

  let vdom =
    vnode "div" [ Attr("id", "my-id") ] []
  in

  let _ = View.render vdom evt_handler container in

  container
  |> expect
  |> toContainHTML {j|<div id="my-id"></div>|j}
end

let () = test "render nested element" begin fun () ->
  let container = document |> createElement "div" in
  let () = container |> setAttribute "id" "app" in

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
    let container = document |> createElement "div" in
    let () = container |> setAttribute "id" "app" in

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
