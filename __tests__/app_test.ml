open Jest
open Expect
open JestDom
open Webapi.Dom

open App

type counter_state = int
type counter_msg =
  | Increment

let unwrap = function
  | Some el -> el
  | None    -> raise (Failure "Element not found")

let app_node () =
  let container = document |> Document.createElement "div" in
  let () = container |> Element.setAttribute "id" "app" in
  container

let find_el selector container =
  container
  |> Element.querySelector selector
  |> unwrap


let click_el selector container =
  container
  |> find_el selector
  |> Element.unsafeAsHtmlElement
  |> HtmlElement.click


let () =

describe "app" begin fun () ->
  test "creates functional application" begin fun () ->
    let node = app_node () in

    let init () = 0 in

    let update state = function
      | Increment -> state + 1, Fx.none
    in

    let view state =
      vnode "div"
        []
        [ vnode "h1"
            [ Attr("id", "count") ]
            [ text ("Count: " ^ (string_of_int state)) ]
        ; vnode "button"
            [ Attr("id", "increment")
            ; Handler("click", Increment)
            ]
            [ text "+" ]
        ]
    in

    let subscriptions _ = [] in

    let () =
      app
        { init=init
        ; view=view
        ; update=update
        ; subscriptions=subscriptions
        ; node=node
        }
    in

    let () = click_el "#increment" node in
    let () = click_el "#increment" node in

    node
    |> find_el "#count"
    |> expect
    |> toHaveTextContent ("Count: " ^ "2")
  end;
end;
