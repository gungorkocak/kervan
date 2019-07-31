open App

external dom : Dom.document = "document" [@@bs.val]
external get_by_id : Dom.document -> string -> Dom.element = "getElementById" [@@bs.send]

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
        let id = Js.Global.setInterval (fun () -> dispatch msg) ms in
        (fun () -> Js.Global.clearInterval id)
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
  (* ; SubTest.every ~key:(sub_key state) 1000 Increment *)
  ]


let update state = function
  | Increment -> state + 1, Fx.none
  | Decrement -> state - 1, Fx.one (EffectTest.log state)
  | Power -> state * state, Fx.none
  | Boost -> state + 1, Fx.one (EffectTest.delay 1000 Power)


let view state =
  vnode "div"
    [ Attr("id", "hello") ]
    [ vnode "div"
        [
          Attr("id", "naber")
        ]
        [ text (string_of_int state) ]
    ; vnode "button"
        [ Attr("id", "btn-inc")
        ; Handler("click", Boost)
        ]
        [ text "++" ]
    ; vnode "button"
        [ Attr("id", "btn-inc")
        ; Handler("click", Increment)
        ]
        [ text "+" ]
    ; vnode "button"
        [ Attr("id", "btn-dec")
        ; Handler("click", Decrement)
        ]
        [ text "-" ]
    ]

let () =
  app
    { init=init
    ; view=view
    ; update=update
    ; subscriptions=subscriptions
    ; node=(get_by_id dom "app")
    }
