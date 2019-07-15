open App

type dom
type id

external dom : dom = "document" [@@bs.val]
external get_by_id : dom -> string -> id = "getElementById" [@@bs.send]

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
  ; SubTest.every ~key:(sub_key state) 1000 Increment
  ]


let update state = function
  | Increment -> state + 1, Fx.none
  | Decrement -> state - 1, Fx.one (EffectTest.log state)
  | Power -> state * state, Fx.none
  | Boost -> state + 1, Fx.one (EffectTest.delay 1000 Power)



let view state =
  hh "div"
    [ Attr("id", "hello") ]
    [ hh "div"
        [
          Attr("id", "naber")
        ]
        [ Text(string_of_int state) ]
    ; hh "button"
        [ Attr("id", "btn-inc")
        ; Handler("onclick", Boost)
        ]
        [ Text("++") ]
    ; hh "button"
        [ Attr("id", "btn-inc")
        ; Handler("onclick", Increment)
        ]
        [ Text("+") ]
    ; hh "button"
        [ Attr("id", "btn-dec")
        ; Handler("onclick", Decrement)
        ]
        [ Text("-") ]
    ]

let () =
  app
    { init=init
    ; view=view
    ; update=update
    ; subscriptions=subscriptions
    ; node=(get_by_id dom "app")
    }
