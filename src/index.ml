(* type state = int
 * type dom
 * type id (\** Abstract type for id object *\)
 * type tagname = string
 * type vdom =
 *   | VDom
 *   | Text of string
 * 
 * type action = (state -> state) option -> (state -> state) array
 * 
 * type props =
 *   {
 *     id : string
 *   ; onclick: action [@bs.optional]
 *   }
 *   [@@bs.deriving abstract]
 * 
 * type props_js = < id : string ; onclick : action > Js.t
 * 
 * type program =
 *   {
 *     view : (state -> vdom)
 *   ; init : int
 *   ; node : id
 *   } [@@bs.deriving jsConverter]
 * 
 * type program_to_js =  < init : int; node : id; view : state -> vdom > Js.t
 * 
 * external dom : dom = "document" [@@bs.val]
 * external get_by_id : dom -> string -> id = "getElementById" [@@bs.send]
 * external h : tagname -> props -> vdom array -> vdom = "h" [@@bs.module "hyperapp"]
 * external app : program_to_js -> unit = "app" [@@bs.module "hyperapp"]
 * 
 * let action (f, props) =
 *   match props with
 *   | Some props -> [ f ; props ]
 *   | None -> [ f ]
 * 
 * let increment state = state + 1
 * 
 * let decrement state = state - 1
 * 
 * let set num state = num
 * 
 * let reset state = 0
 * 
 * let hh tag props children =
 *   h "div" props (Array.of_list children)
 * 
 * let init = 0
 * 
 * let test =
 *   action (set, 42)
 * 
 * let view (state) =
 *   hh "div"
 *     (props ~id:"naber" ())
 *     [
 *       hh "div"
 *         (props ~id:"ahmet" ())
 *         [ Text(string_of_int state)]
 *     ; hh "button"
 *         (props ~id:"mehmet" ~onclick:(action increment) ())
 *         [ Text("+")]
 *     ; hh "button"
 *         (props ~id:"ayse" ~onclick:(action decrement) ())
 *         [ Text("-")]
 *     ; hh "button"
 *         (props ~id:"fatme" ~onclick:(action (set 42)) ())
 *         [ Text("Set to 42")]
 *     ; hh "button"
 *         (props ~id:"reegu" ~onclick:(action reset) ())
 *         [ Text("Reset")]
 *     ]
 * 
 * let main program =
 *   let props = programToJs program in
 *   app props
 * 
 * let () =
 *   main { init=init ; view=view ; node=(get_by_id dom "app") } *)
