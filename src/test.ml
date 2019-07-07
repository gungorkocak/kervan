(* type state = int
 * 
 * type 'a msg = (state -> 'a option -> state) * 'a option
 * 
 * let increment (state: state) _ = state + 1
 * 
 * let decrement state _ = state - 1
 * 
 * type num_record =
 *   {
 *     num: int
 *   }
 * 
 * let set (state : state) (num : num_record option) = match num with
 *   | Some num -> num.num
 *   | None -> state
 * 
 * let x =
 *   (increment, None)
 * 
 * let y =
 *   (set, 42)
 * 
 * let update (f, props) state =
 *   f state props
 * 
 * let () =
 *   let state = update (set, Some { num=42 }) 10 in
 *   let next_state = update (increment, None) state in
 *   Js.log next_state
 * 
 * 
 * (\* below are hcaml *\)
 * 
 * type dom
 * type id
 * type action
 * type tagname = string
 * 
 * (\* type props =
 *  *   {
 *  *     id: string
 *  *   ; onclick: action [@bs.optional]
 *  *   }
 *  *   [@@bs.deriving abstract] *\)
 * 
 * type vdom =
 *   | VDom
 *   | Text of string
 * 
 * type program =
 *   {
 *     view : (state -> vdom)
 *   ; init : int
 *   ; node : id
 *   } [@@bs.deriving jsConverter]
 * 
 * type program_to_js = < init : int; node : id; view : state -> vdom > Js.t
 * 
 * type 'a attr_val =
 *   | String of string
 *   | Action of 'a msg
 * 
 * external dom : dom = "document" [@@bs.val]
 * external get_by_id : dom -> string -> id = "getElementById" [@@bs.send]
 * external h : tagname -> 'a attr_val Js.Dict.t -> vdom array -> vdom = "h" [@@bs.module "hyperapp"]
 * external app : program_to_js -> unit = "app" [@@bs.module "app"]
 * 
 * let hh tag props children =
 *     h tag (Js.Dict.fromList props) (Array.of_list children)
 * 
 * let init = 0
 * 
 * let view (state) =
 *   hh "div"
 *     [
 *       "id", "naber"
 *     ]
 *     [
 *       hh "div"
 *         [
 *           "id", "ahmet"
 *         ]
 *         [ Text(string_of_int state)]
 *     ; hh "button"
 *         [
 *           "id", "mehmet"
 *         ; "onclick", (increment, None)
 *         ]
 *         [ Text("+")]
 *     ; hh "button"
 *         [
 *           "id", "ayse"
 *         ; "onclick", (decrement, None)
 *         ]
 *         [ Text("-")]
 *     ; hh "button"
 *         [
 *           "id", "fatme"
 *         ; "onclick", (set, { num=42 })
 *         ]
 *         [ Text("Set to 42") ]
 *     ]
 * 
 * 
 * let main program =
 *   let props = programToJs program in
 *   app props
 * 
 * let () =
 *   main { init=init ; view=view ; node=(get_by_id dom "app") } *)
