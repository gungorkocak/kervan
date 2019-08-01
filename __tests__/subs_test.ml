open Jest
open Expect

open App

let dispatch _ = ()

let () =

describe "patch sets" begin fun () ->
  test "single sub" begin fun () ->
    let sub_set_count = ref 0 in
    let test_sub ~key () =
      let sub_effect = begin fun _dispatch ->
        let () = sub_set_count := !sub_set_count + 1 in
        (fun () -> ())
      end in
      Sub.start key sub_effect
    in

    let _ = Sub.patch [] [ (test_sub ~key:"test-sub" ()) ] [] dispatch in

    !sub_set_count
    |> expect
    |> toBe 1
  end;

  test "multiple subs" begin fun () ->
    let sub_set_count = ref 0 in
    let test_sub ~key () =
      let sub_effect = begin fun _dispatch ->
        let () = sub_set_count := !sub_set_count + 1 in
        (fun () -> ())
      end in
      Sub.start key sub_effect
    in

    let subs =
      [ (test_sub ~key:"test-sub-1" ())
      ; (test_sub ~key:"test-sub-2" ())
      ; (test_sub ~key:"test-sub-3" ())
      ; (test_sub ~key:"test-sub-4" ())
      ]
    in

    let _ = Sub.patch [] subs [] dispatch in

    !sub_set_count
    |> expect
    |> toBe 4
  end;

  test "a sub from none" begin fun () ->
    let sub_set_count = ref 0 in

    let test_sub ~key () =
      let sub_effect = begin fun _dispatch ->
        let () = sub_set_count := !sub_set_count + 1 in
        (fun () -> ())
      end in
      Sub.start key sub_effect
    in

    let old_subs = [ Sub.none ] in
    let subs = [ test_sub ~key:"test-sub" () ] in

    let _ = Sub.patch old_subs subs [] dispatch in

    !sub_set_count
    |> expect
    |> toBe 1
  end;
end;

describe "patch unsets" begin fun () ->
  test "single sub" begin fun () ->
    let sub_unset_count = ref 0 in

    let test_sub ~key () =
      let sub_effect = begin fun _dispatch ->
        (fun () -> sub_unset_count := !sub_unset_count + 1)
      end in
      Sub.start key sub_effect
    in

    let old_subs = [ test_sub ~key:"test-sub" () ] in
    let subs = [ Sub.none ] in

    let patched_subs = Sub.patch [] old_subs [] dispatch in
    let _ = Sub.patch patched_subs subs [] dispatch in

    !sub_unset_count
    |> expect
    |> toBe 1
  end;

  test "multiple subs" begin fun () ->
    let sub_unset_count = ref 0 in

    let test_sub ~key () =
      let sub_effect = begin fun _dispatch ->
        (fun () -> sub_unset_count := !sub_unset_count + 1)
      end in
      Sub.start key sub_effect
    in

    let old_subs =
      [ test_sub ~key:"test-sub-1" ()
      ; test_sub ~key:"test-sub-2" ()
      ]
    in
    let subs =
      [ Sub.none
      ; Sub.none
      ]
    in

    let patched_subs = Sub.patch [] old_subs [] dispatch in
    let _ = Sub.patch patched_subs subs [] dispatch in

    !sub_unset_count
    |> expect
    |> toBe 2
  end;
end;

describe "patch restarts on key change for" begin fun () ->
  test "single sub" begin fun () ->
    let sub_set_count = ref 0 in
    let sub_unset_count = ref 0 in

    let test_sub ~key () =
      let sub_effect = begin fun _dispatch ->
        let () = sub_set_count := !sub_set_count + 1 in
        (fun () -> sub_unset_count := !sub_unset_count + 1)
      end in
      Sub.start key sub_effect
    in

    let old_subs = [ test_sub ~key:"test-sub-1" () ] in
    let subs = [ test_sub ~key:"test-sub-1-new" () ] in

    let patched_subs = Sub.patch [] old_subs [] dispatch in
    let _ = Sub.patch patched_subs subs [] dispatch in

    [ !sub_set_count ; !sub_unset_count ]
    |> expect
    |> toEqual [ 2 ; 1 ]
  end;

  test "multiple subs" begin fun () ->
    let sub_set_count = ref 0 in
    let sub_unset_count = ref 0 in

    let test_sub ~key () =
      let sub_effect = begin fun _dispatch ->
        let () = sub_set_count := !sub_set_count + 1 in
        (fun () -> sub_unset_count := !sub_unset_count + 1)
      end in
      Sub.start key sub_effect
    in

    let old_subs =
      [ test_sub ~key:"test-sub-1" ()
      ; test_sub ~key:"test-sub-2" ()
      ; test_sub ~key:"test-sub-3" ()
      ]
    in
    let subs =
      [ test_sub ~key:"test-sub-1-new" ()
      ; test_sub ~key:"test-sub-2-new" ()
      ; test_sub ~key:"test-sub-3" ()
      ]
    in

    let patched_subs = Sub.patch [] old_subs [] dispatch in
    let _ = Sub.patch patched_subs subs [] dispatch in

    [ !sub_set_count ; !sub_unset_count ]
    |> expect
    |> toEqual [ 3 + 2 ; 2 ]
  end;
end;
