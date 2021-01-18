module Index = struct
  module Key = struct
    type t = int

    let encode = string_of_int

    let encoded_size = 1

    let decode s x = int_of_string (String.sub s x 1)

    let hash = Hashtbl.hash

    let hash_size = 30

    let equal = Int.equal

    let pp ppf = Format.fprintf ppf "%d"
  end

  module Value = struct
    type t = int

    let encode = string_of_int

    let encoded_size = 1

    let decode s x = int_of_string (String.sub s x 1)

    let hash = Hashtbl.hash

    let hash_size = 30

    let equal = Int.equal

    let pp ppf = Format.fprintf ppf "%d"
  end

  include Index_unix.Make (Key) (Value)
end

let prnt_mem msg index value =
  Format.printf "%s -> %d: %b@." msg value (Index.mem index value)

let sanitize path =
  let rw = Index.v ~readonly:false ~log_size:42 path in
  Index.clear rw;
  Index.flush rw;
  Index.close rw

let test_b =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);
  let path = "./index" in
  sanitize path;
  let rw = Index.v ~readonly:false ~log_size:42 path in
  prnt_mem "A " rw 1;
  Index.replace rw 1 1;
  (* To make it work *)
  (* Index.flush rw ; *)
  prnt_mem "A'" rw 1;
  let ro = Index.v ~readonly:true ~log_size:42 path in
  prnt_mem "B " ro 1;
  Index.close ro;
  Index.flush rw;
  Index.close rw;
  let ro = Index.v ~readonly:true ~log_size:42 path in
  prnt_mem "B'" ro 1;
  Index.close ro;
  (* let rw = Index.v ~readonly:false ~log_size:42 path in
   * Index.flush rw ;
   * Index.close rw ; *)
  let ro = Index.v ~readonly:true ~log_size:42 path in
  prnt_mem "D " ro 1;
  Index.close ro

let () = test_b

(* index.1.1.2
 * A  -> 1: false
 * A' -> 1: true
 * B  -> 1: false
 * B' -> 1: false
 * D  -> 1: false *)
