(* The job server (GNU make manual 5.7.1, "Communicating Options to a
   Sub-make").

   -jN limits how many recipes run at once.  A recursive build has many
   makes, and if each of them took N for itself the tree would run N
   times N recipes; so the makes share one pool of tokens.  The make
   given -jN on the command line makes the pool with N-1 tokens in it
   and names it in MAKEFLAGS; every make in the tree, this one
   included, may run one recipe for free -- it is itself occupying a
   token of the make that started it -- and has to take a token from the
   pool before starting a second.  A token goes back when the recipe it
   let start has finished.

   The pool is a named pipe holding one byte per token, which is how GNU
   make 4.4 and later spell it (--jobserver-auth=fifo:PATH).  A pipe is
   the right shape for this: a byte can be taken by only one reader, so
   no locking is needed, and a make that dies without giving its token
   back costs the build one token rather than deadlocking it.  It is a
   named pipe rather than an inherited pair of descriptors so that each
   make opens it for itself and may read without blocking; a descriptor
   passed down is one open file between all of them, and making that one
   not block would change how every other make reads it. *)

type t = {
  fd : Unix.file_descr;
  path : string;
  owner : int option;             (* the make that made the pool unlinks it *)
}

let byte = Bytes.make 1 '+'

(* open the pipe for both reading and writing: a reader-only open would
   wait for a writer, and the pool has to be readable and writable here
   anyway *)
let open_fifo path =
  match Unix.openfile path [ Unix.O_RDWR; Unix.O_NONBLOCK ] 0o600 with
  | fd -> Some fd
  | exception _ -> None

(* n is the -j count: the pool gets n-1 tokens, since every make may run
   one recipe without taking one *)
let create n =
  let dir = match Sys.getenv_opt "TMPDIR" with Some d when d <> "" -> d | _ -> "/tmp" in
  let path = Printf.sprintf "%s/occmake-jobs.%d" dir (Unix.getpid ()) in
  (try Unix.unlink path with _ -> ());
  match Unix.mkfifo path 0o600 with
  | () ->
      (match open_fifo path with
       | None -> (try Unix.unlink path with _ -> ()); None
       | Some fd ->
           for _ = 2 to n do ignore (Unix.write fd byte 0 1) done;
           Some { fd; path; owner = Some (Unix.getpid ()) })
  | exception _ -> None

let connect path =
  match open_fifo path with
  | Some fd -> Some { fd; path; owner = None }
  | None -> None

let auth s = "fifo:" ^ s.path

(* Take a token, or say that there is none.  The pipe was opened without
   blocking, so an empty pool answers at once and the caller waits for a
   recipe of its own to finish instead. *)
let acquire s =
  match Unix.read s.fd (Bytes.create 1) 0 1 with
  | 1 -> true
  | _ -> false
  | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) -> false
  | exception _ -> false

let release s = try ignore (Unix.write s.fd byte 0 1) with _ -> ()

(* Give up this make's hold on the pool, and unlink it if this is the
   process that made it.  The test on the process matters: a recipe runs
   in a child of this make, and a child that left through exit would
   otherwise take the pool with it. *)
let destroy s =
  (try Unix.close s.fd with _ -> ());
  match s.owner with
  | Some pid when pid = Unix.getpid () -> (try Unix.unlink s.path with _ -> ())
  | _ -> ()

(* The pool named in an inherited MAKEFLAGS.  A make started by GNU make
   4.3 or earlier is offered a pair of descriptors instead
   (--jobserver-auth=R,W); that pool cannot be joined here, and the
   caller says so and runs one recipe at a time rather than taking N for
   itself on top of what the rest of the tree is doing. *)
type inherited = Fifo of string | Descriptors | None_

let parse_auth flags =
  let words = String.split_on_char ' ' (String.map (fun c -> if c = '\t' then ' ' else c) flags) in
  let prefix = "--jobserver-auth=" in
  let value = List.find_map (fun w ->
      let n = String.length prefix in
      if String.length w > n && String.sub w 0 n = prefix
      then Some (String.sub w n (String.length w - n)) else None) words in
  match value with
  | None -> None_
  | Some v ->
      if String.length v > 5 && String.sub v 0 5 = "fifo:"
      then Fifo (String.sub v 5 (String.length v - 5))
      else Descriptors
