module type Byte_input =
sig
  type t
  exception Invalid_encoding of t
  val get : t -> (char * t) option
end
module type Byte_output =
sig
  type t
  val put : t -> char -> t
end
module type Code_point_input =
sig
  type t
  exception Invalid_encoding of t
  val get : t -> (int * t) option
end
module type Code_point_output =
sig
  type t
  val put : t -> int -> t
end

type 'a with_position = {
    input : 'a ;
    line : int ;
    column : int ;
    position : int ;
  }

let position_at_start_of_input x =
  {
    input = x ;
    line = 0 ;
    column = 0 ;
    position = 0 ;
  }

module With_positions (I : Code_point_input) =
struct
  type t = I.t with_position
  exception Invalid_encoding of t

  let start i =
    {
      input = i ;
      line = 0 ;
      column = 0 ;
      position = 0 ;
    }
  let get stream =
    try
      (match I.get stream.input with
        | Some (c, input) ->
            if c = Char.code '\n' then
              Some (c, {
                  input = input ;
                  line = stream.line + 1 ;
                  column = 0 ;
                  position = stream.position + 1 ;
                })
            else
              Some (c, {
                  input = input ;
                  line = stream.line ;
                  column = stream.column + 1 ;
                  position = stream.position + 1 ;
                })
        | None -> None)
    with I.Invalid_encoding _ -> raise (Invalid_encoding stream)
  let line t = t.line
  let column t = t.column
  let position t = t.position
end

module Arr_seq = Simple_sequence.Array_sequence(struct
      let allow_copy_on_resize = true
      let allow_copy_on_multiple_cons = true
    end)
type output_buffer = int with_position Arr_seq.t with_position
type input_buffer = int with_position Arr_seq.t with_position
module Buffer_input :
  sig
    include Code_point_input with type t = input_buffer
    val get_with_position : t -> (int with_position * t) option
    val start : t -> t
    val line : t -> int
    val column : t -> int
    val position : t -> int
  end =
struct
  type t = input_buffer
  exception Invalid_encoding of t
  let get : t -> (int * t) option = fun arg ->
    match Arr_seq.get arg.input with
      | Some (c, stream) -> Some (c.input, { arg with input = stream })
      | None -> None
  let get_with_position arg =
    match Arr_seq.get arg.input with
      | Some (c, stream) -> Some (c, { arg with input = stream })
      | None -> None
  let start arg = arg
  let line arg = arg.line
  let column arg = arg.column
  let position arg = arg.position
end

module Buffer_output :
  sig
    include Code_point_output with type t = output_buffer
    (* line, column, position -> empty_sequence *)
    val start_at_position : int -> int -> int -> t
    val put_with_length : t -> int -> int -> t
    val input_of : t -> input_buffer
  end =
struct
  type t = output_buffer
  let start_at_position l c p =
    {
      input = Arr_seq.nil () ;
      line = l ;
      column = c ;
      position = p ;
    }
  let put stream o =
    if o = Char.code '\n' then
      {
        input = Arr_seq.cons (fun () -> ({ stream with
                    input = o ;
                  }, stream.input)) ;
        line = stream.line + 1 ;
        column = 0 ;
        position = stream.position + 1 ;
      }
    else
      {
        input = Arr_seq.cons (fun () -> ({ stream with
                    input = o ;
                  }, stream.input)) ;
        line = stream.line ;
        column = stream.column + 1 ;
        position = stream.position + 1 ;
      }

  (* This one is for escaped characters! *)
  let put_with_length stream o len =
    {
      input = Arr_seq.cons (fun () -> ({ stream with
                  input = o ;
                }, stream.input)) ;
      line = stream.line ;
      column = stream.column + len ;
      position = stream.position + len ;
    }
  let input_of stream = { stream with input = Arr_seq.reverse stream.input }
end

module Code_point_output_with_positions (O : Code_point_output) =
struct
  type t = O.t with_position
  (*let start o =
    {
      input = o ;
      line = 0 ;
      column = 0 ;
      position = 0 ;
    }*)
  let put stream o =
    if o = Char.code '\n' then
      {
        input = O.put stream.input o ;
        line = stream.line + 1 ;
        column = 0 ;
        position = stream.position + 1 ;
      }
    else
      {
        input = O.put stream.input o ;
        line = stream.line ;
        column = stream.column + 1 ;
        position = stream.position + 1 ;
      }
end

let maximum_codepoint = 0x10FFFF
let replacement_character = 0xFFFD

(*      bit # 01234567
              vvvvvvvv
 is_not_set 0b???????? j
    returns true if bit #j is 0
 *)
let is_not_set i j =
  let mask = 0x80 lsr j in
  not (i land mask = mask)

let rec last c cs =
  match cs with
    |  [] -> c
    | c :: cs -> last c cs

let byte_count bits =
  let rec count res =
    if res < 8  && not (is_not_set bits res) then
      count (res + 1)
    else res in
  count 0

module type Invalid_character_configuration =
sig
  val replace_invalid_characters : bool
end

module Replace_invalid_characters =
struct
  let replace_invalid_characters = true
end

module Abort_on_invalid_characters =
struct
  let replace_invalid_characters = false
end

module Decoded_input (R : Invalid_character_configuration)
        (Bytes : Byte_input) : Code_point_input with type t = Bytes.t =
struct
  type t = Bytes.t

  exception Invalid_encoding of t

  let (>>=) x f =
    match x with
      | Some (x, y) -> f (Char.code x, y)
      | None -> None
  let get input =
    Bytes.get input >>= fun (c0, input) ->
      if is_not_set c0 0 then
        Some (c0 land 0x7F, input)
      else
        let num_bytes = ref 0 in
        let bits = ref c0 in
        while not (is_not_set !bits 0) do
          incr num_bytes ;
          bits := !bits lsl 1 ;
        done ;
        if !num_bytes = 1 || !num_bytes > 6 then
          if R.replace_invalid_characters then
            Some (replacement_character, input)
          else
            raise (Invalid_encoding input)
        else
          let inp = ref input in
          let bytes = ref [] in
          for _ = 2 to !num_bytes do
            match Bytes.get !inp with
              | Some (c, inp') ->
                 (inp := inp' ;
                  bytes := Char.code c :: !bytes)
              | None -> ()
          done ;
          if List.length !bytes + 1 <> !num_bytes then
            if R.replace_invalid_characters then
              Some (replacement_character, !inp)
            else
              raise (Invalid_encoding !inp)
          else
            let result = ref 0 in
            let is_good = ref true in
            result := !result lsl (7 - !num_bytes) ;
            result := !result lor ((0xFF lsr (!num_bytes + 1)) land c0) ;
            List.iter (fun byte ->
              if !is_good && is_not_set byte 1 && not (is_not_set byte 0) then
                result := (!result lsl 6) lor (byte land 0x3F)
              else
                is_good := false) (List.rev !bytes) ;
            if not (!is_good) || !result < 0 || !result > maximum_codepoint then
              if R.replace_invalid_characters then
                Some (replacement_character, !inp)
              else
                raise (Invalid_encoding !inp)
            else
             Some (!result, !inp) ;;
end

module Decoded_output (R : Invalid_character_configuration)
    (Code_points : Code_point_output) :
  sig include Byte_output
    val of_code_points : Code_points.t -> t
    val flush : t -> Code_points.t
  end =
struct
  type t = char list * Code_points.t
  let of_code_points x = ([], x)
  module Tmp : Byte_input with type t = char list =
  struct
    type t = char list
    exception Invalid_encoding of t
    let get = function
      | c :: cs -> Some (c, cs)
      | [] -> None
  end
  module U = Decoded_input (R) (Tmp)
  let flush (cs, out) =
    let inp = ref (List.rev cs) in
    let dun = ref false in
    let outp = ref out in
    while not !dun do
      match U.get !inp with
        | Some (code, input) ->
           (inp := input ;
            outp := Code_points.put !outp code)
        | None -> dun := true
    done ;
    !outp
  let put (cs, out) c =
    (* If not a following byte *)
    if is_not_set (Char.code c) 0 || not (is_not_set (Char.code c) 1) then
      let inp = ref (List.rev cs) in
      let dun = ref false in
      let outp = ref out in
      while not !dun do
        match U.get !inp with
          | Some (code, input) ->
             (inp := input ;
              outp := Code_points.put !outp code)
          | None -> dun := true
      done ;
      if is_not_set (Char.code c) 0 then
        ([], Code_points.put !outp (Char.code c land 0x7F))
      else
        ([c], !outp)
    else
      let count = byte_count (Char.code (last c cs)) in
      if count = List.length (c :: cs) then
        ([], flush (c :: cs, out))
      else
        (c :: cs, out)
end

module Encoded_input (R : Invalid_character_configuration)
    (Code_points : Code_point_input) :
  sig include Byte_input
    val of_code_points : Code_points.t -> t
  end =
struct
  type t = char list * Code_points.t
  exception Invalid_encoding of t
  let of_code_points x = ([], x)
  let encode output =
    match Code_points.get output with
      | Some (i, output) ->
          if 0 <= i && i < 0x80 then
            Some ([Char.chr i], output)
          else
            let num_bytes = ref 0 in
            let remaining = ref i in
            let result = ref [] in
            let mask = ref 0x3F in
            if i < 0 || i > maximum_codepoint then
              begin
                if R.replace_invalid_characters then
                  remaining := replacement_character
                else
                  raise (Invalid_encoding ([], output))
              end ;
            while !remaining > !mask do
              incr num_bytes ;
              result := Char.chr (!remaining land 0x3F lor 0x80) :: !result ;
              remaining := !remaining lsr 6 ;
              mask := !mask lsr 1 ;
            done ;
            incr num_bytes ;
            result := Char.chr (
                        (!remaining land !mask) lor
                        (0xFF00 lsr !num_bytes land 0xFF)) :: !result ;
            Some (!result, output)
      | None -> None
  let rec get output =
    match output with
      | (x :: xs, output) ->
          Some (x, (xs, output))
      | ([], output) ->
          (match encode output with
            | Some output -> get output
            | None -> None)
end

module Encoded_output (R : Invalid_character_configuration)
    (Bytes : Byte_output) : Code_point_output with type t = Bytes.t =
struct
  type t = Bytes.t
  module Tmp : Code_point_input with type t = int option =
  struct
    type t = int option
    exception Invalid_encoding of t
    let get = function
      | Some c -> Some (c, None)
      | None -> None
  end
  module U = Encoded_input (R) (Tmp)
  let put out cp =
    let inp = ref (U.of_code_points (Some cp)) in
    let dun = ref false in (* dun is done but done is a keyword *)
    let outp = ref out in
    while not !dun do
      match U.get !inp with
        | Some (c, input) ->
           (inp := input ;
            outp := Bytes.put !outp c)
        | None -> dun := true
    done ;
    !outp
end

module String_input =
struct
  type t = int * string
  exception Invalid_encoding of t
  let get (pos, str) =
    if pos >= String.length str || pos < 0 then
      None
    else
      Some (String.get str pos, (pos + 1, str))
  let of_string s = (0, s)
end

module Decoded_string_input =
struct
  include Decoded_input (Replace_invalid_characters) (String_input)
  let of_string s = String_input.of_string s
end

let reverse_string s =
  let res = Bytes.make (String.length s) '\000' in
  let i = ref 0 in
  String.iter (fun _ ->
    Bytes.set res !i (String.get s (String.length s - 1 - !i)) ;
    incr i) s ; Bytes.to_string res

module String_output =
struct
  type t = char Arr_seq.t
  (* put = cons, so the output is reversed! to_string reverses the output
     again. *)
  let put out c = Arr_seq.cons (fun () -> (c, out))
  let put_str out s =
    let out = ref out in
    String.iter (fun c ->
      out := put !out c) s ;
    !out
  let to_string s = reverse_string (Arr_seq.to_string s)
  let empty () = Arr_seq.nil ()
end

module Encoded_string_output =
struct
  include Encoded_output (Replace_invalid_characters) (String_output)
  let to_string s = String_output.to_string s
  let empty () = String_output.empty ()
end

module File_input =
struct
  type 'a option_or_end =
    | Something of 'a
    | Nothing
    | The_end
  type s = T of (char * s) option_or_end ref * in_channel
  type t = s
  exception Invalid_encoding of t
  let get (T (current_byte, rest)) =
    match !current_byte with
      | Something res -> Some res
      | Nothing ->
        (try
          let result = (input_char rest, T (ref Nothing, rest)) in
          current_byte := Something result ;
          Some result
        with End_of_file -> (current_byte := The_end ; None))
      | The_end -> None
  let of_channel chan = T (ref Nothing, chan)
  let open_file name = T (ref Nothing, open_in name)
  let close_file (T (_, file)) = close_in file
end

module With_put_str (O : Byte_output) =
struct
  include O
  let put_str out s =
    let out = ref out in
    String.iter (fun c ->
      out := put !out c) s ;
    !out
end

module Code_point_output_with_put_str (O : Code_point_output) =
struct
  include O
  let put_str out s =
    let rec walk_str input output =
      match Decoded_string_input.get input with
        | Some (code_point, input) -> walk_str input (O.put output code_point)
        | None -> output in
    walk_str (Decoded_string_input.of_string s) out
end

module Code_point_list_input =
struct
  type t = int list
  exception Invalid_encoding of t
  let get = function
    | [] -> None
    | x :: xs -> Some (x, xs)
end

module Code_point_list_output =
struct
  type t = int list
  let put xs x = x :: xs
end

module Byte_list_output =
struct
  type t = char list
  let put cs c = c :: cs
end

module B2i = Decoded_output (Abort_on_invalid_characters) (Code_point_list_output)
module I2b = Encoded_output (Abort_on_invalid_characters) (Byte_list_output)

let bytes_of_code_point code_point =
  List.map Char.code (List.rev (I2b.put [] code_point))

let code_point_of_bytes bytes =
  let res = ref (B2i.of_code_points []) in
  String.iter (fun c ->
    res := B2i.put !res c) bytes ;
  match B2i.flush !res with
    | [i] -> i
    | _ :: _ -> failwith "code_point_of_bytes: more than one codepoint"
    | [] -> failwith "code_point_of_bytes: no utf-8 character found"

(*module I = Decoded_input (Replace_invalid_characters) (String_input)*)
module O =
  struct
    module O = Encoded_output (Replace_invalid_characters) (String_output)
    include O
    (* let put_with_length a b _ = O.put a b *)
  end

let string_of_code_points code_points =
  String_output.to_string (List.fold_left O.put
                            (String_output.empty ()) code_points)

let code_points_of_string text =
  let rec code_points_of_bytes_rec acc input =
    match Decoded_string_input.get input with
      | Some (code_point, input) -> code_points_of_bytes_rec (code_point :: acc) input
      | None -> List.rev acc in
  code_points_of_bytes_rec [] (Decoded_string_input.of_string text)

module type Code_point_input_with_of_string =
sig
  include Code_point_input
  val of_string : string -> t
end

module type Code_point_input_with_position =
sig
  include Code_point_input
  val position : t -> int
end

let times_two x = 2 * x ;;
