(** This module contains low-level conversions from utf-8 byte streams to code points and vice versa. *)

(** Byte input streams. *)
module type Byte_input =
sig
  (** The type of the byte stream. *)
  type t

  exception Invalid_encoding of t

  (** Returns the byte at the beginning of the stream and the rest
      of the stream or None if there are no more bytes to read. *)
  val get : t -> (char * t) option
end

(** Byte output streams. *)
module type Byte_output =
sig
  (** The type of the byte stream. *)
  type t

  (** Writes one byte to the output stream. *)
  val put : t -> char -> t
end

(** Code point input streams. *)
module type Code_point_input =
sig
  (** The type of the input stream. *)
  type t

  (** This exception will be thrown if the replacement of invalid characters is
      turned off and an invalid codepoint appears in the input stream. *)
  exception Invalid_encoding of t

  (** Returns the code point at the beginning of the stream and the
      rest of the stream or None if there are no more code points to read. *)
  val get : t -> (int * t) option
end

(** Code point output streams. *)
module type Code_point_output =
sig
  (** The type of the output stream. *)
  type t

  (** Writes one code point to the output stream. *)
  val put : t -> int -> t
end

(** Contains input annotated with line and column information. *)
type 'a with_position = {
    input : 'a ;
    line : int ;
    column : int ;
    position : int ;
  }

(** Initializes the position to zero. *)
val position_at_start_of_input : 'a -> 'a with_position

(** Codepoint input with line and column information.  *)
module With_positions :
  functor (I : Code_point_input) ->
    sig
      include Code_point_input with type t = I.t with_position

      (** Initializes the stream with position zero=beginning of input.  *)
      val start : I.t -> t

      val line : t -> int
      val column : t -> int
      val position : t -> int
    end

(** An input stream that used to be an output buffer stream. *)
module Buffer_input :
  sig
    include Code_point_input

    (** Get the current character with its position
      in the stream. *)
    val get_with_position : t -> (int with_position * t) option

    val start : t -> t

    val line : t -> int
    val column : t -> int

    val position : t -> int

    (* TODO: maybe add a function of type
      t -> t with_position? *)
  end

(** An in-memory output buffer that can be used as
  an input stream later. You can control the starting
  position information and also increase the position
  during putting a character. *)
module Buffer_output :
  sig
    include Code_point_output

    (** Begin with an empty stream that thinks it's at
      a certain position. *)
    val start_at_position : int -> int -> int -> t

    (** Put one character and increase the position
      by a certain amount. This is useful for escape
      sequences where the input is several code points
      that only represent one code point. *)
    val put_with_length : t -> int -> int -> t

    (** Convert this output buffer into an input buffer. *)
    val input_of : t -> Buffer_input.t
  end

(** Codepoint output with line and column information. *)
module Code_point_output_with_positions :
  functor (O : Code_point_output) ->
    sig
      include Code_point_output with type t = O.t with_position
    end

(** The maximum value of a codepoint.
    Valid codepoints are >= 0 and <= maximum_codepoint. *)
val maximum_codepoint : int

(** This symbol is used as a replacement for broken utf-8 characters
    if replacement is turned on. It should be 0xFFFD. *)
val replacement_character : int


(** Contains only one boolean. If [true], replace, if [false], throw exception. *)
module type Invalid_character_configuration =
sig
  (** If this is [true], invalid characters are replaced by [replacement_character].
      If it's [false], an [Invalid_encoding] exception is thrown.
      @see Decoded_input *)
  val replace_invalid_characters : bool
end

(** An exception should be thrown at invalid characters. *)
module Abort_on_invalid_characters : Invalid_character_configuration

(** Invalid characters should be replaced with the [replacement_character]. *)
module Replace_invalid_characters : Invalid_character_configuration

(** Views a byte input stream as a code point input stream. *)
module Decoded_input :
  functor (_ : Invalid_character_configuration) ->
    functor (Bytes : Byte_input) ->
      Code_point_input with type t = Bytes.t

(** Views a byte output stream as a code point output stream. *)
module Encoded_output :
  functor (_ : Invalid_character_configuration) ->
    functor (Bytes : Byte_output) ->
      Code_point_output with type t = Bytes.t

(** Transforms a code point input stream into a byte input stream. *)
module Encoded_input :
  functor (_ : Invalid_character_configuration) ->
    functor (Code_points : Code_point_input) ->
      sig
        include Byte_input

        (** Converts a code point input stream into a byte input stream. *)
        val of_code_points : Code_points.t -> t
      end

(** Transforms a byte input stream into a code point input stream. *)
module Decoded_output :
  functor (_ : Invalid_character_configuration) ->
    functor (Code_points : Code_point_output) ->
      sig
        include Byte_output

        (** Converts a code point output stream into a byte output stream.
            The resulting output stream caches some bytes. *)
        val of_code_points : Code_points.t -> t

        val flush : t -> Code_points.t
      end
 
(** Views strings as byte input streams. *)
module String_input :
sig
  include Byte_input with type t = int * string
  val of_string : string -> t
end

(** Views strings as code point input streams. *)
module Decoded_string_input :
sig
  include Code_point_input with type t = int * string
  val of_string : string -> t
end

module Encoded_string_output :
sig
  include Code_point_output
  val to_string : t -> string
  val empty : unit -> t
end

(** Views files as byte input streams. *)
module File_input :
sig
  type 'a option_or_end =
    | Something of 'a
    | Nothing
    | The_end
  type s = T of (char * s) option_or_end ref * in_channel
  include Byte_input with type t = s

  (** This function turns a mutable input channel into an "immutable" byte
    sequence. Of course it still depends on the input file but the values
    persist in memory and they won't be read twice. *)
  val of_channel : in_channel -> t

  (** This is just a convenience wrapper around [of_channel] and [Pervasives.open_in]. *)
  val open_file : string -> t

  (** This is just like [Pervasives.close_in] but on [t] instead of [in_channel]. *)
  val close_file : t -> unit
end

module With_put_str (O : Byte_output) :
sig
  include module type of O

  (** Write a whole string into the byte output stream. *)
  val put_str : t -> string -> t
end

(** Writes bytes into strings. *)
module String_output :
sig
  include Byte_output (*with type t = char Basics.Arr_seq.t*)

  (** Write a whole string into the byte output stream. *)
  val put_str : t -> string -> t

  val to_string : t -> string

  val empty : unit -> t
end

(** Views lists of ints as code point input streams. *)
module Code_point_list_input :
  Code_point_input with type t = int list

(** Views reverse lists of ints as code point output streams. *)
module Code_point_list_output :
  Code_point_output with type t = int list

(** Views reverse lists of bytes as byte output streams. *)
module Byte_list_output :
  Byte_output with type t = char list

(** Converts a code point into a list of bytes.
    The bytes are returned as integers to enable
    bitwise operations. *)
val bytes_of_code_point : int -> int list

(** Assumes exactly one utf-8 symbol in the argument string.
 Throws a Failure if there's more than one utf-8 symbol in the argument. *)
val code_point_of_bytes : string -> int

(** Converts a list of Unicode code points into a utf-8 string. *)
val string_of_code_points : int list -> string

module type Code_point_input_with_of_string =
sig
  include Code_point_input
  val of_string : string -> t
end

module type Code_point_input_with_position =
sig
  include Code_point_input

  (** Index of the current code point in the input stream. *)
  val position : t -> int
end

val times_two : int -> int
