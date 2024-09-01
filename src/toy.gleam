import gleam/bit_array
import gleam/dict
import gleam/dynamic
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri

/// A decoder is a function that takes a `Dynamic` value and returns a tuple
/// containing the default value of the same type and a `Result` with the decoded
/// value or a list of errors
pub type Decoder(a) {
  Decoder(run: fn(dynamic.Dynamic) -> #(a, Result(a, List(ToyError))))
}

/// Contains decoding or validation errors
pub type ToyError {
  ToyError(error: ToyFieldError, path: List(String))
}

/// Each type of error that can be returned by the decoders
pub type ToyFieldError {
  InvalidType(expected: String, found: String)
  Missing(expected: String)
  ValidationFailed(check: String, expected: String, found: String)
  AllFailed(failures: List(List(ToyError)))
}

fn from_stdlib_errors(errors: List(dynamic.DecodeError)) -> List(ToyError) {
  list.map(errors, fn(err) {
    ToyError(error: InvalidType(err.expected, err.found), path: err.path)
  })
}

fn prepend_path(errors: List(ToyError), path: List(String)) -> List(ToyError) {
  list.map(errors, fn(err) {
    ToyError(..err, path: list.append(path, err.path))
  })
}

@external(erlang, "toy_ffi", "index")
@external(javascript, "./toy_ffi.mjs", "index")
fn index(
  data: dynamic.Dynamic,
  key: anything,
) -> Result(Option(dynamic.Dynamic), String)

/// Decode a field from a `Dynamic` value
///
/// This function will index into dictionary with any key type, tuples with
/// integer or javascript arrays and objects. The value found under the key
/// will be decoded with the provided decoder.
///
/// ```gleam
/// pub fn user_decoder() {
///   use name <- toy.field("name", toy.string)
///   toy.decoded(User(:name))
/// }
/// ```
pub fn field(
  key: c,
  decoder: Decoder(a),
  next: fn(a) -> Decoder(b),
) -> Decoder(b) {
  Decoder(fn(data) {
    case index(data, key) {
      Ok(Some(value)) -> {
        case decoder.run(value) {
          #(_next_default, Ok(value)) -> next(value).run(data)
          #(default, Error(errors)) -> {
            let #(next_default, result) = next(default).run(data)

            let errors = prepend_path(errors, [string.inspect(key)])

            let new_result = case result {
              Ok(_value) -> Error(errors)
              Error(next_errors) -> Error(list.append(next_errors, errors))
            }

            #(next_default, new_result)
          }
        }
      }
      Ok(None) -> {
        let #(default, _) = decoder.run(dynamic.from(Nil))

        let err =
          ToyError(
            error: Missing(dynamic.classify(dynamic.from(default))),
            path: [string.inspect(key)],
          )
        let #(next_default, result) = next(default).run(dynamic.from(data))
        let new_result = case result {
          Ok(_value) -> Error([err])
          Error(next_errors) -> Error([err, ..next_errors])
        }

        #(next_default, new_result)
      }
      Error(expected) -> {
        let #(default, _) = decoder.run(dynamic.from(Nil))

        let err =
          ToyError(
            error: InvalidType(expected, dynamic.classify(dynamic.from(data))),
            path: [string.inspect(key)],
          )
        let #(next_default, result) = next(default).run(dynamic.from(data))
        let new_result = case result {
          Ok(_value) -> Error([err])
          Error(next_errors) -> Error([err, ..next_errors])
        }

        #(next_default, new_result)
      }
    }
  })
}

/// Decode a field from a `Dynamic` value
///
/// This function will index into dictionary with any key type, tuples with
/// integer or javascript arrays and objects. The value found under the key
/// will be decoded with the provided decoder.
///
/// `None` is returned only if the field is missing. Otherwise the provided
/// decoder is used to decode the value.
///
/// ```gleam
/// pub fn reservation_decoder() {
///   use note <- toy.optional_field("note", toy.string)
///   toy.decoded(User(:name))
/// }
/// ```
pub fn optional_field(
  key: c,
  decoder: Decoder(a),
  next: fn(Option(a)) -> Decoder(b),
) -> Decoder(b) {
  Decoder(fn(data) {
    case index(data, key) {
      Ok(Some(value)) -> {
        case decoder.run(value) {
          #(_next_default, Ok(value)) -> next(Some(value)).run(data)
          #(default, Error(errors)) -> {
            let #(next_default, result) = next(Some(default)).run(data)

            let errors = prepend_path(errors, [string.inspect(key)])

            let new_result = case result {
              Ok(_value) -> Error(errors)
              Error(next_errors) -> Error(list.append(next_errors, errors))
            }

            #(next_default, new_result)
          }
        }
      }
      Ok(None) -> {
        let #(next_default, result) = next(None).run(data)
        let new_result = case result {
          Ok(value) -> Ok(value)
          Error(next_errors) -> Error(next_errors)
        }

        #(next_default, new_result)
      }
      Error(expected) -> {
        let #(default, _) = decoder.run(dynamic.from(Nil))

        let err =
          ToyError(
            error: InvalidType(
              expected,
              dynamic.classify(dynamic.from(default)),
            ),
            path: [string.inspect(key)],
          )
        let #(next_default, result) = next(None).run(dynamic.from(data))
        let new_result = case result {
          Ok(_value) -> Error([err])
          Error(next_errors) -> Error([err, ..next_errors])
        }

        #(next_default, new_result)
      }
    }
  })
}

fn deep_index(
  keys: List(c),
) -> fn(dynamic.Dynamic) -> #(List(c), Result(dynamic.Dynamic, ToyFieldError)) {
  let start = fn(x) { #([], Ok(x)) }
  list.fold_right(keys, start, fn(acc, key) {
    fn(data) {
      case index(data, key) {
        Ok(Some(val)) -> {
          let next = acc(val)
          #([key, ..next.0], next.1)
        }
        Ok(None) -> {
          #([key], Error(Missing("Dict")))
        }
        Error(expected) -> {
          #([key], Error(InvalidType(expected, dynamic.classify(data))))
        }
      }
    }
  })
}

/// Same as `field` but indexes recursively with the provided keys
/// ```gleam
/// pub fn user_decoder() {
///   use name <- toy.subfield(["person", "name"], toy.string)
///   toy.decoded(User(:name))
/// }
/// ```
pub fn subfield(
  keys: List(c),
  decoder: Decoder(a),
  next: fn(a) -> Decoder(b),
) -> Decoder(b) {
  let deep_index_fn = deep_index(keys)
  Decoder(fn(data) {
    let #(path, res) = deep_index_fn(data)
    case res {
      Ok(value) -> {
        case decoder.run(value) {
          #(_next_default, Ok(value)) -> next(value).run(data)
          #(default, Error(errors)) -> {
            let #(next_default, result) = next(default).run(data)

            let errors = prepend_path(errors, path |> list.map(string.inspect))

            let new_result = case result {
              Ok(_value) -> Error(errors)
              Error(next_errors) -> Error(list.append(next_errors, errors))
            }

            #(next_default, new_result)
          }
        }
      }
      Error(Missing(_) as err) -> {
        let #(default, _) = decoder.run(dynamic.from(Nil))

        let err = ToyError(error: err, path: path |> list.map(string.inspect))
        let #(next_default, result) = next(default).run(dynamic.from(data))
        let new_result = case result {
          Ok(_value) -> Error([err])
          Error(next_errors) -> Error([err, ..next_errors])
        }

        #(next_default, new_result)
      }
      Error(InvalidType(_, _) as err) -> {
        let #(default, _) = decoder.run(dynamic.from(Nil))

        let err = ToyError(error: err, path: path |> list.map(string.inspect))
        let #(next_default, result) = next(default).run(dynamic.from(data))
        let new_result = case result {
          Ok(_value) -> Error([err])
          Error(next_errors) -> Error([err, ..next_errors])
        }

        #(next_default, new_result)
      }
      Error(_) -> panic as "unreachable"
    }
  })
}

/// Same as `optional_field` but indexes recursively with the provided keys
/// ```gleam
/// pub fn user_decoder() {
///   use name <- toy.optional_subfield(["person", "name"], toy.string)
///   toy.decoded(User(:name))
/// }
/// ```
pub fn optional_subfield(
  keys: List(c),
  decoder: Decoder(a),
  next: fn(Option(a)) -> Decoder(b),
) -> Decoder(b) {
  let deep_index_fn = deep_index(keys)
  Decoder(fn(data) {
    let #(path, res) = deep_index_fn(data)
    case res {
      Ok(value) -> {
        case decoder.run(value) {
          #(_next_default, Ok(value)) -> next(Some(value)).run(data)
          #(default, Error(errors)) -> {
            let #(next_default, result) = next(Some(default)).run(data)

            let errors = prepend_path(errors, path |> list.map(string.inspect))

            let new_result = case result {
              Ok(_value) -> Error(errors)
              Error(next_errors) -> Error(list.append(next_errors, errors))
            }

            #(next_default, new_result)
          }
        }
      }
      Error(Missing(_)) -> {
        let #(next_default, result) = next(None).run(data)
        let new_result = case result {
          Ok(value) -> Ok(value)
          Error(next_errors) -> Error(next_errors)
        }

        #(next_default, new_result)
      }
      Error(InvalidType(_, _) as err) -> {
        let err = ToyError(error: err, path: path |> list.map(string.inspect))
        let #(next_default, result) = next(None).run(dynamic.from(data))
        let new_result = case result {
          Ok(_value) -> Error([err])
          Error(next_errors) -> Error([err, ..next_errors])
        }

        #(next_default, new_result)
      }
      Error(_) -> panic as "unreachable"
    }
  })
}

/// Creates a decoder which directly returns the provided value.
/// It is useful when decoding records.
///
/// ```gleam
/// pub fn user_decoder() {
///   use name <- toy.field("name', toy.string)
///   toy.decoded(User(:name))
/// }
/// ```
pub fn decoded(value: a) -> Decoder(a) {
  Decoder(fn(_) { #(value, Ok(value)) })
}

/// Decode a `String` value
pub const string = Decoder(decode_string)

fn decode_string(data) {
  #("", dynamic.string(data) |> result.map_error(from_stdlib_errors))
}

/// Decode a `Bool` value
pub const bool = Decoder(decode_bool)

fn decode_bool(data) {
  #(False, dynamic.bool(data) |> result.map_error(from_stdlib_errors))
}

/// Decodes a `String` and parses it as `Bool` with the given variants:
/// ```gleam
/// case value {
///   "True" | "true" -> Ok(True)
///   "False" | "false" -> Ok(False)
///   _ -> Error(Nil)
/// }
/// ```
/// In case pattern matching on the string fails,
/// returns `ValidationFailed` error
/// **Error type**: `bool_string`
pub const bool_string = Decoder(decode_bool_string)

fn decode_bool_string(data) {
  case decode_string(data).1 {
    Ok("True") | Ok("true") -> #(False, Ok(True))
    Ok("False") | Ok("false") -> #(False, Ok(False))
    Ok(val) -> #(
      False,
      Error([ToyError(ValidationFailed("bool_string", "BoolString", val), [])]),
    )
    Error(errors) -> #(False, Error(errors))
  }
}

/// Decode an `Int` value
pub const int = Decoder(decode_int)

fn decode_int(data) {
  #(0, dynamic.int(data) |> result.map_error(from_stdlib_errors))
}

/// Decode a `String` and parse it as `Int`
/// In case parsing failed, returns `ValidationFailed` error
/// **Error type**: `int_string`
pub const int_string = Decoder(decode_int_string)

fn decode_int_string(data) {
  case decode_string(data) {
    #(_, Ok(data)) ->
      case int.parse(data) {
        Ok(value) -> #(0, Ok(value))
        Error(Nil) -> #(
          0,
          Error([
            ToyError(ValidationFailed("int_string", "IntString", data), []),
          ]),
        )
      }
    #(_, Error(errors)) -> #(0, Error(errors))
  }
}

/// Decode a `Float` value
pub const float = Decoder(decode_float)

fn decode_float(data) {
  #(0.0, dynamic.float(data) |> result.map_error(from_stdlib_errors))
}

/// Decode a `String` and parse it as `Float`
/// In case parsing failed, returns `ValidationFailed` error
/// **Error type**: `float_string`
pub const float_string = Decoder(decode_float_string)

// Decode a `String` and parse it as `Float`
fn decode_float_string(data) {
  case decode_string(data) {
    #(_, Ok(data)) ->
      case float.parse(data) {
        Ok(value) -> #(0.0, Ok(value))
        Error(Nil) ->
          case int.parse(data) {
            Ok(value) -> #(0.0, Ok(int.to_float(value)))
            Error(_) -> #(
              0.0,
              Error([
                ToyError(
                  ValidationFailed("float_string", "FloatString", data),
                  [],
                ),
              ]),
            )
          }
      }
    #(_, Error(errors)) -> #(0.0, Error(errors))
  }
}

/// Decode a `BitArray`
pub const bit_array = Decoder(decode_bit_array)

fn decode_bit_array(data) {
  #(<<>>, dynamic.bit_array(data) |> result.map_error(from_stdlib_errors))
}

/// Always decodes the provided value as `Dynamic`.
/// Error is never returned from this decoder
pub const dynamic = Decoder(decode_dynamic)

fn decode_dynamic(data) {
  #(dynamic.from(Nil), Ok(data))
}

/// Decodes a *nullish* value.
/// In erlang it is one of these atoms: `undefined`, `null`, `nil`.
/// In javascript it is one of these values: `undefined`, `null`
pub const nullish = Decoder(decode_nullish)

fn decode_nullish(data) {
  case is_nullish(data) {
    True -> #(Nil, Ok(Nil))
    False -> #(
      Nil,
      Error([ToyError(InvalidType("Nil", dynamic.classify(data)), [])]),
    )
  }
}

/// Decodes a `BitArray` from a base16 encoded string
pub const base16_string = Decoder(decode_base16_string)

fn decode_base16_string(data) {
  case dynamic.string(data) {
    Ok(data) ->
      case bit_array.base16_decode(data) {
        Ok(value) -> #(<<>>, Ok(value))
        Error(Nil) -> #(
          <<>>,
          Error([ToyError(ValidationFailed("base16", "base16", data), [])]),
        )
      }
    Error(errors) -> #(<<>>, Error(errors |> from_stdlib_errors))
  }
}

/// Decodes a `BitArray` from a base64 encoded string
pub const base64_string = Decoder(decode_base64_string)

fn decode_base64_string(data) {
  case dynamic.string(data) {
    Ok(data) ->
      case bit_array.base64_decode(data) {
        Ok(value) -> #(<<>>, Ok(value))
        Error(Nil) -> #(
          <<>>,
          Error([ToyError(ValidationFailed("base64", "base64", data), [])]),
        )
      }
    Error(errors) -> #(<<>>, Error(errors |> from_stdlib_errors))
  }
}

/// Decodes a `BitArray` from a url safe base64 encoded string
/// using `-` instead of `+` and `_` instead of `/`
pub const base64_url_string = Decoder(decode_base64_url_string)

fn decode_base64_url_string(data) {
  case dynamic.string(data) {
    Ok(data) ->
      case bit_array.base64_url_decode(data) {
        Ok(value) -> #(<<>>, Ok(value))
        Error(Nil) -> #(
          <<>>,
          Error([
            ToyError(ValidationFailed("base64_url", "base64_url", data), []),
          ]),
        )
      }
    Error(errors) -> #(<<>>, Error(errors |> from_stdlib_errors))
  }
}

@external(javascript, "./toy_ffi.mjs", "parse_uri")
fn parse_uri(data: String) -> Result(uri.Uri, Nil) {
  uri.parse(data)
}

/// Decodes a `Uri` from a string
pub const uri = Decoder(decode_uri)

fn decode_uri(data) {
  let default_uri = uri.Uri(None, None, None, None, "", None, None)
  case dynamic.string(data) {
    Ok(data) ->
      case parse_uri(data) {
        Ok(value) -> #(default_uri, Ok(value))
        Error(Nil) -> #(
          default_uri,
          Error([ToyError(ValidationFailed("uri", "uri", data), [])]),
        )
      }
    Error(errors) -> #(default_uri, Error(errors |> from_stdlib_errors))
  }
}

fn do_try_map_with_index(
  list: List(a),
  index: Int,
  fun: fn(Int, a) -> Result(b, e),
  acc: List(b),
) -> Result(List(b), e) {
  case list {
    [] -> Ok(list.reverse(acc))
    [x, ..xs] ->
      case fun(index, x) {
        Ok(y) -> do_try_map_with_index(xs, index + 1, fun, [y, ..acc])
        Error(error) -> Error(error)
      }
  }
}

fn try_map_with_index(
  value: List(a),
  fun: fn(Int, a) -> Result(b, err),
) -> Result(List(b), err) {
  do_try_map_with_index(value, 0, fun, [])
}

/// Decode a list of values
///
/// ```gleam
/// pub fn fruits_decoder() {
///   toy.list({
///     use name <- toy.field("name", toy.string)
///     toy.decoded(Fruit(:name))
///   })
/// }
pub fn list(item: Decoder(a)) -> Decoder(List(a)) {
  Decoder(fn(data) {
    case dynamic.shallow_list(data) {
      Ok(value) -> {
        let result =
          try_map_with_index(value, fn(index, val) {
            case item.run(val) {
              #(_default, Ok(it)) -> Ok(it)
              #(_default, Error(errors)) ->
                Error(errors |> prepend_path([string.inspect(index)]))
            }
          })

        #([], result)
      }
      Error(errors) -> #([], Error(from_stdlib_errors(errors)))
    }
  })
}

/// Decodes a `Dict` using the provided key and value decoders
pub fn dict(
  key_type: Decoder(a),
  value_type: Decoder(b),
) -> Decoder(dict.Dict(a, b)) {
  Decoder(fn(value) {
    use <- fn(next) { #(dict.new(), next()) }
    use map <- result.try(decode_map(value))
    use pairs <- result.try(
      map
      |> dict.to_list
      |> list.try_map(fn(pair) {
        let #(k, v) = pair
        use k <- result.try(
          map_errors(key_type, prepend_path(_, ["keys"])).run(k).1,
        )
        use v <- result.try(
          map_errors(value_type, prepend_path(_, ["values"])).run(v).1,
        )
        Ok(#(k, v))
      }),
    )
    Ok(dict.from_list(pairs))
  })
}

@external(erlang, "toy_ffi", "decode_map")
@external(javascript, "./toy_ffi.mjs", "decode_map")
fn decode_map(
  a: dynamic.Dynamic,
) -> Result(dict.Dict(dynamic.Dynamic, dynamic.Dynamic), List(ToyError))

@external(erlang, "toy_ffi", "is_nullish")
@external(javascript, "./toy_ffi.mjs", "is_nullish")
fn is_nullish(data: a) -> Bool

/// Creates a new decoder from an existing one, which will return `None` if
/// the value is `null` or `undefined` on javascript, or `nil`, `null`,
/// `undefined` on erlang. Otherwise it will return the result of the provided
/// decoder wrapped in `Some`
pub fn nullable(of dec: Decoder(a)) -> Decoder(Option(a)) {
  Decoder(fn(data) {
    case is_nullish(data) {
      True -> #(None, Ok(None))
      False -> {
        case dec.run(data) {
          #(_default, Ok(value)) -> #(None, Ok(Some(value)))
          #(_default, Error(errors)) -> #(None, Error(errors))
        }
      }
    }
  })
}

@external(erlang, "toy_ffi", "decode_option")
@external(javascript, "./toy_ffi.mjs", "decode_option")
fn decode_option(value: dynamic.Dynamic) -> Result(Option(dynamic.Dynamic), Nil)

/// Decodes a gleam `Option` type. In erlang represented as `{ok, Value}` or `none`.
/// In javascript represented as an instance of `Some` or `None` classes.
pub fn option(of dec: Decoder(a)) -> Decoder(Option(a)) {
  Decoder(fn(data) {
    case decode_option(data) {
      Ok(Some(value)) ->
        case dec.run(value) {
          #(_default, Ok(value)) -> #(None, Ok(Some(value)))
          #(_default, Error(errors)) -> #(None, Error(errors))
        }
      Ok(None) -> #(None, Ok(None))
      Error(_) -> #(
        None,
        Error([
          ToyError(error: InvalidType("Option", string.inspect(data)), path: []),
        ]),
      )
    }
  })
}

/// Attempts to decode the value with each of the decoders in order. The first
/// successful one will be returned. If none of the decoders are successful,
/// an error is returned specifying possible options.
///
/// This function will panic if the list of decoders is empty.
///
/// ```gleam
/// let dog_decoder = fn() {
///   use tag <- toy.field("tag", toy.string)
///   toy.decoded(Dog(tag:))
/// }
///
/// let cat_decoder = fn() {
///   use collar <- toy.field("collar", toy.string)
///   toy.decoded(Cat(collar:))
/// }
///
/// let fish_decoder = fn() {
///   use color <- toy.field("color", toy.string)
///   toy.decoded(Fish(color:))
/// }
///
/// let decoder = toy.one_of([dog_decoder(), cat_decoder(), fish_decoder()])
///
/// dict.from_list([#("tag", dynamic.from("woof"))])
/// |> dynamic.from
/// |> toy.decode(decoder)
/// |> should.equal(Ok(Dog(tag: "woof")))
///
/// dict.from_list([#("feathers", dynamic.from("blue"))])
/// |> dynamic.from
/// |> toy.decode(decoder)
/// |> should.equal(
///   Error([
///     toy.ToyError(
///       toy.AllFailed([
///         [toy.ToyError(toy.Missing("String"), ["\"tag\""])],
///         [toy.ToyError(toy.Missing("String"), ["\"collar\""])],
///         [toy.ToyError(toy.Missing("String"), ["\"color\""])],
///       ]),
///       [],
///     ),
///   ]),
/// )
/// ```
pub fn one_of(decoders: List(Decoder(a))) -> Decoder(a) {
  Decoder(fn(data) { decode_one_of(data, None, decoders, []) })
}

fn decode_one_of(
  data: dynamic.Dynamic,
  default: Option(a),
  decoders: List(Decoder(a)),
  seen_errors: List(List(ToyError)),
) -> #(a, Result(a, List(ToyError))) {
  case decoders {
    [dec, ..rest] ->
      case dec.run(data) {
        #(default, Ok(value)) -> #(default, Ok(value))
        #(default, Error(errors)) ->
          decode_one_of(data, Some(default), rest, [errors, ..seen_errors])
      }
    [] -> {
      let assert Some(default) = default
      #(
        default,
        Error([
          ToyError(
            error: AllFailed(failures: seen_errors |> list.reverse),
            path: [],
          ),
        ]),
      )
    }
  }
}

/// Takes any decoder and a list of mappings from the decoded value to a new
/// value. Returns the new value corresponding to the decoded value.
///
/// In case of an error, return a `ValidationFailed` with the expected type
/// being a list of the possible values separated by a comma.
///
/// **Panics if the list of variants is empty**
///
/// **Error type**: `enum`
///
/// ```gleam
/// pub type Fish {
///   Salmon
///   Trout
///   Shard
///   Cod
/// }
///
/// let decoder =
///   toy.string
///   |> toy.enum([
///     #("salmon", Salmon),
///     #("trout", Trout),
///     #("shard", Shard),
///     #("cod", Cod),
///   ])
///
/// dynamic.from("salmon")
/// |> toy.decode(decoder)
/// |> should.equal(Ok(Salmon))
/// ```
pub fn enum(dec: Decoder(a), variants: List(#(a, b))) -> Decoder(b) {
  let assert Ok(#(_key, default)) = list.first(variants)

  Decoder(fn(data) {
    case dec.run(data) {
      #(_default, Ok(value)) -> {
        let mapped_value = list.key_find(variants, value)

        let possible_tags =
          list.map(variants, fn(tag) { string.inspect(tag.0) })
          |> string.join(", ")

        case mapped_value {
          Ok(value) -> #(value, Ok(value))
          Error(Nil) -> #(
            default,
            Error([
              ToyError(
                ValidationFailed("enum", possible_tags, string.inspect(value)),
                path: [],
              ),
            ]),
          )
        }
      }
      #(_default, Error(errors)) -> #(default, Error(errors))
    }
  })
}

// String validation

/// Validates that the string contains an email address.
/// This is done by checking if the string contains the "@" character.
///
/// **Error type**: `string_email`
pub fn string_email(dec: Decoder(String)) -> Decoder(String) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(value)) ->
        case string.contains(value, "@") {
          True -> #(default, Ok(value))
          False -> #(
            default,
            Error([
              ToyError(
                error: ValidationFailed(
                  check: "string_email",
                  expected: "@",
                  found: value,
                ),
                path: [],
              ),
            ]),
          )
        }
      with_decode_errors -> with_decode_errors
    }
  })
}

/// Validates that the string contains some characters that are not whitespace.
///
/// **Error type**: `string_nonempty`
pub fn string_nonempty(dec: Decoder(String)) -> Decoder(String) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) -> {
        let len = string.length(string.trim(data))
        case len > 0 {
          True -> #(default, Ok(data))
          False -> #(
            default,
            Error([
              ToyError(
                error: ValidationFailed(
                  check: "string_nonempty",
                  expected: "non_empty",
                  found: "[]",
                ),
                path: [],
              ),
            ]),
          )
        }
      }
      with_decode_error -> with_decode_error
    }
  })
}

/// Validates that the length of the string is at least the provided number
///
/// **Error type**: `string_min`
pub fn string_min(dec: Decoder(String), minimum: Int) -> Decoder(String) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) -> {
        let len = string.length(data)
        case len >= minimum {
          True -> #(default, Ok(data))
          False -> #(
            default,
            Error([
              ToyError(
                error: ValidationFailed(
                  check: "string_min",
                  expected: ">=" <> int.to_string(minimum),
                  found: int.to_string(len),
                ),
                path: [],
              ),
            ]),
          )
        }
      }
      with_decode_error -> with_decode_error
    }
  })
}

/// Validates that the length of the string is less than the provided number
///
/// **Error type**: `string_max`
pub fn string_max(dec: Decoder(String), maximum: Int) -> Decoder(String) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) -> {
        let len = string.length(data)
        case len < maximum {
          True -> #(default, Ok(data))
          False -> #(
            default,
            Error([
              ToyError(
                error: ValidationFailed(
                  check: "string_max",
                  expected: "<" <> int.to_string(maximum),
                  found: int.to_string(len),
                ),
                path: [],
              ),
            ]),
          )
        }
      }
      with_decode_error -> with_decode_error
    }
  })
}

/// Validates that the string is a valid uri
///
/// **Error type**: `string_uri`
pub fn string_uri(dec: Decoder(String)) -> Decoder(String) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) -> {
        case parse_uri(data) {
          Ok(_value) -> #(default, Ok(data))
          Error(Nil) -> #(
            default,
            Error([
              ToyError(
                error: ValidationFailed(
                  check: "string_uri",
                  expected: "uri",
                  found: data,
                ),
                path: [],
              ),
            ]),
          )
        }
      }
      with_decode_error -> with_decode_error
    }
  })
}

// Int validation

/// Validates that number is greater or equal to the provided minimum
///
/// **Error type**: `int_min`
pub fn int_min(dec: Decoder(Int), minimum: Int) -> Decoder(Int) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) ->
        case data >= minimum {
          True -> #(default, Ok(data))
          False -> #(
            default,
            Error([
              ToyError(
                error: ValidationFailed(
                  check: "int_min",
                  expected: ">=" <> int.to_string(minimum),
                  found: int.to_string(data),
                ),
                path: [],
              ),
            ]),
          )
        }
      with_decode_error -> with_decode_error
    }
  })
}

/// Validates that number is less than the provided maximum
///
/// **Error type**: `int_max`
pub fn int_max(dec: Decoder(Int), maximum: Int) -> Decoder(Int) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) ->
        case data < maximum {
          True -> #(default, Ok(data))
          False -> #(
            default,
            Error([
              ToyError(
                error: ValidationFailed(
                  check: "int_max",
                  expected: "<" <> int.to_string(maximum),
                  found: int.to_string(data),
                ),
                path: [],
              ),
            ]),
          )
        }
      with_decode_error -> with_decode_error
    }
  })
}

/// Validates that number is in the provided range: [minimum, maximum)
///
/// **Error type**: `int_range`
pub fn int_range(dec: Decoder(Int), minimum: Int, maximum: Int) -> Decoder(Int) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) ->
        case data >= minimum && data < maximum {
          True -> #(default, Ok(data))
          False -> #(
            default,
            Error([
              ToyError(
                error: ValidationFailed(
                  check: "int_range",
                  expected: int.to_string(minimum)
                    <> ".."
                    <> int.to_string(maximum),
                  found: int.to_string(data),
                ),
                path: [],
              ),
            ]),
          )
        }
      with_decode_error -> with_decode_error
    }
  })
}

// Float validation

/// Validates that number is greater or equal to the provided minimum
///
/// **Error type**: `float_min`
pub fn float_min(dec: Decoder(Float), minimum: Float) -> Decoder(Float) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) ->
        case data >=. minimum {
          True -> #(default, Ok(data))
          False -> #(
            default,
            Error([
              ToyError(
                error: ValidationFailed(
                  check: "float_min",
                  expected: ">=" <> float.to_string(minimum),
                  found: float.to_string(data),
                ),
                path: [],
              ),
            ]),
          )
        }
      with_decode_error -> with_decode_error
    }
  })
}

/// Validates that number is less than the provided maximum
///
/// **Error type**: `float_max`
pub fn float_max(dec: Decoder(Float), maximum: Float) -> Decoder(Float) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) ->
        case data <. maximum {
          True -> #(default, Ok(data))
          False -> #(
            default,
            Error([
              ToyError(
                error: ValidationFailed(
                  check: "float_max",
                  expected: "<" <> float.to_string(maximum),
                  found: float.to_string(data),
                ),
                path: [],
              ),
            ]),
          )
        }
      with_decode_error -> with_decode_error
    }
  })
}

/// Validates that the number is withing the provided range [minimum, maximum)
///
/// **Error type**: `float_range`
pub fn float_range(
  dec: Decoder(Float),
  minimum: Float,
  maximum: Float,
) -> Decoder(Float) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) ->
        case data >=. minimum && data <. maximum {
          True -> #(default, Ok(data))
          False -> #(
            default,
            Error([
              ToyError(
                error: ValidationFailed(
                  check: "float_range",
                  expected: float.to_string(minimum)
                    <> ".."
                    <> float.to_string(maximum),
                  found: float.to_string(data),
                ),
                path: [],
              ),
            ]),
          )
        }
      with_decode_error -> with_decode_error
    }
  })
}

// List validation

/// Validates that the list is not empty (contains at least one element)
///
/// **Error type**: `list_nonempty`
pub fn list_nonempty(dec: Decoder(List(a))) -> Decoder(List(a)) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) -> {
        case data {
          [_, ..] -> #(default, Ok(data))
          _ -> #(
            default,
            Error([
              ToyError(
                error: ValidationFailed(
                  check: "list_nonempty",
                  expected: "non_empty",
                  found: "[]",
                ),
                path: [],
              ),
            ]),
          )
        }
      }
      with_decode_error -> with_decode_error
    }
  })
}

/// Validates that the length of the list is at least the provided number
///
/// **Error type**: `list_min`
pub fn list_min(dec: Decoder(List(a)), minimum: Int) -> Decoder(List(a)) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) -> {
        let len = list.length(data)
        case len >= minimum {
          True -> #(default, Ok(data))
          False -> #(
            default,
            Error([
              ToyError(
                error: ValidationFailed(
                  check: "list_min",
                  expected: ">=" <> int.to_string(minimum),
                  found: int.to_string(len),
                ),
                path: [],
              ),
            ]),
          )
        }
      }
      with_decode_error -> with_decode_error
    }
  })
}

/// Validates that the length of the list is less than the provided maximum
///
/// **Error type**: `list_max`
pub fn list_max(dec: Decoder(List(a)), maximum: Int) -> Decoder(List(a)) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) -> {
        let len = list.length(data)
        case len < maximum {
          True -> #(default, Ok(data))
          False -> #(
            default,
            Error([
              ToyError(
                error: ValidationFailed(
                  check: "list_max",
                  expected: "<" <> int.to_string(maximum),
                  found: int.to_string(len),
                ),
                path: [],
              ),
            ]),
          )
        }
      }
      with_decode_error -> with_decode_error
    }
  })
}

// Generic validation

/// Map the result of the decoder to a new value
///
/// ```gleam
/// pub type Unit {
///   Centimeters(Int)
///   Milimeters(Int)
/// }
///
/// pub type User {
///   User(height: Unit)
/// }
///
/// pub fn user_decoder() {
///   use height <- toy.field("height", toy.int |> toy.map(Centimeters))
///   toy.decoded(User(:height))
/// }
pub fn map(dec: Decoder(a), fun: fn(a) -> b) -> Decoder(b) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(_default, Ok(data)) -> {
        let new_val = fun(data)
        #(new_val, Ok(new_val))
      }
      #(default, Error(errors)) -> #(fun(default), Error(errors))
    }
  })
}

/// Refine the result of the decoder with a validation function
///
/// ```gleam
/// pub fn user_decoder() {
///   use name <- toy.field("name", toy.string |> toy.refine(fn(name) {
///     case name {
///       "toy" -> Error([toy.ToyError(toy.ValidationFailed("name_taken", "new_name", name), [])])
///       _ -> Ok(Nil)
///     }
///   }))
///  toy.decoded(User(:name))
/// }
pub fn refine(
  dec: Decoder(a),
  fun: fn(a) -> Result(Nil, List(ToyError)),
) -> Decoder(a) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) -> {
        case fun(data) {
          Ok(Nil) -> #(default, Ok(data))
          Error(errors) -> #(default, Error(errors))
        }
      }
      with_decode_error -> with_decode_error
    }
  })
}

/// Validates that the decoded value is equal to the provided value
/// The comparison is made using the `==` operator
/// **Error type**: `is_literal`
pub fn is_equal(dec: Decoder(a), literal: a) -> Decoder(a) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) -> {
        case data == literal {
          True -> #(default, Ok(data))
          False -> #(
            default,
            Error([
              ToyError(
                ValidationFailed(
                  "is_equal",
                  string.inspect(literal),
                  string.inspect(data),
                ),
                [],
              ),
            ]),
          )
        }
      }
      with_decode_error -> with_decode_error
    }
  })
}

/// Map the result of the decoder to a new value or return an error
///
/// ```gleam
/// pub fn user_decoder() {
///   use name <- toy.field("name", toy.string |> toy.try_map("", fn(name) {
///     case name {
///       "toy" -> Error([toy.ToyError(toy.ValidationFailed("name_taken", "new_name", name), [])])
///       _ -> Ok(string.uppercase(name))
///     }
///   }))
///  toy.decoded(User(:name))
/// }
pub fn try_map(
  dec: Decoder(a),
  default: b,
  fun: fn(a) -> Result(b, List(ToyError)),
) -> Decoder(b) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(_default, Ok(data)) ->
        case fun(data) {
          Ok(new_value) -> #(default, Ok(new_value))
          Error(errors) -> #(default, Error(errors))
        }
      #(_default, Error(errors)) -> #(default, Error(errors))
    }
  })
}

/// If the passed in decoder returns an error, the provided function is called
/// to allow you to change or swap the errors
pub fn map_errors(dec: Decoder(a), fun: fn(List(ToyError)) -> List(ToyError)) {
  Decoder(fn(data) {
    case dec.run(data) {
      #(default, Ok(data)) -> #(default, Ok(data))
      #(default, Error(errors)) -> #(default, Error(fun(errors)))
    }
  })
}

/// Takes a `Dynamic` value and runs a `Decoder` on it, returning the result
/// of the decoding process
pub fn decode(
  data: dynamic.Dynamic,
  decoder: Decoder(a),
) -> Result(a, List(ToyError)) {
  decoder.run(data).1 |> result.map_error(list.reverse)
}

/// Returns a decoder that always fails with the provided error
pub fn fail(error: ToyFieldError, default: b) -> Decoder(b) {
  Decoder(fn(_data) { #(default, Error([ToyError(error:, path: [])])) })
}
