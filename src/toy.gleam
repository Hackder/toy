import gleam/dynamic
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type Decoder(a) =
  fn(dynamic.Dynamic) -> #(a, Result(a, List(ToyError)))

pub type ToyError {
  ToyError(error: ToyFieldError, path: List(String))
}

pub type ToyFieldError {
  InvalidType(expected: String, found: String)
  Missing(expected: String)
  ValidationFailed(check: String, expected: String, found: String)
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

pub fn field(
  key: c,
  decoder: Decoder(a),
  next: fn(a) -> Decoder(b),
) -> Decoder(b) {
  fn(data) {
    case index(data, key) {
      Ok(Some(value)) -> {
        case decoder(value) {
          #(_next_default, Ok(value)) -> next(value)(data)
          #(default, Error(errors)) -> {
            let #(next_default, result) = next(default)(data)

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
        let #(default, _) = decoder(dynamic.from(Nil))

        let err =
          ToyError(
            error: Missing(dynamic.classify(dynamic.from(default))),
            path: [string.inspect(key)],
          )
        let #(next_default, result) = next(default)(dynamic.from(data))
        let new_result = case result {
          Ok(_value) -> Error([err])
          Error(next_errors) -> Error([err, ..next_errors])
        }

        #(next_default, new_result)
      }
      Error(expected) -> {
        let #(default, _) = decoder(dynamic.from(Nil))

        let err =
          ToyError(
            error: InvalidType(expected, dynamic.classify(dynamic.from(data))),
            path: [string.inspect(key)],
          )
        let #(next_default, result) = next(default)(dynamic.from(data))
        let new_result = case result {
          Ok(_value) -> Error([err])
          Error(next_errors) -> Error([err, ..next_errors])
        }

        #(next_default, new_result)
      }
    }
  }
}

pub fn optional_field(
  key: c,
  decoder: Decoder(a),
  next: fn(Option(a)) -> Decoder(b),
) -> Decoder(b) {
  fn(data) {
    case index(data, key) {
      Ok(Some(value)) -> {
        case decoder(value) {
          #(_next_default, Ok(value)) -> next(Some(value))(data)
          #(default, Error(errors)) -> {
            let #(next_default, result) = next(Some(default))(data)

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
        let #(default, _) = decoder(dynamic.from(Nil))

        let err =
          ToyError(
            error: Missing(dynamic.classify(dynamic.from(default))),
            path: [string.inspect(key)],
          )
        let #(next_default, result) = next(None)(data)
        let new_result = case result {
          Ok(value) -> Ok(value)
          Error(next_errors) -> Error([err, ..next_errors])
        }

        #(next_default, new_result)
      }
      Error(expected) -> {
        let #(default, _) = decoder(dynamic.from(Nil))

        let err =
          ToyError(
            error: InvalidType(
              expected,
              dynamic.classify(dynamic.from(default)),
            ),
            path: [string.inspect(key)],
          )
        let #(next_default, result) = next(None)(dynamic.from(data))
        let new_result = case result {
          Ok(_value) -> Error([err])
          Error(next_errors) -> Error([err, ..next_errors])
        }

        #(next_default, new_result)
      }
    }
  }
}

pub fn decoded(value: a) -> Decoder(a) {
  fn(_) { #(value, Ok(value)) }
}

pub fn string(data) {
  #("", dynamic.string(data) |> result.map_error(from_stdlib_errors))
}

pub fn int(data) {
  #(0, dynamic.int(data) |> result.map_error(from_stdlib_errors))
}

pub fn float(data) {
  #(0.0, dynamic.float(data) |> result.map_error(from_stdlib_errors))
}

pub fn bit_array(data) {
  #(<<>>, dynamic.bit_array(data) |> result.map_error(from_stdlib_errors))
}

pub fn dynamic(data) {
  #(dynamic.from(Nil), Ok(data))
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

pub fn list(item: Decoder(a)) -> Decoder(List(a)) {
  fn(data) {
    case dynamic.shallow_list(data) {
      Ok(value) -> {
        let result =
          try_map_with_index(value, fn(index, val) {
            case item(val) {
              #(_default, Ok(it)) -> Ok(it)
              #(_default, Error(errors)) ->
                Error(errors |> prepend_path([string.inspect(index)]))
            }
          })

        #([], result)
      }
      Error(errors) -> #([], Error(from_stdlib_errors(errors)))
    }
  }
}

@external(erlang, "toy_ffi", "is_nullish")
@external(javascript, "./toy_ffi.mjs", "is_nullish")
fn is_nullish(data: a) -> Bool

pub fn nullable(of dec: Decoder(a)) -> Decoder(Option(a)) {
  fn(data) {
    case is_nullish(data) {
      True -> #(None, Ok(None))
      False -> {
        case dec(data) {
          #(_default, Ok(value)) -> #(None, Ok(Some(value)))
          #(_default, Error(errors)) -> #(None, Error(errors))
        }
      }
    }
  }
}

@external(erlang, "toy_ffi", "decode_option")
@external(javascript, "./toy_ffi.mjs", "decode_option")
fn decode_option(value: dynamic.Dynamic) -> Result(Option(dynamic.Dynamic), Nil)

pub fn option(of dec: Decoder(a)) -> Decoder(Option(a)) {
  fn(data) {
    case decode_option(data) {
      Ok(Some(value)) ->
        case dec(value) {
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
  }
}

// String validation

pub fn string_email(dec: Decoder(String)) -> Decoder(String) {
  fn(data) {
    case dec(data) {
      #(default, Ok(value)) ->
        case string.contains(value, "@") {
          True -> #(default, Ok(value))
          False -> #(
            default,
            Error([
              ToyError(
                error: ValidationFailed(
                  check: "email",
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
  }
}

pub fn string_nonempty(dec: Decoder(String)) -> Decoder(String) {
  fn(data) {
    case dec(data) {
      #(default, Ok(data)) -> {
        let len = string.length(data)
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
  }
}

pub fn string_min(dec: Decoder(String), minimum: Int) -> Decoder(String) {
  fn(data) {
    case dec(data) {
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
  }
}

pub fn string_max(dec: Decoder(String), maximum: Int) -> Decoder(String) {
  fn(data) {
    case dec(data) {
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
  }
}

// Int validation

pub fn int_min(dec: Decoder(Int), minimum: Int) -> Decoder(Int) {
  fn(data) {
    case dec(data) {
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
  }
}

pub fn int_max(dec: Decoder(Int), maximum: Int) -> Decoder(Int) {
  fn(data) {
    case dec(data) {
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
  }
}

pub fn int_range(dec: Decoder(Int), minimum: Int, maximum: Int) -> Decoder(Int) {
  fn(data) {
    case dec(data) {
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
  }
}

// Float validation

pub fn float_min(dec: Decoder(Float), minimum: Float) -> Decoder(Float) {
  fn(data) {
    case dec(data) {
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
  }
}

pub fn float_max(dec: Decoder(Float), maximum: Float) -> Decoder(Float) {
  fn(data) {
    case dec(data) {
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
  }
}

pub fn float_range(
  dec: Decoder(Float),
  minimum: Float,
  maximum: Float,
) -> Decoder(Float) {
  fn(data) {
    case dec(data) {
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
  }
}

// List validation

pub fn list_nonempty(dec: Decoder(List(a))) -> Decoder(List(a)) {
  fn(data) {
    case dec(data) {
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
  }
}

pub fn list_min(dec: Decoder(List(a)), minimum: Int) -> Decoder(List(a)) {
  fn(data) {
    case dec(data) {
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
  }
}

pub fn list_max(dec: Decoder(List(a)), maximum: Int) -> Decoder(List(a)) {
  fn(data) {
    case dec(data) {
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
  }
}

// Generic validation

pub fn map(dec: Decoder(a), fun: fn(a) -> b) -> Decoder(b) {
  fn(data) {
    case dec(data) {
      #(_default, Ok(data)) -> {
        let new_val = fun(data)
        #(new_val, Ok(new_val))
      }
      #(default, Error(errors)) -> #(fun(default), Error(errors))
    }
  }
}

pub fn refine(
  dec: Decoder(a),
  fun: fn(a) -> Result(Nil, List(ToyError)),
) -> Decoder(a) {
  fn(data) {
    case dec(data) {
      #(default, Ok(data)) -> {
        case fun(data) {
          Ok(Nil) -> #(default, Ok(data))
          Error(errors) -> #(default, Error(errors))
        }
      }
      with_decode_error -> with_decode_error
    }
  }
}

pub fn try_map(
  dec: Decoder(a),
  default: b,
  fun: fn(a) -> Result(b, List(ToyError)),
) -> Decoder(b) {
  fn(data) {
    case dec(data) {
      #(_default, Ok(data)) ->
        case fun(data) {
          Ok(new_value) -> #(default, Ok(new_value))
          Error(errors) -> #(default, Error(errors))
        }
      #(_default, Error(errors)) -> #(default, Error(errors))
    }
  }
}

pub fn decode(
  data: dynamic.Dynamic,
  decoder: Decoder(a),
) -> Result(a, List(ToyError)) {
  decoder(data).1 |> result.map_error(list.reverse)
}

pub fn fail(error: ToyFieldError, default: b) -> Decoder(b) {
  fn(_data) { #(default, Error([ToyError(error:, path: [])])) }
}
