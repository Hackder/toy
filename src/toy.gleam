import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/result
import gleam/string

pub type Decoder(a) =
  fn(dynamic.Dynamic) -> #(a, Result(a, List(ToyError)))

pub type RecordDecoder(a) =
  fn(dict.Dict(dynamic.Dynamic, dynamic.Dynamic)) ->
    #(a, Result(a, List(ToyError)))

pub type ToyError {
  ToyError(error: ToyFieldError, path: List(String))
}

pub type ToyFieldError {
  InvalidType(expected: String, found: String)
  Missing
  IntTooSmall(value: Int, minimum: Int)
  IntTooLarge(value: Int, maximum: Int)
  IntOutsideRange(value: Int, minimum: Int, maximum: Int)
  FloatTooSmall(value: Float, minimum: Float)
  FloatTooLarge(value: Float, maximum: Float)
  FloatOutsideRange(value: Float, minimum: Float, maximum: Float)
  NotOneOf(value: String, all: List(String))
  Custom(tag: String, data: dynamic.Dynamic)
}

fn from_stdlib_errors(errors: List(dynamic.DecodeError)) -> List(ToyError) {
  list.map(errors, fn(err) {
    ToyError(error: InvalidType(err.expected, err.found), path: err.path)
  })
}

pub fn record(next: fn() -> RecordDecoder(b)) -> Decoder(b) {
  fn(data) {
    case dynamic.dict(dynamic.dynamic, dynamic.dynamic)(data) {
      Ok(dict_data) -> {
        next()(dict_data)
      }
      Error(errors) -> {
        let #(next_default, _result) = next()(dict.new())
        #(next_default, Error(from_stdlib_errors(errors)))
      }
    }
  }
}

fn prepend_path(errors: List(ToyError), path: List(String)) -> List(ToyError) {
  list.map(errors, fn(err) {
    ToyError(..err, path: list.append(path, err.path))
  })
}

pub fn field(
  key: c,
  decoder: Decoder(a),
  next: fn(a) -> RecordDecoder(b),
) -> RecordDecoder(b) {
  fn(data) {
    case dict.get(data, dynamic.from(key)) {
      Ok(value) -> {
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
      Error(Nil) -> {
        let #(default, _) = decoder(dynamic.from(Nil))

        let err = ToyError(error: Missing, path: [string.inspect(key)])
        let #(next_default, result) = next(default)(data)
        let new_result = case result {
          Ok(_value) -> Error([err])
          Error(next_errors) -> Error([err, ..next_errors])
        }

        #(next_default, new_result)
      }
    }
  }
}

pub fn decoded_record(value: a) -> RecordDecoder(a) {
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

pub fn list(item: Decoder(a)) -> Decoder(List(a)) {
  fn(data) {
    case dynamic.shallow_list(data) {
      Ok(value) -> {
        let result =
          list.try_map(value, fn(val) {
            case item(val) {
              #(_default, Ok(it)) -> Ok(it)
              #(_default, Error(errors)) -> Error(errors)
            }
          })

        #([], result)
      }
      Error(errors) -> #([], Error(from_stdlib_errors(errors)))
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
              ToyError(error: IntTooSmall(value: data, minimum:), path: []),
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
              ToyError(error: IntTooLarge(value: data, maximum:), path: []),
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
                error: IntOutsideRange(value: data, minimum:, maximum:),
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
              ToyError(error: FloatTooSmall(value: data, minimum:), path: []),
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
              ToyError(error: FloatTooLarge(value: data, maximum:), path: []),
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
                error: FloatOutsideRange(value: data, minimum:, maximum:),
                path: [],
              ),
            ]),
          )
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

pub fn fail_record(error: ToyFieldError, default: b) -> RecordDecoder(b) {
  fn(_data) { #(default, Error([ToyError(error:, path: [])])) }
}
