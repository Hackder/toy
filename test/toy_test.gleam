import gleam/dict
import gleam/dynamic
import gleam/int
import gleam/string
import gleeunit
import gleeunit/should
import toy

pub fn main() {
  gleeunit.main()
}

pub fn string_test() {
  let data = dynamic.from("Hello, world!")
  toy.decode(data, toy.string) |> should.equal(Ok("Hello, world!"))
}

pub fn string_invalid_test() {
  let data = dynamic.from(42)
  toy.decode(data, toy.string)
  |> should.equal(Error([toy.ToyError(toy.InvalidType("String", "Int"), [])]))
}

pub fn string_refine_test() {
  let data = dynamic.from("thomas@gmail.com")
  toy.decode(
    data,
    toy.string
      |> toy.refine(fn(val) {
        case string.contains(val, "@") {
          True -> Ok(Nil)
          False ->
            Error([
              toy.ToyError(
                error: toy.Custom("invalid_email", dynamic.from(Nil)),
                path: [],
              ),
            ])
        }
      }),
  )
  |> should.equal(Ok("thomas@gmail.com"))
}

pub fn string_try_map_test() {
  let data = dynamic.from("123")
  toy.decode(
    data,
    toy.string
      |> toy.try_map(0, fn(value) {
        case int.parse(value) {
          Ok(val) -> Ok(val)
          Error(Nil) ->
            Error([
              toy.ToyError(
                error: toy.Custom("int_parse_failed", dynamic.from(Nil)),
                path: [],
              ),
            ])
        }
      }),
  )
}

pub fn int_map_test() {
  let data = dynamic.from(42)
  toy.decode(data, toy.int |> toy.map(fn(val) { val + 1 }))
  |> should.equal(Ok(43))
}

pub type Address {
  Address(street: String, city: String, zip: Int)
}

pub type Friend {
  Friend(name: String, age: Int, height: Float, address: Address)
}

pub type User {
  User(
    name: String,
    age: Int,
    height: Float,
    address: Address,
    friends: List(Friend),
  )
}

pub fn simple_record_test() {
  let simple_record_decoder = fn() {
    use <- toy.record()
    use street <- toy.field("street", toy.string)
    use city <- toy.field("city", toy.string)
    use zip <- toy.field("zip", toy.int)
    toy.decoded_record(Address(street:, city:, zip:))
  }

  let data =
    dict.from_list([
      #("street", dynamic.from("123 Main St")),
      #("city", dynamic.from("Springfield")),
      #("zip", dynamic.from(12_345)),
    ])
    |> dynamic.from

  toy.decode(data, simple_record_decoder())
  |> should.equal(Ok(Address("123 Main St", "Springfield", 12_345)))
}

pub type Sizing {
  Automatic
  Fixed(width: Float, height: Float)
}

pub fn simple_union_test() {
  let simple_union_decoder = fn() {
    use <- toy.record()
    use type_ <- toy.field("type", toy.string)

    case type_ {
      "automatic" -> {
        toy.decoded_record(Automatic)
      }
      "fixed" -> {
        use width <- toy.field("width", toy.float)
        use height <- toy.field("height", toy.float)
        toy.decoded_record(Fixed(width:, height:))
      }
      _ ->
        toy.fail_record(toy.NotOneOf(type_, ["automatic", "fixed"]), Automatic)
    }
  }

  let data =
    dict.from_list([
      #("type", dynamic.from("fixed")),
      #("width", dynamic.from(23.0)),
      #("height", dynamic.from(100.0)),
    ])
    |> dynamic.from

  toy.decode(data, simple_union_decoder())
  |> should.equal(Ok(Fixed(width: 23.0, height: 100.0)))
}

pub fn complex_validated_record_test() {
  let address_decoder = fn() {
    use city <- toy.field("city", toy.string)
    use street <- toy.field("street", toy.string)
    use zip <- toy.field(
      "zip",
      toy.int
        |> toy.refine(fn(zip) {
          case string.length(int.to_string(zip)) == 5 {
            True -> Ok(Nil)
            False ->
              Error([
                toy.ToyError(toy.Custom("zip_length", dynamic.from(Nil)), []),
              ])
          }
        }),
    )
    toy.decoded_record(Address(street:, city:, zip:))
  }

  let decoder = fn() {
    use <- toy.record()
    use name <- toy.field("name", toy.string)
    use age <- toy.field("age", toy.int |> toy.int_min(10))
    use height <- toy.field("height", toy.float |> toy.float_range(0.0, 300.0))
    use address <- toy.field("address", toy.record(address_decoder))
    use friends <- toy.field(
      "friends",
      toy.list(
        toy.record(fn() {
          use name <- toy.field("name", toy.string)
          use age <- toy.field("age", toy.int)
          use height <- toy.field("height", toy.float)
          use address <- toy.field("address", toy.record(address_decoder))
          toy.decoded_record(Friend(name:, age:, height:, address:))
        }),
      ),
    )
    toy.decoded_record(User(name:, age:, height:, address:, friends:))
  }

  let data =
    dict.from_list([
      #("name", dynamic.from("Thomas")),
      #("age", dynamic.from(42)),
      #("height", dynamic.from(1.8)),
      #(
        "address",
        dict.from_list([
          #("street", dynamic.from("123 Main St")),
          #("city", dynamic.from("Springfield")),
          #("zip", dynamic.from(12_345)),
        ])
          |> dynamic.from,
      ),
      #(
        "friends",
        [
          dict.from_list([
            #("name", dynamic.from("Alice")),
            #("age", dynamic.from(40)),
            #("height", dynamic.from(1.6)),
            #(
              "address",
              dict.from_list([
                #("street", dynamic.from("456 Elm St")),
                #("city", dynamic.from("Springfield")),
                #("zip", dynamic.from(12_345)),
              ])
                |> dynamic.from,
            ),
          ])
          |> dynamic.from,
        ]
          |> dynamic.from,
      ),
    ])
    |> dynamic.from

  toy.decode(data, decoder())
  |> should.equal(
    Ok(
      User(
        name: "Thomas",
        age: 42,
        height: 1.8,
        address: Address("123 Main St", "Springfield", 12_345),
        friends: [
          Friend(
            name: "Alice",
            age: 40,
            height: 1.6,
            address: Address("456 Elm St", "Springfield", 12_345),
          ),
        ],
      ),
    ),
  )
}
