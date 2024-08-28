import gleam/dict
import gleam/dynamic
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
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
  let data = dynamic.from("Thomas")
  toy.decode(
    data,
    toy.string
      |> toy.refine(fn(val) {
        use first <- result.try(
          string.first(val)
          |> result.replace_error([
            toy.ToyError(
              error: toy.ValidationFailed(
                check: "capital_letter",
                expected: "String",
                found: "nothing",
              ),
              path: [],
            ),
          ]),
        )

        case string.uppercase(first) == first {
          True -> Ok(Nil)
          False ->
            Error([
              toy.ToyError(
                error: toy.ValidationFailed(
                  check: "capital_letter",
                  expected: string.uppercase(first),
                  found: first,
                ),
                path: [],
              ),
            ])
        }
      }),
  )
  |> should.equal(Ok("Thomas"))
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
                error: toy.ValidationFailed(
                  check: "int_parse",
                  expected: "integer",
                  found: value,
                ),
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

pub type Image {
  Image(url: String, alt: String)
}

pub type User {
  User(
    name: String,
    email: String,
    age: Int,
    height: Float,
    address: Address,
    friends: List(Friend),
    profile_picture: Option(Image),
    title_picture: Option(Image),
  )
}

pub fn simple_record_test() {
  let simple_record_decoder = fn() {
    use street <- toy.field("street", toy.string)
    use city <- toy.field("city", toy.string)
    use zip <- toy.field("zip", toy.int)
    toy.decoded(Address(street:, city:, zip:))
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

pub fn invalid_simple_record_test() {
  let simple_record_decoder = fn() {
    use street <- toy.field("street", toy.string)
    use city <- toy.field("city", toy.string)
    use zip <- toy.field("zip", toy.int)
    toy.decoded(Address(street:, city:, zip:))
  }

  let data =
    Nil
    |> dynamic.from

  toy.decode(data, simple_record_decoder())
  |> should.equal(
    // TODO: Improve these error messages
    Error([
      toy.ToyError(toy.InvalidType("Dict", "Nil"), ["\"zip\""]),
      toy.ToyError(toy.InvalidType("Dict", "Nil"), ["\"city\""]),
      toy.ToyError(toy.InvalidType("Dict", "Nil"), ["\"street\""]),
    ]),
  )
}

pub fn empty_simple_record_test() {
  let simple_record_decoder = fn() {
    use street <- toy.field("street", toy.string)
    use city <- toy.field("city", toy.string)
    use zip <- toy.field("zip", toy.int)
    toy.decoded(Address(street:, city:, zip:))
  }

  let data =
    dict.new()
    |> dynamic.from

  toy.decode(data, simple_record_decoder())
  |> should.equal(
    Error([
      toy.ToyError(toy.Missing("Int"), ["\"zip\""]),
      toy.ToyError(toy.Missing("String"), ["\"city\""]),
      toy.ToyError(toy.Missing("String"), ["\"street\""]),
    ]),
  )
}

pub type Sizing {
  Automatic
  Fixed(width: Float, height: Float)
}

pub fn simple_union_test() {
  let simple_union_decoder = fn() {
    use type_ <- toy.field("type", toy.string)

    case type_ {
      "automatic" -> {
        toy.decoded(Automatic)
      }
      "fixed" -> {
        use width <- toy.field("width", toy.float)
        use height <- toy.field("height", toy.float)
        toy.decoded(Fixed(width:, height:))
      }
      discriminant ->
        toy.fail(
          toy.InvalidType(expected: "Sizing", found: "type:" <> discriminant),
          Automatic,
        )
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
          let len = string.length(int.to_string(zip))
          case len == 5 {
            True -> Ok(Nil)
            False ->
              Error([
                toy.ToyError(
                  toy.ValidationFailed(
                    check: "zip_length",
                    expected: "5",
                    found: int.to_string(len),
                  ),
                  [],
                ),
              ])
          }
        }),
    )
    toy.decoded(Address(street:, city:, zip:))
  }

  let image_decoder = fn() {
    use url <- toy.field("url", toy.string)
    use alt <- toy.field("alt", toy.string)
    toy.decoded(Image(url:, alt:))
  }

  let decoder = fn() {
    use name <- toy.field("name", toy.string)
    use email <- toy.field("email", toy.string |> toy.string_email)
    use age <- toy.field("age", toy.int |> toy.int_min(10))
    use height <- toy.field("height", toy.float |> toy.float_range(0.0, 300.0))
    use address <- toy.field("address", address_decoder())
    use friends <- toy.field(
      "friends",
      toy.list({
        use name <- toy.field("name", toy.string)
        use age <- toy.field("age", toy.int)
        use height <- toy.field("height", toy.float)
        use address <- toy.field("address", address_decoder())
        toy.decoded(Friend(name:, age:, height:, address:))
      }),
    )
    use profile_picture <- toy.optional_field(
      "profile_picture",
      image_decoder(),
    )
    use title_picture <- toy.optional_field("title_picture", image_decoder())

    toy.decoded(User(
      name:,
      email:,
      age:,
      height:,
      address:,
      friends:,
      profile_picture:,
      title_picture:,
    ))
  }

  let data =
    dict.from_list([
      #("name", dynamic.from("Thomas")),
      #("email", dynamic.from("thomas@example.com")),
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
      #(
        "profile_picture",
        dict.from_list([
          #("url", "https://example.com/picture.jpg"),
          #("alt", "The best picture"),
        ])
          |> dynamic.from,
      ),
    ])
    |> dynamic.from

  toy.decode(data, decoder())
  |> should.equal(
    Ok(User(
      name: "Thomas",
      email: "thomas@example.com",
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
      profile_picture: Some(Image(
        url: "https://example.com/picture.jpg",
        alt: "The best picture",
      )),
      title_picture: None,
    )),
  )
}
