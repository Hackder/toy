import gleam/dict
import gleam/dynamic
import gleam/int
import gleam/list
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

pub fn bool_test() {
  toy.decode(dynamic.from(True), toy.bool)
  |> should.equal(Ok(True))
}

pub fn bool_invalid_test() {
  toy.decode(dynamic.from(23), toy.bool)
  |> should.equal(Error([toy.ToyError(toy.InvalidType("Bool", "Int"), [])]))
}

pub fn bool_string_test() {
  [#("true", True), #("True", True), #("false", False), #("False", False)]
  |> list.each(fn(item) {
    toy.decode(dynamic.from(item.0), toy.bool_string)
    |> should.equal(Ok(item.1))
  })
}

pub fn bool_string_invalid_test() {
  toy.decode(dynamic.from("thing"), toy.bool_string)
  |> should.equal(
    Error([
      toy.ToyError(
        toy.ValidationFailed("bool_string", "BoolString", "thing"),
        [],
      ),
    ]),
  )

  toy.decode(dynamic.from("1"), toy.bool_string)
  |> should.equal(
    Error([
      toy.ToyError(toy.ValidationFailed("bool_string", "BoolString", "1"), []),
    ]),
  )

  toy.decode(dynamic.from(123), toy.bool_string)
  |> should.equal(Error([toy.ToyError(toy.InvalidType("String", "Int"), [])]))
}

pub fn equal_test() {
  toy.decode(
    dynamic.from("Hello Joe!"),
    toy.string |> toy.is_equal("Hello Joe!"),
  )
  |> should.equal(Ok("Hello Joe!"))
}

pub fn equal_invalid_test() {
  toy.decode(
    dynamic.from("Hello Joe!"),
    toy.string |> toy.is_equal("Hello joe!"),
  )
  |> should.equal(
    Error([
      toy.ToyError(
        toy.ValidationFailed("is_equal", "\"Hello joe!\"", "\"Hello Joe!\""),
        [],
      ),
    ]),
  )
}

pub fn nullish_test() {
  toy.decode(dynamic.from(Nil), toy.nullish)
  |> should.equal(Ok(Nil))
}

pub fn nullish_invalid_test() {
  toy.decode(dynamic.from(1), toy.nullish)
  |> should.equal(Error([toy.ToyError(toy.InvalidType("Nil", "Int"), [])]))
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

pub fn int_string_test() {
  toy.decode(dynamic.from("12345"), toy.int_string)
  |> should.equal(Ok(12_345))

  toy.decode(dynamic.from("-12345"), toy.int_string)
  |> should.equal(Ok(-12_345))
}

pub fn int_string_invalid_test() {
  toy.decode(dynamic.from("123dd2"), toy.int_string)
  |> should.equal(
    Error([
      toy.ToyError(
        toy.ValidationFailed("int_string", "IntString", "123dd2"),
        [],
      ),
    ]),
  )
}

pub fn float_string_test() {
  toy.decode(dynamic.from("12345.003"), toy.float_string)
  |> should.equal(Ok(12_345.003))

  toy.decode(dynamic.from("-123"), toy.float_string)
  |> should.equal(Ok(-123.0))
}

pub fn float_string_invalid_test() {
  toy.decode(dynamic.from("123dd2.3"), toy.float_string)
  |> should.equal(
    Error([
      toy.ToyError(
        toy.ValidationFailed("float_string", "FloatString", "123dd2.3"),
        [],
      ),
    ]),
  )
}

pub fn int_map_test() {
  let data = dynamic.from(42)
  toy.decode(data, toy.int |> toy.map(fn(val) { val + 1 }))
  |> should.equal(Ok(43))
}

pub fn nullable_nil_test() {
  let decoder = toy.int |> toy.nullable
  let data = dynamic.from(Nil)

  toy.decode(data, decoder) |> should.equal(Ok(None))
}

pub fn nullable_validated_test() {
  let decoder = toy.int |> toy.int_min(10) |> toy.nullable

  dynamic.from(11)
  |> toy.decode(decoder)
  |> should.equal(Ok(Some(11)))
}

pub fn option_none_test() {
  let decoder = toy.int |> toy.option

  dynamic.from(None)
  |> toy.decode(decoder)
  |> should.equal(Ok(None))
}

pub fn option_validated_test() {
  let decoder = toy.int |> toy.int_min(10) |> toy.option

  dynamic.from(Some(11))
  |> toy.decode(decoder)
  |> should.equal(Ok(Some(11)))
}

pub type Pet {
  Dog(tag: String)
  Cat(collar: String)
  Fish(color: String)
}

pub fn one_of_test() {
  let dog_decoder = fn() {
    use tag <- toy.field("tag", toy.string)
    toy.decoded(Dog(tag:))
  }

  let cat_decoder = fn() {
    use collar <- toy.field("collar", toy.string)
    toy.decoded(Cat(collar:))
  }

  let fish_decoder = fn() {
    use color <- toy.field("color", toy.string)
    toy.decoded(Fish(color:))
  }

  let decoder = toy.one_of([dog_decoder(), cat_decoder(), fish_decoder()])

  dict.from_list([#("tag", dynamic.from("woof"))])
  |> dynamic.from
  |> toy.decode(decoder)
  |> should.equal(Ok(Dog(tag: "woof")))

  dict.from_list([#("feathers", dynamic.from("blue"))])
  |> dynamic.from
  |> toy.decode(decoder)
  |> should.equal(
    Error([
      toy.ToyError(
        toy.AllFailed([
          [toy.ToyError(toy.Missing("String"), ["\"tag\""])],
          [toy.ToyError(toy.Missing("String"), ["\"collar\""])],
          [toy.ToyError(toy.Missing("String"), ["\"color\""])],
        ]),
        [],
      ),
    ]),
  )
}

pub fn map_errors_test() {
  let dog_decoder = fn() {
    use tag <- toy.field("tag", toy.string)
    toy.decoded(Dog(tag:))
  }

  let cat_decoder = fn() {
    use collar <- toy.field("collar", toy.string)
    toy.decoded(Cat(collar:))
  }

  let decoder =
    toy.one_of([dog_decoder(), cat_decoder()])
    |> toy.map_errors(fn(_errors) {
      [toy.ToyError(toy.InvalidType("Animal", ""), [])]
    })

  dynamic.from(Nil)
  |> toy.decode(decoder)
  |> should.equal(Error([toy.ToyError(toy.InvalidType("Animal", ""), [])]))
}

pub type Address {
  Address(street: String, city: String, zip: Int)
}

pub type Friend {
  Friend(
    name: String,
    age: Int,
    height: Float,
    address: Address,
    profile_picture: Option(Image),
  )
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

pub fn subfield_test() {
  let decoder = {
    use title <- toy.subfield(["city", "library", "book", "title"], toy.string)
    toy.decoded(title)
  }

  dict.from_list([
    #(
      "city",
      dict.from_list([
        #(
          "library",
          dict.from_list([
            #("book", dict.from_list([#("title", "The Gleam tour")])),
          ]),
        ),
      ]),
    ),
  ])
  |> dynamic.from
  |> toy.decode(decoder)
  |> should.equal(Ok("The Gleam tour"))
}

pub fn subfield_missing_test() {
  let decoder = {
    use title <- toy.subfield(["city", "library", "bookk", "title"], toy.string)
    toy.decoded(title)
  }

  dict.from_list([
    #(
      "city",
      dict.from_list([
        #(
          "library",
          dict.from_list([
            #("book", dict.from_list([#("title", "The Gleam tour")])),
          ]),
        ),
      ]),
    ),
  ])
  |> dynamic.from
  |> toy.decode(decoder)
  |> should.equal(
    Error([
      toy.ToyError(toy.Missing("Dict"), ["\"city\"", "\"library\"", "\"bookk\""]),
    ]),
  )
}

pub fn subfield_invalid_test() {
  let decoder = {
    use title <- toy.subfield(["city", "library", "book", "title"], toy.string)
    toy.decoded(title)
  }

  dict.from_list([
    #(
      "city",
      dict.from_list([
        #("library", dict.from_list([#("book", "Crime and Punishment")])),
      ]),
    ),
  ])
  |> dynamic.from
  |> toy.decode(decoder)
  |> should.equal(
    Error([
      toy.ToyError(toy.InvalidType("Dict", "String"), [
        "\"city\"", "\"library\"", "\"book\"", "\"title\"",
      ]),
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

pub fn dict_valid_test() {
  let decoder = toy.dict(toy.string, toy.int)

  let data = dict.from_list([#("hi", 1), #("another", 2)])

  data
  |> dynamic.from
  |> toy.decode(decoder)
  |> should.equal(Ok(data))
}

pub fn dict_invalid_test() {
  let decoder = toy.dict(toy.string, toy.int)

  dynamic.from(Nil)
  |> toy.decode(decoder)
  |> should.equal(Error([toy.ToyError(toy.InvalidType("Dict", "Nil"), [])]))
}

pub fn dict_invalid_value_test() {
  let decoder = toy.dict(toy.string, toy.int)

  let data =
    dict.from_list([
      #("hi", dynamic.from(1)),
      #("another", dynamic.from("thing")),
    ])

  data
  |> dynamic.from
  |> toy.decode(decoder)
  |> should.equal(
    Error([toy.ToyError(toy.InvalidType("Int", "String"), ["values"])]),
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
        use profile_picture <- toy.field(
          "profile_picture",
          image_decoder() |> toy.nullable,
        )
        toy.decoded(Friend(name:, age:, height:, address:, profile_picture:))
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
              "profile_picture",
              dict.from_list([
                #("url", "https://example.com/picture.jpg"),
                #("alt", "The best picture"),
              ])
                |> dynamic.from,
            ),
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
          dict.from_list([
            #("name", dynamic.from("Bob")),
            #("age", dynamic.from(23)),
            #("height", dynamic.from(1.3)),
            #("profile_picture", dynamic.from(Nil)),
            #(
              "address",
              dict.from_list([
                #("street", dynamic.from("456 Haskell Road")),
                #("city", dynamic.from("Narnia")),
                #("zip", dynamic.from(98_767)),
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
          profile_picture: Some(Image(
            url: "https://example.com/picture.jpg",
            alt: "The best picture",
          )),
        ),
        Friend(
          name: "Bob",
          age: 23,
          height: 1.3,
          address: Address("456 Haskell Road", "Narnia", 98_767),
          profile_picture: None,
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
