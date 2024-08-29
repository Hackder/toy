# Toy

Practical, composable dynamic decoders and validators for Gleam!

[![Package Version](https://img.shields.io/hexpm/v/toy)](https://hex.pm/packages/toy)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/toy/)

```sh
gleam add toy
```
```gleam
import gleam/dynamic
import gleam/json
import toy

pub type User {
  User(name: String, height: Float, age: Int)
}

pub fn user_decoder() {
  use name <- toy.field("name", toy.string)
  use height <- toy.field("height", toy.float |> toy.float_min(0.0))
  use age <- toy.field("age", toy.int |> toy.int_min(0))

  toy.decoded(User(name:, height:, age:))
}

pub fn main() {
  let assert Ok(data) =
    json.decode(
      "
  {
    \"name\": \"Alice\",
    \"height\": 1.7,
    \"age\": 25
  }
  ",
      dynamic.dynamic,
    )

  let user = data |> toy.decode(user_decoder())
}
```

## Goals

Toy aims to provide a simple to use, composable api for decoding and validating
dynamic data in Gleam. It is inspired by the gleam standard library,
[decode](https://hexdocs.pm/decode/) and [Zod](https://zod.dev/).

Toy strives to satisfy all of the following goals:
- Simple to use and composable
- Good DX
- Type safe
- Minimize surface for dumb mistakes
- Return all error messages possible
- Allow for easy internationalization of error messages
- Provide validation functions for common use cases

## Why not existing solutions?

In application development you most of the time decode dynamic values
into records (objects in other lanugages).
It might be a JSON payload from an API, a configuration file,
or a database row. So this will be the main focus of this comparison.

### Gleam standard library [gleam_stdlib/dynamic](https://hexdocs.pm/gleam_stdlib/gleam/dynamic.html)

```gleam
pub fn user_decoder_stdlib() {
  dynamic.decode3(
    User,
    dynamic.field("name", dynamic.string),
    dynamic.field("height", dynamic.float),
    dynamic.field("age", dynamic.int),
  )
}
```

The standard library requires two thing from the developer. You have to
use the right function, `decode3` in this case, because the user has 3 fields
and *you have to list the fields in the same order as the record definition*.

The first point is generally not a problem until you reach 10 fields, because
there is no `decode10` function. The second point is a bigger problem.
When developing, it is very common to modify existing records (add/remove fields).
When you do this, it is very easy to mess up the order int the decoder. And if the
two fields you swapped are both strings, the compiler will not catch this mistake.
**You will end up with bad data in your application**.

### The decode package [decode](https://hexdocs.pm/decode/)

```gleam
pub fn user_decoder_decode() {
  decode.into({
    use name <- decode.parameter
    use height <- decode.parameter
    use age <- decode.parameter
    User(name, height, age)
  })
  |> decode.field("name", decode.string)
  |> decode.field("height", decode.float)
  |> decode.field("age", decode.int)
}
```

The decode package is a bit better than the standard library. Now you can decode
records with any number of fields in the same way easily. However the second
problem still persists. The second `decode.field("height", decode.float)` call 
has to match up with the second `use height <- decode.parameter` call, which
needs to match up with the right field in the record definition. If you mess
this up during refactoring or developing a new feature, **you will end up with
bad data in your application.**

### Toy

```gleam
pub fn user_decoder_toy() {
  use name <- toy.field("name", toy.string)
  use height <- toy.field("height", toy.float)
  use age <- toy.field("age", toy.int)

  toy.decoded(User(name:, height:, age:))
}
```

Toy solves both problems. Each field is defined on a single line. And since
gleam 1.4.0, we can construct the record with the *label shorthand* syntax.
By using this syntax the compiler guarantees that the *name* value will be
assigned to the name field in the record, regardless of the order you specify
them in. This makes is much harder to mess up the decoder when refactoring.

But there is a catch. **Every line of the decoder will be executed on every
invocation**. To return errors for all fields at once, we need to continue
decoding even after the first error. *But what will be in the `name` variable
if the `name` field failed to decode?* The answer is a default string value
(empty string in this case). So you have to make sure to not perform any
side effects in the decoder, because withing the decoder function you are never
guaranteed that the data is actually valid.

```gleam
pub fn user_decoder_toy() {
  use name <- toy.field("name", toy.string)
  // This is bad, you should never perform side effects in the decoder
  io.println(name)
  use height <- toy.field("height", toy.float)
  use age <- toy.field("age", toy.int)

  toy.decoded(User(name:, height:, age:))
}
```

This is not as scary as it seems. When you are using toy and decode a value with
it, you will always get valid data. The rule only applies withing the decoder
itself.

And in practice, the rule *don't perform side effects in the decoder* is much
easier to follow than the rule *make sure the field order in the decoder always
matches the record definition*.

## Usage

Here is a complex example of a decoder, including validation. This should give
you a good overview of the capabilities of toy.

```gleam
import birl
import gleam/dynamic
import gleam/io
import gleam/json
import gleam/option.{type Option}
import gleam/order
import gleam/result
import toy

pub type Guest {
  Guest(name: String, age: Int, special_needs: Option(String))
}

pub type Table {
  Any
  Specific(table_number: Int)
}

pub type Reservation {
  Reservation(
    person_name: String,
    email: String,
    datetime: birl.Time,
    guests: List(Guest),
    table: Table,
    notes: Option(String),
  )
}

pub fn time_decoder() {
  toy.string
  |> toy.try_map(birl.now(), fn(val) {
    birl.parse(val)
    |> result.replace_error([toy.ToyError(toy.InvalidType("DateTime", val), [])])
  })
}

pub fn time_future(val: birl.Time) {
  case birl.compare(val, birl.now()) {
    order.Gt -> Ok(Nil)
    _ ->
      Error([
        toy.ToyError(
          toy.ValidationFailed("date_future", "DateTime", birl.to_http(val)),
          [],
        ),
      ])
  }
}

pub fn reservation_decoder() {
  use person_name <- toy.field("person_name", toy.string |> toy.string_nonempty)
  use email <- toy.field("email", toy.string |> toy.string_email)
  use datetime <- toy.field(
    "datetime",
    time_decoder() |> toy.refine(time_future),
  )
  use guests <- toy.field(
    "guests",
    toy.list({
      use name <- toy.field("name", toy.string)
      use age <- toy.field("age", toy.int |> toy.int_min(18))
      use special_needs <- toy.optional_field("special_needs", toy.string)
      toy.decoded(Guest(name:, age:, special_needs:))
    })
      |> toy.list_nonempty,
  )
  use table <- toy.field("table", {
    use type_ <- toy.field("type", toy.string)

    case type_ {
      "specific" -> {
        use table_number <- toy.field("table_number", toy.int)
        toy.decoded(Specific(table_number:))
      }
      "any" -> toy.decoded(Any)
      _ -> toy.fail(toy.InvalidType("Table", type_), Any)
    }
  })
  use notes <- toy.field("notes", toy.string |> toy.nullable)
  toy.decoded(Reservation(
    person_name:,
    email:,
    datetime:,
    guests:,
    table:,
    notes:,
  ))
}

const data = "
{
  \"person_name\": \"Alice\",
  \"email\": \"alice@example.com\",
  \"datetime\": \"2025-01-01T12:00:00Z\",
  \"guests\": [
    { \"name\": \"Alice\", \"age\": 25 },
    { \"name\": \"Bob\", \"age\": 30, \"special_needs\": \"Vegan\" }
  ],
  \"table\": { \"type\": \"specific\", \"table_number\": 1 },
  \"notes\": null
}
"

pub fn main() {
  let assert Ok(data) = json.decode(data, dynamic.dynamic)

  data |> toy.decode(reservation_decoder()) |> io.debug
}
```

Further documentation can be found at <https://hexdocs.pm/toy>.

