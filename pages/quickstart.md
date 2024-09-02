# Quickstart

Welcome to the Toy quickstart guide!

This guide will walk you through the basics of using Toy to decode and validate
dynamic data in Gleam.

## Installation

Toy is available on [Hex](https://hex.pm/packages/toy) and can be installed
with the following command:

```sh
gleam add toy
```

## Rule number one of Toy

**Don not perform side effects in the decoder**. I repeat, **do not perform side
effects in the decoder**.

```gleam
import toy
import gleam/dynamic
import gleam/dict

pub type User {
  User(name: String, age: Int)
}

pub fn user_decoder() {
  use name <- toy.field("name", toy.string)
  use age <- toy.field("age", toy.int)

  // DO NOT DO THIS
  log.info(name)
  // DO NOT DO THIS
  db.save_user(name, age)

  toy.decoded(User(name:, age:))
}

pub fn main() {
  let assert Ok(user) = dict.from_list([
    #("name", dynamic.from("Alice")),
    #("age", dynamic.from(42)),
  ])
  |> dynamic.from
  |> toy.decode(user_decoder())

  // Do this
  log.info(user.name)
  // Do this
  db.save_user(user.name, user.age)
}
```

The reason for this rule is simple. Every line of your decoder will be executed
always. If the name isn't a string, the decoder will continue to the next line
and check the age, and so on. This allows us to return errors for all fields at
once.

If name is not a string, the name variable will be set to a "default" value. In
case of a string, this is a hardcoded empty string `""`. In case of an int, it
is 0. etc.

So if you perform any side effects in the decoder, you will be presented with
unvalidated data. If you need to do something with the data, you should decode it
first. See the example above.

## Decoding a record

Okay, so, now that we have that out of the way, let's get to the meat of Toy.
In real applications, it is very common to decode a record. Be it from a JSON
payload, a database row, or a configuration file. Toy allows you to do this very
easily. Let's take a look at a simple user decoder.

```gleam
pub fn user_decoder() {
  use name <- toy.field("name", toy.string)
  use age <- toy.field("age", toy.int)

  toy.decoded(User(name:, age:))
}

pub fn main() {
  dict.from_list([
    #("name", dynamic.from("Alice")),
    #("age", dynamic.from(42)),
  ])
  |> dynamic.from
  |> toy.decode(user_decoder())
  |> should.equal(Ok(User(name: "Alice", age: 42)))
}
```

Our user has a *name* and an *age*. We use the `use` keyword along with the `field`
function that toy provides for us. The `field` function takes two arguments.

The first argument can by "anything". Toy will then attempt to index into the
dynamic value with the provided key. Most of the time, you will pass in a string
or an int as the key. In this case, we are trying to decode an object. Therefore
we pass the key `"name"` as a string.

The second argument is the decoder to use. Toy provides a number of decoders out
of the box. But they are not special at all. You could pass in your own decoder
as well. In fact, our function `user_decoder` creates a decoder, same as in Toy!
So we can easily compose decoders together.

```gleam
pub fn account_decoder() {
    use type_ <- toy.field("type", toy.string)
    use user <- toy.field("user", user_decoder())

    toy.decoded(Account(type_:, user:))
}
```

See! We can decode a record with a nested record just as easily as we can decode
a our user record. This is one of the goals of Toy. It aims to be *Composable*.

Last thing in our record decoding journey is the `toy.decoded` function. This
function isn't anything special. It creates a decoder that will always succeed
and return the provided value. The reason for it are the `use` expressions.
However, you don't have to worry about it for now, just remember to wrap your
final decoded value in this function.

> Also, notice how we use the label shorthand syntax to set the fields on
the final record. When you use this feature, you don't have to worry about the
order of arguments passed into the final record. Gleam will automatically assign
the `name` value to the `name` field in the record. If you use this shorthand
in all of your decoders (and we highly recommend it), you minimize the chance of
messing up the order of fields in the record, let's say during refactoring,
or developing a new feature.

## Different decoding based on a value

In real applications data is often complicated and varied. Let's say the user
sends us their pet preference as a string. How can we map this string into a
type? Or what if their pets have different attributes each? We need to decode
the correct attributes based on the pet type.

### Simple enum

Toy provides two main ways of decoding an Enum. If your enum is simple, you can
use the `toy.enum` function. The function is very simple. It takes a list of
mappings from the source value to the target value.

```gleam
pub type Pet {
  Dog
  Cat
  Fish
}

pub fn main() {
  let decoder = toy.string |> toy.enum([
    #("dog", Dog),
    #("cat", Cat),
    #("fish", Fish),
  ])

  let data = dynamic.from("dog")

  toy.decode(data, decoder)
  |> should.equal(Ok(Dog))
}
```

You may notice it is generic. Therefore you can use it over any value. Not just
strings.

```gleam
pub fn main() {
  let decoder = toy.int |> toy.enum([
    #(1, Dog),
    #(2, Cat),
    #(3, Fish),
  ])

  let data = dynamic.from(2)

  toy.decode(data, decoder)
  |> should.equal(Ok(Cat))
}
```

### Advanced enum

If your enum is more complicated, you may want to implement a custom decoder.
Don't worry, it is very simple.

```gleam
pub type Pet {
  Dog(tag: String)
  Cat(collar: String)
  Fish(color: String)
}

pub fn pet_decoder() {
  use type_ <- toy.field("type", toy.string)

  case type_ {
    "dog" -> {
      use tag <- toy.field("tag", toy.string)
      toy.decoded(Dog(tag:))
    }
    "cat" -> {
      use collar <- toy.field("collar", toy.string)
      toy.decoded(Cat(collar:))
    }
    "fish" -> {
      use color <- toy.field("color", toy.string)
      toy.decoded(Fish(color:))
    }
    _ -> toy.fail(toy.InvalidType("Pet", type_), Dog(""))
  }
}
```

You may notice that we pass in a `Dog("")` value to the `fail` function.
This dog will not escape this decoder. It is a default value that will be
returned if this decoder fails, so we can continue to decode the rest of the
data.

You may also ask, *How do I know if the "type_" field is a real value or a default?*.
The simple answer is that you don't. And you don't need to. You can always treat
the value as if it were valid. If it isn't valid, the field function that decoded
it will return an error. So you can safely use the values and rely on the upstream
decoders to return errors if they fail.

### Enum without discriminant

So, your API returns a value which can have many shapes, but no field can be
used to differentiate between them. Don't worry, Toy provides a solution for you.
The `toy.one_of` function takes a list of decoders and attempts to decode the
value with each of the decoders in order. The first successful one will be
returned. If none of the decoders are successful, all errors will be returned.
If you want to consolidate the errors, or just change them to something else,
you can use the `toy.map_errors` function.

```gleam
pub type Pet {
  Dog(tag: String)
  Cat(collar: String)
  Fish(color: String)
}

pub fn dog_decoder() {
  use tag <- toy.field("tag", toy.string)
  toy.decoded(Dog(tag:))
}

pub fn cat_decoder() {
  use collar <- toy.field("collar", toy.string)
  toy.decoded(Cat(collar:))
}

pub fn fish_decoder() {
  use color <- toy.field("color", toy.string)
  toy.decoded(Fish(color:))
}

pub fn main() {
  let decoder = toy.one_of([dog_decoder(), cat_decoder(), fish_decoder()])

  let data = dynamic.from(dict.from_list([
      #("tag", dynamic.from("woof")),
  ]))

  toy.decode(data, decoder)
  |> should.equal(Ok(Dog(tag: "woof")))
}
```

## Validation

Toy is not just a decoding library. It also provides validation functions.
This makes it easy to write one decoder, and be confidend that you data has 
the right shape, and values are within the expected limits.

Validation in Toy is done "one by one". This means that if one validation fails,
the others won't be executed.

Let's validate the age field of our user decoder.

```gleam
pub fn user_decoder() {
  use name <- toy.field("name", toy.string)
  use age <- toy.field("age", toy.int |> toy.int_min(18))

  toy.decoded(User(name:, age:))
}
```

It's simple! You just pipe the decoder to the validation function and that's it.

### Custom validation (refine)

toy doesn't contain all the validation functions that you might need. So we
provide you with the facility to create your own validation functions.
One of these is the `toy.refine` function, which has been inspired by the
[Zod](https://github.com/colinhacks/zod) typescript library.

```gleam
pub fn user_decoder() {
  use name <- toy.field("name", toy.string |> toy.refine(fn(name) {
    case name {
      "toy" -> Error([toy.ToyError(toy.ValidationFailed("name_taken", "new_name", name), [])])
      _ -> Ok(Nil)
    }
  }))
 toy.decoded(User(name:))
}
```


## Modifying the result

As you go by your day as a happy Gleam developer, you will find yourself in a
situation, where those pesky (Insert other language) devs return an object where
the value might be `null` or `undefined`. Toy provides you with the `toy.nullable`
function, which you can use exactly like any other validator. But this function
will actually modify the result of the decoder.

```gleam
pub fn user_decoder() {
  use name <- toy.field("name", toy.string |> toy.string_min(1) |> toy.nullable)
  toy.decoded(User(name:))
}
```

Notice, that the `toy.string_min(1)` function is applied before the `toy.nullable`.
This must be done this way. If you were to put it after the `toy.nullable`, the
compiler would complain, because after the `toy.nullable` function, the result
of the decoder is `Option(String)`.

This is not the only function that allows for this behavior. You can
use the `toy.map` or `toy.try_map` functions to create your own custom modifiers.

> There is one important difference between `toy.nullable` and `toy.map`. They
have different execution orders. The `toy.nullable` takes the dynamic value,
checks if it is `null` and if not, it passed it onto the next decoder.
`toy.map` on the other hand, takes the dynamic value, passes it onto the next
decoder, and then applies the function to the result of the decoder.
This enables the nice piping property of Toy's validation functions.
You will hopefully never run into this, but if you do, now you know.


