import { Ok, Error, List } from "./gleam.mjs";
import { Some, None } from "../gleam_stdlib/gleam/option.mjs";
import { default as Dict } from "../gleam_stdlib/dict.mjs";
import { classify_dynamic } from "../gleam_stdlib/gleam_stdlib.mjs";
import { ToyError, InvalidType } from "./toy.mjs";

const NOTHING = Symbol.for("nothing");

/**
 * @param {Map | WeakMap | Dict} data
 * @param {string | number} key
 */
export function index(data, key) {
  const int = Number.isInteger(key);

  if (data instanceof Dict || data instanceof WeakMap || data instanceof Map) {
    const entry = data.get(key, NOTHING);
    if (entry != NOTHING) {
      return new Ok(new Some(entry));
    } else {
      return new Ok(new None());
    }
  }

  if (
    (int && Array.isArray(data)) ||
    (data && typeof data === "object") ||
    (data && Object.getPrototypeOf(data) === Object.prototype)
  ) {
    return new Ok(new Some(data[key]));
  }

  return new Error(int ? "Indexable" : "Dict");
}

export function is_nullish(value) {
  return value === null || value === undefined;
}

export function decode_option(value) {
  if (value instanceof None) {
    return new Ok(new None());
  }

  if (value instanceof Some) {
    return new Ok(value);
  }

  return new Error(undefined);
}

function decoder_error(expected, actual) {
  return new Error(
    List.fromArray([
      new ToyError(
        new InvalidType(expected, classify_dynamic(actual)),
        List.fromArray([]),
      ),
    ]),
  );
}

export function decode_map(data) {
  if (data instanceof Dict) {
    return new Ok(data);
  }
  if (data instanceof Map || data instanceof WeakMap) {
    return new Ok(Dict.fromMap(data));
  }
  if (data == null) {
    return decoder_error("Dict", data);
  }
  if (typeof data !== "object") {
    return decoder_error("Dict", data);
  }
  const proto = Object.getPrototypeOf(data);
  if (proto === Object.prototype || proto === null) {
    return new Ok(Dict.fromObject(data));
  }
  return decoder_error("Dict", data);
}
