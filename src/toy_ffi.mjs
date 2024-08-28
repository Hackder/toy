import { Ok, Error } from "./gleam.mjs";
import { Some, None } from "../gleam_stdlib/gleam/option.mjs";
import { default as Dict } from "../gleam_stdlib/dict.mjs";

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
