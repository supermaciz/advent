import argv
import gleam/int
import gleam/io
import gleam/list.{Continue}
import gleam/string
import simplifile

pub fn main() -> Nil {
  let assert [part, ..] = argv.load().arguments
  case simplifile.read("./input.txt"), part {
    Error(err), _ -> err |> simplifile.describe_error |> io.print_error
    Ok(input), "part1" -> {
      part1(input)
    }
    // Ok(input), "part2" -> {
    //   part2(input)
    // }
    Ok(_), _ -> panic as string.concat(["Unknown part: ", part])
  }
}

pub fn part1(input: String) -> Nil {
  let result =
    input
    |> string.split("\n")
    |> list.filter_map(fn(line) {
      case line {
        "" -> Error(Nil)
        _ -> Ok(largest_joltage(line))
      }
    })
    |> int.sum()

  io.println("Part 1: " <> int.to_string(result))
}

pub fn largest_joltage(bank: String) -> Int {
  let #(first, second) =
    bank
    |> string.to_graphemes()
    |> list.fold_until(#(-1, -1), fn(acc, digit_str) {
      let maybe_digit = int.parse(digit_str)

      case acc, maybe_digit {
        _acc, Error(_) -> panic as string.concat(["Invalid digit: ", digit_str])
        #(-1, -1), Ok(digit) -> list.Continue(#(digit, -1))
        #(d1, -1), Ok(digit) -> list.Continue(#(d1, digit))
        #(d1, d2), Ok(digit) if d2 > d1 -> list.Continue(#(d2, digit))
        #(d1, d2), Ok(digit) if digit > d2 -> list.Continue(#(d1, digit))
        #(d1, d2), Ok(digit) if digit <= d2 -> list.Continue(#(d1, d2))
        acc, Ok(_) -> list.Continue(acc)
      }
    })

  let number_str = int.to_string(first) <> int.to_string(second)
  case int.parse(number_str) {
    Ok(num) -> num
    Error(_) ->
      panic as string.concat(["Failed to parse largest joltage: ", number_str])
  }
}
