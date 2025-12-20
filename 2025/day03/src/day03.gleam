import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import simplifile

pub type Part {
  Part1
  Part2
}

pub fn main() -> Nil {
  let assert [part, ..] = argv.load().arguments
  case simplifile.read("./input.txt"), part {
    Error(err), _ -> err |> simplifile.describe_error |> io.print_error
    Ok(input), "part1" -> {
      solve_part(Part1, input)
    }
    Ok(input), "part2" -> {
      solve_part(Part2, input)
    }
    Ok(_), _ -> panic as string.concat(["Unknown part: ", part])
  }
}

pub fn solve_part(part: Part, input: String) -> Nil {
  let result =
    input
    |> string.split("\n")
    |> list.filter_map(fn(line) {
      case line, part {
        "", _ -> Error(Nil)
        _, Part1 -> Ok(largest_joltage(line))
        _, Part2 -> Ok(largest_joltage12(line))
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

pub fn largest_joltage12(bank: String) -> Int {
  let len = string.length(bank)
  // io.println("Bank: " <> bank)

  let number_str =
    bank
    |> string.to_graphemes()
    |> list.index_fold([], fn(acc, digit_str, i) {
      let maybe_digit = int.parse(digit_str)
      let rem_digits_count = len - i

      // io.println(
      //   "Current digit "
      //   <> digit_str
      //   <> "(i:"
      //   <> int.to_string(i)
      //   <> "). Remaining digits: "
      //   <> int.to_string(rem_digits_count),
      // )
      case acc, list.length(acc), maybe_digit {
        _acc, _, Error(_) ->
          panic as string.concat(["Invalid digit: ", digit_str])
        [], _, Ok(digit) -> [digit]
        [prev, ..rest], acc_len, Ok(digit)
          if digit > prev && rem_digits_count > 12 - acc_len
        -> [digit, ..rest]
        acc, acc_len, Ok(digit) if acc_len < 12 -> [digit, ..acc]
        acc, _, Ok(_digit) -> acc
      }
    })
    |> list.reverse()
    |> list.map(int.to_string)
    |> string.join("")

  // io.println("=========\n")

  case int.parse(number_str) {
    Ok(num) -> num
    Error(_) ->
      panic as string.concat(["Failed to parse largest joltage: ", number_str])
  }
}
