import argv
import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/result
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
        _, Part2 -> Ok(largest_joltage_n(line, 12))
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

pub fn largest_joltage_n(bank: String, n: Int) -> Int {
  io.println("Bank: " <> bank)

  let number_str =
    bank
    |> best_digits(0, n, [])
    |> list.reverse()
    |> list.map(int.to_string)
    |> string.join("")

  io.println("=========\n")

  case int.parse(number_str) {
    Ok(num) -> num
    Error(_) ->
      panic as string.concat(["Failed to parse largest joltage: ", number_str])
  }
}

fn best_digits(bank: String, i: Int, n: Int, acc: List(Int)) -> List(Int) {
  let bank_len = string.byte_size(bank)
  let remaining_bank_digits = bank_len - i
  let acc_len = list.length(acc)
  let digits_to_find = n - acc_len

  let maybe_next_acc = {
    use <- bool.guard(when: string.byte_size(bank) == i, return: Error(Nil))
    use maybe_digit <- result.try(bit_array.slice(
      bit_array.from_string(bank),
      i,
      1,
    ))
    use digit_str <- result.try(bit_array.to_string(maybe_digit))
    use digit <- result.try(int.parse(digit_str))

    io.print(
      "Current digit \""
      <> digit_str
      <> "\" (Index: "
      <> int.to_string(i)
      <> "). "
      <> "Remaining bank digits: "
      <> int.to_string(remaining_bank_digits)
      <> ". Digits to find: "
      <> int.to_string(digits_to_find),
    )

    case acc {
      [] -> {
        io.println(" => A")
        Ok([digit])
      }
      [prev, ..rest] if digit > prev && remaining_bank_digits > digits_to_find -> {
        io.println(" => B")
        Ok([digit, ..rest])
      }
      _ if digits_to_find >= 1 -> {
        io.println(" => C")
        Ok([digit, ..acc])
      }
      _ if remaining_bank_digits > 0 -> {
        io.println(" => D")
        Ok(acc)
      }

      _ -> {
        io.println(" => STOP")
        Error(Nil)
      }
    }
  }

  case maybe_next_acc {
    Ok(next_acc) -> best_digits(bank, i + 1, n, next_acc)
    Error(_) -> acc
  }
}
