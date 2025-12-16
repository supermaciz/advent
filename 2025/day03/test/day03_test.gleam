import day03
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn largest_joltage_test() {
  assert day03.largest_joltage("987654321111111") == 98
  assert day03.largest_joltage("811111111111119") == 89
  assert day03.largest_joltage("234234234234278") == 78
  assert day03.largest_joltage("818181911112111") == 92
}
