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

pub fn largest_joltage12_test() {
  assert day03.largest_joltage12("987654321111111") == 987_654_321_111
  assert day03.largest_joltage12("811111111111119") == 811_111_111_119
  assert day03.largest_joltage12("234234234234278") == 434_234_234_278
  assert day03.largest_joltage12("818181911112111") == 888_911_112_111
}
