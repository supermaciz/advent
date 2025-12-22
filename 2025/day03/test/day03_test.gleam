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

pub fn largest_joltage_n_test() {
  assert day03.largest_joltage_n("987654321111111", 12) == 987_654_321_111
  assert day03.largest_joltage_n("811111111111119", 12) == 811_111_111_119
  assert day03.largest_joltage_n("234234234234278", 12) == 434_234_234_278
  assert day03.largest_joltage_n("818181911112111", 12) == 888_911_112_111

  assert day03.largest_joltage_n("818181911112111", 2) == 92
}
