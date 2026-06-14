package day01

import "core:testing"

@(test)
test_compute_postion_1 :: proc(t: ^testing.T) {
	result := compute_position(")")
	expected := 1
	testing.expect_value(t, result, expected)
}

@(test)
test_compute_postion_5_instructions :: proc(t: ^testing.T) {
	result := compute_position("()())")
	expected := 5
	testing.expect_value(t, result, expected)
}
