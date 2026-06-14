package day01

import "core:testing"

@(test)
test_0_floor :: proc(t: ^testing.T) {
	result := compute_floor("(())")
	expected := 0
	testing.expect_value(t, result, expected)
}

@(test)
test_0_floor_alt :: proc(t: ^testing.T) {
	result := compute_floor("()()")
	expected := 0
	testing.expect_value(t, result, expected)
}

@(test)
test_3_floor_open :: proc(t: ^testing.T) {
	result := compute_floor("(((")
	expected := 3
	testing.expect_value(t, result, expected)
}

@(test)
test_3_floor_mixed :: proc(t: ^testing.T) {
	result := compute_floor("(()(()(")
	expected := 3
	testing.expect_value(t, result, expected)
}

@(test)
test_3_floor_close_then_open :: proc(t: ^testing.T) {
	result := compute_floor("))(((((")
	expected := 3
	testing.expect_value(t, result, expected)
}

@(test)
test_neg1_floor :: proc(t: ^testing.T) {
	result := compute_floor("())")
	expected := -1
	testing.expect_value(t, result, expected)
}

@(test)
test_neg1_floor_alt :: proc(t: ^testing.T) {
	result := compute_floor("))(")
	expected := -1
	testing.expect_value(t, result, expected)
}

@(test)
test_neg3_floor :: proc(t: ^testing.T) {
	result := compute_floor(")))")
	expected := -3
	testing.expect_value(t, result, expected)
}

@(test)
test_neg3_floor_alt :: proc(t: ^testing.T) {
	result := compute_floor(")())())")
	expected := -3
	testing.expect_value(t, result, expected)
}
