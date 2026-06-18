package day02

import "core:testing"


@(test)
test_paper_needed :: proc(t: ^testing.T) {
	test_cases := []struct {
		present:  Present,
		expected: int,
	}{{present = {2, 3, 4}, expected = 58}, {present = {1, 1, 10}, expected = 43}}

	for tc in test_cases {
		result := paper_needed(tc.present)
		testing.expect_value(t, result, tc.expected)
	}
}

@(test)
test_parse_line :: proc(t: ^testing.T) {
	data := "2x3x4\n"
	result := parse_line(data)
	expected := Present{2, 3, 4}
	testing.expect_value(t, result.length, expected.length)
	testing.expect_value(t, result.width, expected.width)
	testing.expect_value(t, result.height, expected.height)
}

@(test)
test_smallest_perimeter :: proc(t: ^testing.T) {
	test_cases := []struct {
		present:  Present,
		expected: int,
	}{{present = {2, 3, 4}, expected = 10}, {present = {1, 1, 10}, expected = 4}}

	for tc in test_cases {
		result := smallest_perimeter(tc.present)
		testing.expect_value(t, result, tc.expected)
	}
}

@(test)
test_ribbon_bow :: proc(t: ^testing.T) {
	test_cases := []struct {
		present:  Present,
		expected: int,
	}{{present = {2, 3, 4}, expected = 24}, {present = {1, 1, 10}, expected = 10}}

	for tc in test_cases {
		result := ribbon_bow(tc.present)
		testing.expect_value(t, result, tc.expected)
	}
}
