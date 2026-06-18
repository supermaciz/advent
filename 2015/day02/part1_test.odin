package day02

import "core:flags/example"
import "core:testing"


@(test)
test_compute_present_paper1 :: proc(t: ^testing.T) {
	result := compute_present_paper(Present{2, 3, 4})
	expected := 58
	testing.expect_value(t, result, expected)
}

@(test)
test_compute_present_paper2 :: proc(t: ^testing.T) {
	result := compute_present_paper(Present{1, 1, 10})
	expected := 43
	testing.expect_value(t, result, expected)
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
