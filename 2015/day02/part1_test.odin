package day02

import "core:flags/example"
import "core:testing"


@(test)
test_compute_present_paper1 :: proc(t: ^testing.T) {
	result := compute_present_paper(2, 3, 4)
	expected := 58
	testing.expect_value(t, result, expected)
}

@(test)
test_compute_present_paper2 :: proc(t: ^testing.T) {
	result := compute_present_paper(1, 1, 10)
	expected := 43
	testing.expect_value(t, result, expected)
}

@(test)
test_parse_line :: proc(t: ^testing.T) {
	data := "2x3x4\n"
	result_l, result_w, result_h := parse_line(data)
	expected_l, expected_w, expected_h := 2, 3, 4
	testing.expect_value(t, result_l, expected_l)
	testing.expect_value(t, result_w, expected_w)
	testing.expect_value(t, result_h, expected_h)
}
