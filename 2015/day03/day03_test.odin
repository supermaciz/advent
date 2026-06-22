package day03

import "core:testing"


@(test)
test_visited_houses :: proc(t: ^testing.T) {
	test_cases := []struct {
		moves:    []byte,
		expected: int,
	} {
		{[]byte{'>'}, 2},
		{[]byte{'^', '>', 'v', '<'}, 4},
		{[]byte{'^', 'v', '^', 'v', '^', 'v', '^', 'v', '^', 'v'}, 2},
	}
	for tc in test_cases {
		result := visited_houses(tc.moves)
		testing.expectf(
			t,
			result == tc.expected,
			"Moves: [%s]. Result: %d. Expected: %d",
			tc.moves,
			result,
			tc.expected,
		)
		testing.expect_value(t, result, tc.expected)
	}
}

@(test)
test_visited_houses_2_santas :: proc(t: ^testing.T) {
	test_cases := []struct {
		moves:    []byte,
		expected: int,
	} {
		{[]byte{'^', 'v'}, 3},
		{[]byte{'^', '>', 'v', '<'}, 3},
		{[]byte{'^', 'v', '^', 'v', '^', 'v', '^', 'v', '^', 'v'}, 11},
	}
	for tc in test_cases {
		result := visited_houses_2_santas(tc.moves)
		testing.expectf(
			t,
			result == tc.expected,
			"Moves: [%s]. Result: %d. Expected: %d",
			tc.moves,
			result,
			tc.expected,
		)
		testing.expect_value(t, result, tc.expected)
	}
}
