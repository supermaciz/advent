package day01

import "core:fmt"
import "core:os"

// Part 1
compute_floor :: proc(instructions: string) -> (floor: int) {
	for x in instructions {
		if x == '(' {
			floor += 1
		} else if x == ')' {
			floor -= 1
		}
	}
	return floor
}

// Part 2
compute_position :: proc(instructions: string) -> (position: int) {
	floor: int
	for x, i in instructions {
		if x == '(' {
			floor += 1
		} else if x == ')' {
			floor -= 1
		}
		if floor == -1 {
			position = i + 1
			break
		}
	}
	return position
}


main :: proc() {
	data, err := os.read_entire_file("input.txt", context.allocator)
	if err != os.General_Error.None {
		fmt.eprintln("Impossible de lire le fichier", err)
		return
	}

	defer delete(data)

	content := string(data)
	result := compute_floor(content)
	fmt.println("day 01 result:", result)

	result2 := compute_position(content)
	fmt.println("part2 result:", result2)
}
