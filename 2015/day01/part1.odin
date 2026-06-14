package day01

import "core:fmt"
import "core:os"
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


main :: proc() {
	data, err := os.read_entire_file("input.txt", context.allocator)
	if err != os.General_Error.None {
		fmt.eprintln("Impossible de lire le fichier", err)
		return
	}

	defer delete(data)

	content := string(data)
	result := compute_floor(content)
	fmt.println("day 01 result: ", result)
}
