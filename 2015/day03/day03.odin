package day03

import "core:fmt"
import "core:os"

Coor :: struct {
	x: int,
	y: int,
}

move_to_house :: proc(position: Coor, direction: byte) -> Coor {
	result := position

	switch direction {
	case '>':
		result.x += 1
	case '<':
		result.x -= 1
	case '^':
		result.y += 1
	case 'v':
		result.y -= 1
	}

	return result
}

visited_houses :: proc(moves: []byte) -> int {
	visited := make(map[Coor]bool)
	defer delete(visited)
	position: Coor
	visited[position] = true

	for move in moves {
		position = move_to_house(position, move)
		visited[position] = true
	}

	return len(visited)
}

main :: proc() {
	moves, err := os.read_entire_file("input.txt", context.allocator)
	if err != nil {
		fmt.eprintln("Can't open file:", err)
		return
	}
	defer delete(moves)

	result := visited_houses(moves)
	fmt.println("Part1:", result)
}
