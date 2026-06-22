package day03

import "core:fmt"
import "core:os"

Coor :: struct {
	x: int,
	y: int,
}

Santa :: enum {
	Original,
	Robo,
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

santa_type :: proc(move_index: int) -> Santa {
	if move_index % 2 == 0 {
		return .Original
	}
	return .Robo
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

visited_houses_2_santas :: proc(moves: []byte) -> int {
	visited := make(map[Coor]bool)
	defer delete(visited)
	position_santa: Coor
	position_robo: Coor
	visited[position_santa] = true

	for move, i in moves {
		type := santa_type(i)
		if type == .Original {
			position_santa = move_to_house(position_santa, move)
			visited[position_santa] = true
		} else if type == .Robo {
			position_robo = move_to_house(position_robo, move)
			visited[position_robo] = true
		}
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
	fmt.println("Part 1:", result)

	result2 := visited_houses_2_santas(moves)
	fmt.println("Part 2:", result2)
}
