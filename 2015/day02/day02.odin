package day02

import "core:bufio"
import "core:fmt"
import "core:io"
import "core:os"
import "core:strconv"
import "core:strings"

// A right rectangular prism
Present :: struct {
	length: int,
	width:  int,
	height: int,
}

compute_present_paper :: proc(present: Present) -> int {
	return 2 * present.length * present.width +
		2 * present.width * present.height +
		2 * present.height * present.length +
		compute_smallest_surface(present)
}

compute_smallest_surface :: proc(present: Present) -> int {
	return min(
		present.length * present.width,
		present.height * present.width,
		present.length * present.height,
	)
}

parse_line :: proc(line: string) -> Present {
	present: Present
	numbers := strings.split(line, "x")
	defer delete(numbers)
	for nb, i in numbers {
		tmp, _ := strconv.parse_int(nb)
		switch i {
		case 0:
			present.length = tmp
		case 1:
			present.width = tmp
		case 2:
			present.height = tmp
		}
	}
	return present
}

solve_part1 :: proc(reader: ^bufio.Reader) -> int {
	result: int
	for {
		line, err := bufio.reader_read_string(reader, '\n')
		if len(line) > 0 {
			fmt.print("Line:", line)
			result += compute_present_paper(parse_line(line))
			delete(line)
		}

		if err != nil {
			if err != io.Error.EOF {
				fmt.eprintln("Erreur de lecture:", err)
			}
			break
		}
	}

	return result
}


main :: proc() {
	file, err := os.open("input.txt")
	if err != nil {
		fmt.eprintln("Impossible d'ouvrir le fichier:", err)
		return
	}
	defer os.close(file)

	reader: bufio.Reader
	file_stream := os.to_reader(file)
	bufio.reader_init(&reader, file_stream)
	defer bufio.reader_destroy(&reader)

	fmt.println("\n\n______\nPart 1:", solve_part1(&reader))
}
