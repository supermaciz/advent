package day02

import "core:bufio"
import "core:fmt"
import "core:io"
import "core:os"
import "core:strconv"
import "core:strings"

compute_present_paper :: proc(l, w, h: int) -> int {
	return 2 * l * w + 2 * w * h + 2 * h * l + compute_smallest_surface(l, w, h)
}

compute_smallest_surface :: proc(l, w, h: int) -> int {
	return min(l * w, h * w, l * h)
}

parse_line :: proc(line: string) -> (l, w, h: int) {
	numbers := strings.split(line, "x")
	defer delete(numbers)
	for nb, i in numbers {
		tmp, ok := strconv.parse_int(nb)
		switch i {
		case 0:
			l = tmp
		case 1:
			w = tmp
		case 2:
			h = tmp
		}
	}
	return
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
