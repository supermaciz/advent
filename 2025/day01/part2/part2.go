package main

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

type Safe struct {
	FirstNumber  int
	LastNumber   int
	CurrentState int
	ZeroCount    int
}

func (s *Safe) String() string {
	return fmt.Sprintf("Safe{CurrentState: %d, ZeroCount: %d}", s.CurrentState, s.ZeroCount)
}

const dialSize = 100

func (s *Safe) goLeft(distance int) {
	oldState := s.CurrentState

	s.ZeroCount += countZeroLeft(s.CurrentState, distance, dialSize)
	s.CurrentState = positiveModulo(s.CurrentState-distance, dialSize)
	fmt.Println(oldState, "goLeft", distance, "=>", s.String())
}

func (s *Safe) goRight(distance int) {
	oldState := s.CurrentState

	s.ZeroCount += countZeroRight(s.CurrentState, distance, dialSize)
	s.CurrentState = positiveModulo(s.CurrentState+distance, dialSize)
	fmt.Println(oldState, "goRight", distance, "=>", s.String())
}

func countZeroRight(start, distance, mod int) int {
	if distance <= 0 {
		return 0
	}
	start = positiveModulo(start, mod)

	// Starting on 0 doesn't count; only clicks that land on 0.
	if start == 0 {
		return distance / mod
	}

	// First time reaching 0 is after (mod - start) clicks.
	first := mod - start
	if distance < first {
		return 0
	}
	return 1 + (distance-first)/mod
}

func countZeroLeft(start, distance, mod int) int {
	if distance <= 0 {
		return 0
	}
	start = positiveModulo(start, mod)

	if start == 0 {
		return distance / mod
	}

	// First time reaching 0 is after 'start' clicks.
	first := start
	if distance < first {
		return 0
	}
	return 1 + (distance-first)/mod
}

func positiveModulo(a, b int) int {
	return ((a % b) + b) % b
}

func main() {
	safe := Safe{FirstNumber: 0, LastNumber: 99, CurrentState: 50}
	f, err := os.Open(os.Args[1])
	if err != nil {
		panic(err)
	}
	defer f.Close()
	sb := new(strings.Builder)
	io.Copy(sb, f)
	for s := range strings.SplitSeq(sb.String(), "\n") {
		if s == "" {
			break
		}
		switch s[0] {
		case 'L':
			distance, err := strconv.Atoi(s[1:])
			if err != nil {
				panic(err)
			}
			safe.goLeft(distance)
		case 'R':
			distance, err := strconv.Atoi(s[1:])
			if err != nil {
				panic(err)
			}
			safe.goRight(distance)
		default:
			panic("unknown command")
		}
	}

	fmt.Println("Password:", safe.ZeroCount)
}
