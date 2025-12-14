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

func (s *Safe) goLeft(distance int) {
	oldState := s.CurrentState
	tmp := s.CurrentState - distance
	s.CurrentState = positiveModulo(tmp, 100)
	s.checkZero()
	fmt.Println(oldState, "goLeft", distance, "=>", s.String())
}

func (s *Safe) goRight(distance int) {
	oldState := s.CurrentState
	tmp := s.CurrentState + distance
	s.CurrentState = positiveModulo(tmp, 100)
	s.checkZero()
	fmt.Println(oldState, "goRight", distance, "=>", s.String())
}

func (s *Safe) checkZero() {
	if s.CurrentState == 0 {
		s.ZeroCount++
	}
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
