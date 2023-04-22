package main

import (
	"fmt"
	"math"
	"reflect"
)

// define
type number interface {
	int | int8 | int16 | int32 | int64 | float32 | float64 | complex64 | complex128
}

type tuple[N number] struct {
	a, b N
}

type anyTuple[T any] struct {
	a, b T
}

func add2together[N number](a, b N) N {
	return a + b
}

func addTupleTogether[N number](t tuple[N]) N {
	return t.a + t.b
}

func main() {
	fmt.Println("Numbers")
	fmt.Println(add2together(2, 3))
	fmt.Println(add2together(math.Pi, math.E))
	fmt.Println(add2together(2+5i, 6-3i))

	fmt.Println("\nTuples")
	fmt.Println(addTupleTogether(tuple[int]{2, 3}))
	fmt.Println(addTupleTogether(tuple[float64]{math.Pi, math.E}))
	fmt.Println(addTupleTogether(tuple[complex64]{2 + 5i, 6 - 3i}))

	jp := journalPayload{journalId: 0}
	process(jp)

	sp := submissionPayload{
		journalId:  0,
		documentId: 0,
		revision:   0,
	}
	process(sp)

	/*	e := emailPayload{thing: 0}
		process(e) // can't do this due to type constraint
	*/
}

type journalPayload struct {
	journalId int
}

type submissionPayload struct {
	journalId  int
	documentId int
	revision   int
}
type payload interface {
	journalPayload | submissionPayload
	process()
}

type emailPayload struct {
	thing int
}

func (e emailPayload) process() {
	//
}

func (p journalPayload) process() {
	fmt.Println("processing the journal payload")
}

func (p submissionPayload) process() {
	fmt.Println("processing the submission payload")
}
func process[P payload](p P) {
	fmt.Printf("processing the %s\n", reflect.TypeOf(p).Name())
	p.process()
}
