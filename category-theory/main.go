package main

import (
	"fmt"
	"math/rand/v2"
)

// go run main.go id.go compose.go memoize.go
// go build main.go id.go compose.go memoize.go && main

func m(i int) int {
	return i * 2
}

func n(i int) int {
	return i * 3
}

func o(i int) int {
	return i * 100
}

func p(i int) string {
	return fmt.Sprintf("%x", i)
}

func printNonsenseThenAdd3(x int) int {
	fmt.Printf("wibble")
	return x + 3
}

// takes a unit () and returns an integer
// mathematically there are no pure fuctions that go from no set to a set
func f44() int {
	return 44
}

func main() {
	fmt.Println(id(1))
	fmt.Println(id("Hello World!"))
	fmt.Println(id(false))
	newFN := compose(m, n)
	fmt.Println("should return 60 : ", newFN(10))
	fmt.Println("should return 6,000 : ", composeMultiple(m, n, o)(10))
	multiplyBySixAndConvertToHex := compose(compose(m, n), p)
	fmt.Println("should retun 3c : ", multiplyBySixAndConvertToHex(10))
	fmt.Println(compose(id, multiplyBySixAndConvertToHex)(10) == multiplyBySixAndConvertToHex(10))

	memoized := memoize(printNonsenseThenAdd3)
	fmt.Println(memoized(4))
	fmt.Println(memoized(5))
	fmt.Println(memoized(4))

	fmt.Println("Not memoized call 1: ", rand.IntN(100))
	fmt.Println("Not memoized call 2: ", rand.IntN(100))

	memorizedSeed := memoize(rand.IntN)
	fmt.Println("Memoized call 1:", memorizedSeed(100))
	fmt.Println("Memoized call 2:", memorizedSeed(100))
	fmt.Println("Memoized call 3:", memorizedSeed(120))
	fmt.Println("Memoized call 4:", memorizedSeed(120))

	fmt.Println(f44())

	fmt.Println("**** Kleisi ****")
	fmt.Println("root 2: ", safe_root(2))
	fmt.Println("root -1: ", safe_root(-1))
	fmt.Println("reciprocal 2: ", safe_reciprocal(float32(2)))
	fmt.Println("reciprocal 0: ", safe_reciprocal(0))

	fmt.Println("stringthing 2: ", safe_stringthing(2))
	fmt.Println("stringthing 0: ", safe_stringthing(0))

	safe_root_reciprocal := fish(safe_root, safe_reciprocal)
	fmt.Println("srr 2: ", safe_root_reciprocal(2))
	fmt.Println("srr .5: ", safe_root_reciprocal(0.5))
	fmt.Println("srr -1: ", safe_root_reciprocal(-1))

	safe_foo := fish(safe_root_reciprocal, safe_stringthing)
	fmt.Println("string foo: ", *safe_foo(1).v)
	fmt.Println("string foo: ", safe_foo(1).b)
}
