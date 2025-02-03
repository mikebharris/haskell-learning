package main

import "fmt"

func id[T any](x T) T {
	return x
}

func main() {
	fmt.Println(id(1))
	fmt.Println(id("Hello World!"))
	fmt.Println(id(false))
}
