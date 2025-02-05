package main

// the id arrow - takes an object of any type and returns itself
func id[T any](x T) T {
	return x
}