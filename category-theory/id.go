package main

func id[T any](x T) T {
	return x
}