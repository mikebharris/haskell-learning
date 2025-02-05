package main

// takes a function that takes a comparable as an argument and that returns any value
// and returns the same function, but "wrapper" in the memoizer
func memoize[A comparable, B any](f func(A) B) func(A) B {
	results := make(map[A]B)
	// returns a closure - as results is within scope
	// results is then within the closure
	return func(a A) B {
		if _, ok := results[a]; !ok {
			results[a] = f(a)
		}
		return results[a]
	}
}
