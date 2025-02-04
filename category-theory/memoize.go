package main

func memoize[A comparable, B any](f func(A) B) func(A) B {
	results := make(map[A]B)
	return func(a A) B {
		if _, ok := results[a]; !ok {
			results[a] = f(a)
		}
		return results[a]
	}
}
