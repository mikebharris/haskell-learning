package main

// takes a function that takes a value in set A and returns a value in set B
// and takes another function that takes a value in set B and returns a value in set C
// and thus returns a new composite function that takes a value in set A and returns a value in set C
func compose[A any, B any, C any](f func(A) B, g func(B) C) func(A) C {
	return func(a A) C {
		return g(f(a))
	}
}

// the same but for an infinite list of functions
// but composition I think strictly is just of two functions
// one just nests them, as in i := compose(h, (compose f, g))
func composeMultiple[A any](manyFuncs ...func(A) A) func(A) A {
	return func(a A) A {
		for _, f := range manyFuncs {
			a = f(a)
		}
		return a
	}
}
