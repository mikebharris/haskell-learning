package main

func compose[A any, B any, C any](f func(A) B, g func(B) C) func(A) C {
	return func(a A) C {
		return g(f(a))
	}
}

func composeMultiple[A any](manyFuncs ...func(A) A) func(A) A {
	return func(a A) A {
		for _, f := range manyFuncs {
			a = f(a)
		}
		return a
	}
}
