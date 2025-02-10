package main

import "math"

type Opt[T any] struct {
	v T
	b bool
}

func idk[A any](a A) Opt[A] {
	return Opt[A]{a, true}
}

func fish[A any, B any, C any](f func(A) Opt[B], g func(B) Opt[C]) func(A) Opt[C] {
	return func(a A) Opt[C] {
		o1 := f(a)
		if !o1.b {
			return Opt[C]{b: false}
		}
		return g(o1.v)
	}
}

func safe_root(x float32) Opt[float32] {
	if x >= 0 {
		return Opt[float32]{float32(math.Sqrt(float64(x))), true}
	} else {
		return Opt[float32]{b: false}
	}
}

func safe_reciprocal(x float32) Opt[float32] {
	if x == 0 {
		return Opt[float32]{b: false}
	} else {
		return Opt[float32]{1 / x, true}
	}
}

func safe_stringthing(x float32) Opt[*string] {
	if x == 0 {
		return Opt[*string]{b: false}
	}
	m := "hello"
	return Opt[*string]{&m, true}
}
