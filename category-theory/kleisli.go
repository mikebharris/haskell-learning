package main

import "math"

type Opt[T any] struct {
	v T
	b bool
}

func idk[A any](a A) []A {
	return []A{a}
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

func fishopt[A any, B any, C any](f func(A) []B, g func(B) []C) func(A) []C {
	return func(a A) []C {
		o1 := f(a)
		if len(o1) == 0 {
			return []C{}
		}
		return g(o1[0])
	}
}

func safe_root(x float32) []float32 {
	if x >= 0 {
		return []float32{float32(math.Sqrt(float64(x)))}
	} else {
		return []float32{}
	}
}

func safe_reciprocal(x float32) []float32 {
	if x == 0 {
		return []float32{}
	} else {
		return []float32{1 / x}
	}
}

func safe_stringthing(x float32) []*string {
	if x == 0 {
		return []*string{}
	}
	m := "hello"
	return []*string{&m}
}
