package arbre

import (
	"strconv"
)

type Object interface {
	String() string
}

type Number struct {
	inner int
}

type Boolean struct {
	inner bool
}

type Bytestring struct {
	inner []byte
}

func (self Number) String() string {
	return strconv.Itoa(self.inner)
}

func (self Number) Add(b Object) Object {
	temp, ok := b.(Number)
	if ok {
		return Number{inner: self.inner + temp.inner}
	} else {
		return nil
	}
}

func (self Number) Subtract(b Object) Number {
	return Number{inner: self.inner - b.(Number).inner}
}

func (self Number) Multiply(b Object) Number {
	return Number{inner: self.inner * b.(Number).inner}
}

func (self Number) Divide(b Object) Number {
	return Number{inner: self.inner / b.(Number).inner}
}

func (self Number) Equals(b Object) Boolean {
	return Boolean{inner: self.inner == b.(Number).inner}
}

func (self Number) LessThan(b Object) Boolean {
	return Boolean{inner: self.inner < b.(Number).inner}
}

func (self Number) GreaterThan(b Object) Boolean {
	return Boolean{inner: self.inner > b.(Number).inner}
}

func (self Boolean) String() string {
	return strconv.FormatBool(self.inner)
}

func (self Boolean) And(b Object) Boolean {
	return Boolean{inner: self.inner && b.(Boolean).inner}
}

func (self Boolean) Or(b Object) Boolean {
	return Boolean{inner: self.inner || b.(Boolean).inner}
}

func (self Boolean) Not() Boolean {
	return Boolean{inner: !self.inner}
}

func (self Boolean) Equals(b Object) Boolean {
	return Boolean{inner: self.inner == b.(Boolean).inner}
}

func (self Boolean) If(a Block, b Block) Block {
	if self.inner {
		return a
	} else {
		return b
	}
}

func (self Bytestring) String() string {
	return string(self.inner)
}
