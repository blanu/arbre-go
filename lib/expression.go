package arbre

import (
	"fmt"
)

type Value interface {
	fmt.Stringer
}

type Expression interface {
}

type Block struct {
	temp int
}

type Failure struct {
	inner error
}

func (self Failure) String() string {
	return self.inner.Error()
}

type Call struct {
	receiver Expression
	method   string
	args     []Expression
}
