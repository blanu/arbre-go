package arbre

import (
	"fmt"
)

type Output interface {
	Put(Value)
}

type Stdout struct {
}

func (self Stdout) Put(val Value) {
	fmt.Println(val.String())
}
