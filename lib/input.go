package arbre

import (
	"bufio"
	"errors"
	"os"
)

type Input interface {
	Get() Value
}

type Stdin struct {
}

type Start struct {
	first bool
}

func (self Stdin) Get() (Value, error) {
	var reader = bufio.NewReader(os.Stdin)
	var b, err = reader.ReadByte()
	if err != nil {
		return Failure{inner: err}, err
	} else {
		var bytes []byte = make([]byte, 1)
		bytes[0] = b
		return Bytestring{inner: bytes}, nil
	}
}

func (self Start) Get() (Value, error) {
	if self.first {
		var args = os.Args
		if len(args) > 0 {
			var arg = args[0]
			self.first = false
			return Bytestring{inner: []byte(arg)}, nil
		} else {
			self.first = false
			return Bytestring{inner: []byte("")}, nil
		}
	} else {
		var err error = errors.New("No more input")
		return Failure{inner: err}, err
	}
}
