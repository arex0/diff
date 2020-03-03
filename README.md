# diff
offers algorithms to perform operations required for synchronizing bytes.

Based on myestr diff, is backward compatible, and closures provide better room for expansion

## Usage

The following example compares two text1 and text2, then patch differences to text1.

```go
package main

import (
	"fmt"

	"github.com/arex0/diff"
)

var text1 = `Hamlet: Do you see yonder cloud that's almost in shape of a camel?
Polonius: By the mass, and 'tis like a camel, indeed.
Hamlet: Methinks it is like a weasel.
Polonius: It is backed like a weasel.
Hamlet: Or like a whale?
Polonius: Very like a whale.
-- Shakespeare`
var text2 = `Hamlet: Do you see the cloud over there that's almost the shape of a camel?
Polonius: By golly, it is like a camel, indeed.
Hamlet: I think it looks like a weasel.
Polonius: It is shaped like a weasel.
Hamlet: Or like a whale?
Polonius: It's totally like a whale.
-- Shakespeare`

var b1 = []byte(text1)
var b2 = []byte(text2)

func main() {
    diffs := diff.Get(b1, b2, diff.WithChecklines(true), diff.WithSemantic(true))
    patch := diffs.ToPatch()
    newText, err := diff.Patch(b1, patch)
    if err != nil{
        // log
    }
    // newText == text2
}
```

