package diff_test

import (
	"testing"

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

var patch = diff.Get(b1, b2).ToPatch()

func TestDiff(t *testing.T) {
	diffs := diff.Get(b1, b2, diff.WithChecklines(true), diff.WithSemantic(true))
	patch := diffs.ToPatch()
	newText, _ := diff.Patch(b1, patch)
	println(string(patch))
	println(string(newText))
	for i := 0; i < 100; i++ {
		newText, _ := diff.Patch(b1, diff.Get(b1, b2, diff.WithChecklines(true), diff.WithSemantic(true)).ToPatch())
		if string(newText) != text2 {
			t.Error(string(newText))
			t.Error(text2)
			break
		}
	}
}

func BenchmarkGetPatch(b *testing.B) {
	for i := 0; i < b.N; i++ {
		diff.Get(b1, b2, diff.WithChecklines(true), diff.WithSemantic(true)).ToPatch()
	}
}
func BenchmarkPatch(b *testing.B) {
	for i := 0; i < b.N; i++ {
		diff.Patch(b1, patch)
	}
}
