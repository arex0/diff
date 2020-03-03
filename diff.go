package diff

import (
	"bytes"
	"errors"
	"regexp"
	"time"

	b "github.com/arex0/go-ext/bytes"
)

// Option export to user for easy expansion.
type Option func(*options)
type options struct {
	deadline   time.Time
	editcost   int
	checklines bool
	semantic   bool
}

// WithDeadline set last time when the diff should be complete by.
func WithDeadline(deadline time.Time) Option {
	return func(o *options) { o.deadline = deadline }
}

// WithEditcost expanding the length of a diff by editcost(defalut is 4)(16: size of struct diff, 4: size of patch token), then that optimisation will reduce the total costs.
func WithEditcost(editcost int) Option {
	return func(o *options) { o.editcost = editcost }
}

// WithChecklines is a speedup flag, if false, then don't run a ine-level diff first to identify the changed areas, if true, then run a faster slightly less optimal diff.
func WithChecklines(checklines bool) Option {
	return func(o *options) { o.checklines = checklines }
}

// WithSemantic is a flag, default is false, if true make Diffs is to be human-readable.
func WithSemantic(semantic bool) Option {
	return func(o *options) { o.semantic = semantic }
}

// Get return difference of two []byte, using [ Myer's diff algorithm ](https://neil.fraser.name/writing/diff/myestr.pdf).
// Export to help users use extended options.
func Get(b1, b2 []byte, opts ...Option) Diffs {
	// set default options: no deadline, do not checklines.
	options := options{
		deadline:   time.Time{},
		editcost:   4,
		checklines: true,
		semantic:   false,
	}
	// set options with user opts.
	for _, set := range opts {
		set(&options)
	}
	// call implementation.
	diffs := get(b1, b2, options.deadline, options.checklines)
	if options.semantic {
		diffs = cleanupSemantic(diffs)
	}
	return cleanupEfficiency(diffs, options.editcost)
}
func get(b1, b2 []byte, deadline time.Time, checklines bool) Diffs {
	var diffs Diffs
	// check for equality (speedup).
	if bytes.Equal(b1, b2) {
		if len(b1) == 0 {
			return nil
		}
		return Diffs{Diff{op: Equals, bs: b1}}
	}
	// trim of common prefix and suffix (speedup).
	var commonprefix, commonsuffix []byte
	commonlength := b.CommonPrefixLength(b1, b2)
	if commonlength != 0 {
		commonprefix = b1[0:commonlength]
		b1 = b1[commonlength:]
		b2 = b2[commonlength:]
	}
	commonlength = b.CommonSuffixLength(b1, b2)
	if commonlength != 0 {
		commonsuffix = b1[len(b1)-commonlength:]
		b1 = b1[0 : len(b1)-commonlength]
		b2 = b2[0 : len(b2)-commonlength]
	}
	// compute middle block diff.
	diffs = compute(b1, b2, deadline, checklines)
	// restore the prefix and suffx.
	if commonprefix != nil {
		diffs.unshift(Diff{op: Equals, bs: commonprefix})
	}
	if commonsuffix != nil {
		diffs.push(Diff{op: Equals, bs: commonsuffix})
	}
	// merge adjacent differences of the same type.
	diffs = cleanupMerge(diffs)
	return diffs
}
func compute(b1, b2 []byte, deadline time.Time, checklines bool) Diffs {
	// just add or delete a continuous piece of bytes (speedup).
	if len(b1) == 0 {
		return Diffs{Diff{op: Insert, bs: b2}}
	}
	if len(b2) == 0 {
		return Diffs{Diff{op: Delete, bs: b1}}
	}
	// parted changed bytes.
	var long, short []byte
	if len(b1) > len(b2) {
		long = b1
		short = b2
	} else {
		long = b2
		short = b1
	}
	if i := bytes.Index(long, short); i != -1 {
		var op Operation
		if len(b1) > len(b2) {
			op = Delete
		} else {
			op = Insert
		}
		return Diffs{Diff{op: op, bs: long[0:i]}, Diff{op: Equals, bs: short}, Diff{op: op, bs: long[i+len(short):]}}
	}
	// Binary search:
	if len(short) == 1 { // ending search.
		return Diffs{Diff{op: Delete, bs: b1}, Diff{op: Insert, bs: b2}}
	}
	var hm [][]byte
	if len(long) >= 4 && len(short)*2 >= len(long) {
		I := func(long, short []byte, i int) [][]byte {
			var bestCommonA, bestCommonB, bestlongA, bestlongB, bestshortA, bestshortB []byte
			bestCommonLen := 0
			seed := long[i : i+len(long)/4]
			for j := bytes.Index(short, seed); j != -1; j = b.IndexFrom(short, seed, j+1) {
				prefixLength := b.CommonPrefixLength(long[i:], short[j:])
				suffixLength := b.CommonSuffixLength(long[0:i], short[0:j])
				if bestCommonLen < suffixLength+prefixLength {
					bestCommonA = short[j-suffixLength : j]
					bestCommonB = short[j : j+prefixLength]
					bestCommonLen = len(bestCommonA) + len(bestCommonB)
					bestlongA = long[0 : i-suffixLength]
					bestlongB = long[i+prefixLength:]
					bestshortA = short[0 : j-suffixLength]
					bestshortB = short[j+prefixLength:]
				}
			}
			if bestCommonLen*2 < len(long) {
				return nil
			}
			return [][]byte{
				bestlongA,
				bestlongB,
				bestshortA,
				bestshortB,
				append(bestCommonA, bestCommonB...),
			}
		}
		hm1 := I(long, short, (len(long)+3)/4) // first check if the second quarter is the seed for a half-match.
		hm2 := I(long, short, (len(long)+1)/2) // check again based on the third quarter.
		if hm1 != nil || hm2 != nil {
			if hm1 == nil {
				hm = hm2
			} else if hm2 == nil {
				hm = hm1
			} else if len(hm1[4]) > len(hm2[4]) {
				hm = hm1
			} else {
				hm = hm2
			}
			if len(b1) <= len(b2) {
				hm[0], hm[2] = hm[2], hm[0]
				hm[1], hm[3] = hm[3], hm[1]
			}
		}
	}
	if hm != nil {
		return get(hm[0], hm[2], deadline, checklines).concat(Diffs{Diff{op: Equals, bs: hm[4]}}).concat(get(hm[1], hm[3], deadline, checklines))
	}

	// Line match(text mode):
	if checklines && len(short) > 0x80 {
		// lines match
		lineArray := []string{""}
		lineHash := map[string]int{}
		munge := func(text []byte, maxLines int) []byte {
			lineStart := 0
			lineEnd := -1
			var h []rune
			lenLineArray := len(lineArray)
			for lineEnd < len(text)-1 {
				lineEnd = b.IndexFrom(text, []byte{10} /*\n*/, lineStart)
				if lineEnd == -1 {
					lineEnd = len(text) - 1
				}
				line := string(text[lineStart : lineEnd+1])
				lineStart = lineEnd + 1

				if lineValue, ok := lineHash[line]; ok {
					h = append(h, rune(lineValue))
				} else {
					if lenLineArray == maxLines {
						// bail out at 65535 because rune 65536 == char 0.
						line = string(text[lineStart])
						lineEnd = len(text)
					}
					h = append(h, rune(lenLineArray))
					lineHash[line] = lenLineArray
					lineArray = append(lineArray, line)
					lenLineArray++
				}
			}
			return b.FromRunes(h)
		}
		// scan the text on a line-by-line basis first.
		diffs := get(munge(b1, 40000), munge(b2, 65535), deadline, false)

		// convert the diff back to original text.
		hydrated := make(Diffs, 0, len(diffs))
		for _, diff := range diffs {
			var text []byte
			for _, r := range diff.bs {
				text = append(text, lineArray[r]...)
			}
			diff.bs = text
			hydrated.push(diff)
		}
		diffs = hydrated

		// eliminate freak matches (e.g. blank lines).
		diffs = cleanupSemantic(diffs)

		// rediff any replacement blocks, this time character-by-character.
		// add a dummy entry at the end.
		diffs.push(Diff{op: Equals, bs: nil})
		pointer := 0
		countDelete := 0
		countInsert := 0
		var bytesDelete, bytesInsert []byte
		for pointer < len(diffs) {
			switch diffs[pointer].op {
			case Insert:
				countInsert++
				bytesInsert = append(bytesInsert, diffs[pointer].bs...)
			case Delete:
				countDelete++
				bytesDelete = append(bytesDelete, diffs[pointer].bs...)
			case Equals:
				if countDelete >= 1 && countInsert >= 1 {
					diffs.splice(pointer-countDelete-countInsert, countDelete+countInsert)
					pointer -= (countDelete + countInsert)
					d := get(bytesDelete, bytesInsert, deadline, false)
					for j := len(d) - 1; j >= 0; j-- {
						diffs.splice(pointer, 0, d[j])
					}
					pointer += len(d)
				}
				countInsert = 0
				countDelete = 0
				bytesDelete = nil
				bytesInsert = nil
			}
			pointer++
		}
		diffs.pop(1)
		return diffs
	}

	// Byte match:
	// cache the size to prevent multiple calls.
	lenB1, lenB2 := len(b1), len(b2)
	dMax := (lenB1 + lenB2 + 1) / 2
	vOffset, vLength := dMax, 2*dMax
	v1, v2 := make([]int, vLength), make([]int, vLength)
	for i := 0; i < vLength; i++ {
		v1[i] = -1
		v2[i] = -1
	}
	v1[vOffset+1] = 0
	v2[vOffset+1] = 0
	delta := lenB1 - lenB2
	// if the total number of characters is odd, then the front path will collide with the reverse path.
	front := (delta%2 != 0)
	// offsets for start and end of k loop.
	// prevents mapping of space beyond the grid.
	k1start, k1end, k2start, k2end := 0, 0, 0, 0
	for d := 0; d < dMax; d++ {
		// bail out if deadline is reached.
		if !deadline.IsZero() && time.Now().After(deadline) {
			break
		}
		// walk the front path one step.
		for k1 := -d + k1start; k1 <= d-k1end; k1 += 2 {
			k1Offset := vOffset + k1
			var x1 int
			if k1 == -d || (k1 != d && v1[k1Offset-1] < v1[k1Offset+1]) {
				x1 = v1[k1Offset+1]
			} else {
				x1 = v1[k1Offset-1] + 1
			}
			y1 := x1 - k1
			for x1 < lenB1 && y1 < lenB2 && b1[x1] == b2[y1] {
				x1++
				y1++
			}
			v1[k1Offset] = x1
			if x1 > lenB1 {
				k1end += 2 // ran off the right of the graph.
			} else if y1 > lenB2 {
				k1start += 2 // ran off the bottom of the graph.
			} else if front {
				k2Offset := vOffset + delta - k1
				if k2Offset >= 0 && k2Offset < vLength && v2[k2Offset] != -1 {
					// mirror x2 onto top-left coordinate system.
					x2 := lenB1 - v2[k2Offset]
					if x1 >= x2 {
						return get(b1[0:x1], b2[0:y1], deadline, false).concat(get(b1[x1:], b2[y1:], deadline, false))
					}
				}
			}
		}
		// walk the reverse path one step.
		for k2 := -d + k2start; k2 <= d-k2end; k2 += 2 {
			k2Offset := vOffset + k2
			var x2 int
			if k2 == -d || (k2 != d && v2[k2Offset-1] < v2[k2Offset+1]) {
				x2 = v2[k2Offset+1]
			} else {
				x2 = v2[k2Offset-1] + 1
			}
			y2 := x2 - k2
			for x2 < lenB1 && y2 < lenB2 && b1[lenB1-x2-1] == b2[lenB2-y2-1] {
				x2++
				y2++
			}
			v2[k2Offset] = x2
			if x2 > lenB1 {
				k2end += 2 // ran off the left of the graph.
			} else if y2 > lenB2 {
				k2start += 2 // ran off the top of the graph.
			} else if !front {
				k1Offset := vOffset + delta - k2
				if k1Offset >= 0 && k1Offset < vLength && v1[k1Offset] != -1 {
					x1 := v1[k1Offset]
					y1 := vOffset + x1 - k1Offset
					// mirror x2 onto top-left coordinate system.
					x2 = lenB1 - v2[k2Offset]
					if x1 >= x2 {
						return get(b1[0:x1], b2[0:y1], deadline, false).concat(get(b1[x1:], b2[y1:], deadline, false))
					}
				}
			}
		}
	}
	return Diffs{Diff{op: Delete, bs: b1}, Diff{op: Insert, bs: b2}}
}

// ToPatch prase a Diffs to []byte for transfer, than call Patch(src,patch) to sync difference.
func (diffs Diffs) ToPatch() []byte {
	if len(diffs) == 0 {
		return []byte{'[', ']'}
	}
	patch := []byte{'['}
	for _, diff := range diffs {
		switch diff.op {
		case Equals:
			if len(diff.bs) >= 0x100 {
				patch = append(append(patch, '0'), b.Itoa(len(diff.bs), 10)...)
			} else {
				patch = append(patch, '1', byte(len(diff.bs)))
			}
		case Delete:
			if len(diff.bs) >= 0x100 {
				patch = append(append(patch, '2'), b.Itoa(len(diff.bs), 10)...)
			} else {
				patch = append(patch, '3', byte(len(diff.bs)))
			}
		case Insert:
			patch = append(append(patch, '4'), b.Quote(diff.bs, '"')...)
		}
		patch = append(patch, ',')
	}
	patch[len(patch)-1] = ']'
	return patch
}

// Patch update src with a patch, return a newer []byte and nil or nil and a error.
func Patch(src, patch []byte) ([]byte, error) {
	// check and trim quote.
	if patch[0] != '[' || patch[len(patch)-1] != ']' {
		return nil, errors.New("illegal patch")
	}
	patch = patch[1 : len(patch)-1]
	t := uint(0)          // source bytes index.
	m := uint(len(patch)) // cache for mulipie call
	buffer := []byte{}    // patch result buffer.
	for i := uint(0); i < m; i++ {
		// deal with a diff by type.
		switch patch[i] {
		case '0':
			i++
			// count equal byte size.
			x := uint(0)
			for j := i; j < m && patch[j] >= '0' && patch[j] <= '9'; j++ {
				x = x*10 + uint(patch[j]-'0')
				i++
			}
			// ok, then append to buffer, and src index move on.
			buffer = append(buffer, src[t:t+x]...)
			t += x
		case '1':
			i++
			// count equal byte size.
			x := uint(patch[i])
			i++
			// equal part append to buffer, and src index move on.
			buffer = append(buffer, src[t:t+x]...)
			t += x
		case '2':
			i++
			// count delete byte size.
			x := uint(0)
			for j := i; j < m && patch[j] >= '0' && patch[j] <= '9'; j++ {
				x = x*10 + uint(patch[j]-'0')
				i++
			}
			// delete do not need append, src index move on.
			t += x
		case '3':
			i++
			// delete do not need append, src index move on.
			t += uint(patch[i])
			i++
		case '4':
			i++
			if bs, end := b.ReadQuote(patch, '"', i); end > i {
				buffer = append(buffer, bs...)
				i = end + 1
			}
		default:
			return nil, errors.New("illegal patch")
		}
		// all diff divided by ',', check if is illegal.
		if i < m && patch[i] != ',' {
			return nil, errors.New("illegal patch")
		}
	}
	return buffer, nil
}

// CleanupMerge merge like edit sections.
func cleanupMerge(diffs Diffs) Diffs {
	diffs.push(Diff{op: Equals, bs: nil})
	pointer := 0
	countDelete := 0
	countInsert := 0
	commonlength := 0
	var bytesDelete []byte
	var bytesInsert []byte
	for pointer < len(diffs) {
		switch diffs[pointer].op {
		case Insert:
			countInsert++
			bytesInsert = append(bytesInsert, diffs[pointer].bs...)
			pointer++
		case Delete:
			countDelete++
			bytesDelete = append(bytesDelete, diffs[pointer].bs...)
			pointer++
		case Equals:
			// upon reaching an equality, check for prior redundancies.
			if countDelete+countInsert > 1 {
				if countDelete != 0 && countInsert != 0 {
					// factor out any common prefixies.
					commonlength = b.CommonPrefixLength(bytesInsert, bytesDelete)
					if commonlength != 0 {
						x := pointer - countDelete - countInsert
						if x > 0 && diffs[x-1].op == Equals {
							diffs[x-1].bs = append(diffs[x-1].bs, bytesInsert[0:commonlength]...)
						} else {
							diffs.unshift(Diff{op: Equals, bs: bytesInsert[0:commonlength]})
							pointer++
						}
						bytesInsert = bytesInsert[commonlength:]
						bytesDelete = bytesDelete[commonlength:]
					}
					// factor out any common suffixies.
					commonlength = b.CommonSuffixLength(bytesInsert, bytesDelete)
					if commonlength != 0 {
						insertIndex := len(bytesInsert) - commonlength
						deleteIndex := len(bytesDelete) - commonlength
						diffs[pointer].bs = append(bytesInsert[insertIndex:], diffs[pointer].bs...)
						bytesInsert = bytesInsert[0:insertIndex]
						bytesDelete = bytesDelete[0:deleteIndex]
					}
				}
				// delete the offending records and add the merged ones.
				if countDelete == 0 && countInsert == 0 {
					diffs.splice(pointer-countDelete-countInsert, countDelete+countInsert)
				} else if countDelete == 0 {
					diffs.splice(pointer-countInsert, countDelete+countInsert, Diff{op: Insert, bs: bytesInsert})
					pointer++
				} else if countInsert == 0 {
					diffs.splice(pointer-countDelete, countDelete+countInsert, Diff{op: Delete, bs: bytesDelete})
					pointer++
				} else {
					diffs.splice(pointer-countDelete-countInsert, countDelete+countInsert, Diff{op: Delete, bs: bytesDelete}, Diff{op: Insert, bs: bytesInsert})
					pointer += 2
				}
				pointer -= (countDelete + countInsert - 1)
			} else if pointer != 0 && diffs[pointer-1].op == Equals {
				// merge this equality with the previous one.
				diffs[pointer-1].bs = append(diffs[pointer-1].bs, diffs[pointer].bs...)
				diffs.splice(pointer, 1)
			} else {
				pointer++
			}
			countInsert = 0
			countDelete = 0
			bytesDelete = nil
			bytesInsert = nil
		}
	}
	if len(diffs[len(diffs)-1].bs) == 0 {
		diffs.pop(1)
	}
	changes := false
	pointer = 1
	// Intentionally ignore the first and last element (don't need checking).
	for pointer < (len(diffs) - 1) {
		if diffs[pointer-1].op == Equals && diffs[pointer+1].op == Equals {
			if bytes.HasSuffix(diffs[pointer].bs, diffs[pointer-1].bs) {
				// This is a single edit surrounded by equalities.
				diffs[pointer].bs = append(diffs[pointer-1].bs, diffs[pointer].bs[0:len(diffs[pointer].bs)-len(diffs[pointer-1].bs)]...)
				diffs[pointer+1].bs = append(diffs[pointer-1].bs, diffs[pointer+1].bs...) //todo
				diffs.splice(pointer-1, 1)
				changes = true
			} else if bytes.HasPrefix(diffs[pointer].bs, diffs[pointer+1].bs) {
				// Shift the edit over the next equality.
				diffs[pointer-1].bs = append(diffs[pointer-1].bs, diffs[pointer+1].bs...)
				diffs[pointer].bs = append(diffs[pointer].bs[len(diffs[pointer+1].bs):], diffs[pointer+1].bs...) //todo
				diffs.splice(pointer+1, 1)
				changes = true
			}
		}
		pointer++
	}
	if changes {
		diffs = cleanupMerge(diffs)
	}
	return diffs
}

// CleanupSemantic make Diffs is to be human-readable.
func cleanupSemantic(diffs Diffs) Diffs {
	changes := false
	var equalities stack
	var lastEquality []byte
	pointer := 0
	lenInsertions1, lenDeletions1 := 0, 0
	lenInsertions2, lenDeletions2 := 0, 0
	for pointer < len(diffs) {
		if diffs[pointer].op == Equals {
			equalities.push(pointer)
			lenInsertions1 = lenInsertions2
			lenDeletions1 = lenDeletions2
			lenInsertions2 = 0
			lenDeletions2 = 0
			lastEquality = diffs[pointer].bs
		} else {
			if diffs[pointer].op == Insert {
				lenInsertions2 += len(diffs[pointer].bs)
			} else {
				lenDeletions2 += len(diffs[pointer].bs)
			}
			if lastEquality != nil && (len(lastEquality) <= max(lenInsertions1, lenDeletions1)) && (len(lastEquality) <= max(lenInsertions2, lenDeletions2)) {
				diffs.splice(equalities.peek(), 0, Diff{op: Delete, bs: lastEquality})
				diffs[equalities.peek()+1].op = Insert
				equalities.pop()
				equalities.pop()
				if equalities.len() > 0 {
					pointer = equalities.peek()
				} else {
					pointer = -1
				}
				lenInsertions1 = 0
				lenDeletions1 = 0
				lenInsertions2 = 0
				lenDeletions2 = 0
				lastEquality = nil
				changes = true
			}
		}
		pointer++
	}
	// normalize the diff.
	if changes {
		diffs = cleanupMerge(diffs)
	}
	semanticScore := func(one, two []byte) int {
		if len(one) == 0 || len(two) == 0 {
			return 6
		}
		char1 := one[len(one)-1]
		char2 := two[0]
		nonAlphaNumeric1 := b.IsLetterOrDigit(char1)
		nonAlphaNumeric2 := b.IsLetterOrDigit(char2)
		whitespace1 := nonAlphaNumeric1 && b.IsWhiteSpace(char1)
		whitespace2 := nonAlphaNumeric2 && b.IsWhiteSpace(char2)
		lineBreak1 := whitespace1 && b.IsControl(char1)
		lineBreak2 := whitespace2 && b.IsControl(char2)
		blankLine1 := lineBreak1 && blanklineEndRegex.Match(one)
		blankLine2 := lineBreak2 && blanklineStartRegex.Match(two)
		if blankLine1 || blankLine2 {
			return 5
		} else if lineBreak1 || lineBreak2 {
			return 4
		} else if nonAlphaNumeric1 && !whitespace1 && whitespace2 {
			return 3
		} else if whitespace1 || whitespace2 {
			return 2
		} else if nonAlphaNumeric1 || nonAlphaNumeric2 {
			return 1
		}
		return 0
	}
	// look for single edits surrounded on both sides by equalities
	pointer = 1
	for pointer < len(diffs)-1 {
		if diffs[pointer-1].op == Equals && diffs[pointer+1].op == Equals {
			equality1 := diffs[pointer-1].bs
			edit := diffs[pointer].bs
			equality2 := diffs[pointer+1].bs
			// shift the edit as far left as possible.
			commonOffset := b.CommonSuffixLength(equality1, edit)
			if commonOffset > 0 {
				commonString := edit[len(edit)-commonOffset:]
				equality1 = equality1[0 : len(equality1)-commonOffset]
				edit = append(commonString, edit[0:len(edit)-commonOffset]...)
				equality2 = append(commonString, equality2...)
			}
			// step character by character right,
			bestEquality1 := equality1
			bestEdit := edit
			bestEquality2 := equality2
			bestScore := semanticScore(equality1, edit) + semanticScore(edit, equality2)
			for len(edit) != 0 && len(equality2) != 0 {
				if len(equality2) < 1 || edit[0] != equality2[0] {
					break
				}
				equality1 = append(equality1, edit[0])
				edit = append(edit[1:], equality2[0])
				equality2 = equality2[1:]
				score := semanticScore(equality1, edit) + semanticScore(edit, equality2)
				if score >= bestScore {
					bestScore = score
					bestEquality1 = equality1
					bestEdit = edit
					bestEquality2 = equality2
				}
			}
			if !bytes.Equal(diffs[pointer-1].bs, bestEquality1) {
				if len(bestEquality1) != 0 {
					diffs[pointer-1].bs = bestEquality1
				} else {
					diffs.splice(pointer-1, 1)
					pointer--
				}
				diffs[pointer].bs = bestEdit
				if len(bestEquality2) != 0 {
					diffs[pointer+1].bs = bestEquality2
				} else {
					diffs.splice(pointer+1, 1)
					pointer--
				}
			}
		}
		pointer++
	}
	// find any overlaps between deletions and insertions.
	pointer = 1
	for pointer < len(diffs) {
		if diffs[pointer-1].op == Delete && diffs[pointer].op == Insert {
			deletion := diffs[pointer-1].bs
			insertion := diffs[pointer].bs
			lenOverlap1 := b.CommonOverlapLength(deletion, insertion)
			lenOverlap2 := b.CommonOverlapLength(insertion, deletion)
			if lenOverlap1 >= lenOverlap2 {
				if lenOverlap1*2 >= len(deletion) || lenOverlap1*2 >= len(insertion) {
					diffs.splice(pointer, 0, Diff{op: Equals, bs: insertion[0:lenOverlap1]})
					diffs[pointer-1].bs = deletion[0 : len(deletion)-lenOverlap1]
					diffs[pointer+1].bs = insertion[lenOverlap1:]
					pointer++
				}
			} else {
				if lenOverlap2*2 >= len(deletion) || lenOverlap2*2 >= len(insertion) {
					diffs.splice(pointer, 0, Diff{op: Equals, bs: deletion[0:lenOverlap2]})
					diffs[pointer-1].op = Insert
					diffs[pointer-1].bs = insertion[0 : len(insertion)-lenOverlap2]
					diffs[pointer+1].op = Delete
					diffs[pointer+1].bs = deletion[lenOverlap2:]
					pointer++
				}
			}
			pointer++
		}
		pointer++
	}
	return diffs
}

// CleanupEfficiency means if expanding the length of a diff by editcost(16: size of struct diff, 4: size of patch token), then that optimisation will reduce the total costs.
func cleanupEfficiency(diffs Diffs, editcost int) Diffs {
	changes := false
	var equalities stack
	var lastEquality []byte
	pointer := 0 // Index of current position.
	preIns := false
	preDel := false
	postIns := false
	postDel := false
	for pointer < len(diffs) {
		if diffs[pointer].op == Equals { // Equality found.
			if len(diffs[pointer].bs) < editcost && (postIns || postDel) {
				equalities.push(pointer)
				preIns = postIns
				preDel = postDel
				lastEquality = diffs[pointer].bs
			} else {
				equalities.clear()
				lastEquality = nil
			}
			postIns = false
			postDel = false
		} else { // An insertion or deletion.
			if diffs[pointer].op == Delete {
				postDel = true
			} else {
				postIns = true
			}
			sumPres := 0
			if preIns {
				sumPres++
			}
			if preDel {
				sumPres++
			}
			if postIns {
				sumPres++
			}
			if postDel {
				sumPres++
			}
			if len(lastEquality) != 0 &&
				((preIns && preDel && postIns && postDel) ||
					((len(lastEquality) < editcost/2) && sumPres == 3)) {

				insPoint := equalities.peek()
				diffs.splice(insPoint, 0, Diff{op: Delete, bs: lastEquality})
				diffs[insPoint+1].op = Insert
				equalities.pop()
				lastEquality = nil

				if preIns && preDel {
					postIns = true
					postDel = true
					equalities.clear()
				} else {
					if equalities.len() > 0 {
						equalities.pop()
					}
					if equalities.len() > 0 {
						pointer = equalities.peek()
					} else {
						pointer = -1
					}
					postIns = false
					postDel = false
				}
				changes = true
			}
		}
		pointer++
	}

	if changes {
		diffs = cleanupMerge(diffs)
	}
	return diffs
}

// Operation explains diff type.
type Operation int

// diff include three type operation.
const (
	Equals Operation = iota
	Delete
	Insert
)

// Diff contains diff type and bytes.
type Diff struct {
	op Operation
	bs []byte
}

// Diffs is []Diff define to call func(Diffs){}.
type Diffs []Diff

func (diffs Diffs) concat(diffs2 Diffs) Diffs { return append(diffs, diffs2...) }
func (diffs *Diffs) unshift(diff Diff)        { diffs.splice(0, 0, diff) }
func (diffs *Diffs) push(diff Diff)           { *diffs = append(*diffs, diff) }
func (diffs *Diffs) pop(n int)                { *diffs = (*diffs)[0 : len(*diffs)-n] }
func (diffs *Diffs) splice(index int, amount int, appends ...Diff) {
	tDiffs := *diffs
	lenAppends := len(appends)
	if lenAppends == amount {
		copy(tDiffs[index:], appends)
	} else if lenAppends < amount {
		copy(tDiffs[index:], appends)
		copy(tDiffs[index+lenAppends:], tDiffs[index+amount:])
		tDiffs = tDiffs[0 : len(tDiffs)-amount+lenAppends]
	} else {
		tDiffs = append(tDiffs, make(Diffs, lenAppends-amount)...)
		copy(tDiffs[index+lenAppends:], tDiffs[index+amount:])
		copy(tDiffs[index:], appends)
	}
	*diffs = tDiffs
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

type (
	stack struct {
		top    *node
		length int
	}
	node struct {
		prev  *node
		value int
	}
)

func (s *stack) len() int {
	return s.length
}
func (s *stack) peek() int {
	return s.top.value
}
func (s *stack) pop() {
	if s.length != 0 {
		s.top = s.top.prev
		s.length--
	}
}
func (s *stack) push(value int) {
	s.top = &node{s.top, value}
	s.length++
}
func (s *stack) clear() {
	s.top = nil
	s.length = 0
}

var (
	blanklineEndRegex   = regexp.MustCompile(`\n\r?\n$`)
	blanklineStartRegex = regexp.MustCompile(`^\r?\n\r?\n`)
)
