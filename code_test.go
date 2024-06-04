// Copyright 2023 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ordered

import (
	"bytes"
	"fmt"
	"math"
	"reflect"
	"strings"
	"testing"
)

const (
	i64 = int64(0)
	u64 = uint64(0)
	f64 = float64(0.0)
	f32 = float32(0.0)
)

var encodingTests = []struct {
	in  any
	out string
}{
	{i64 + 0, "3000"},
	{i64 + 0x123456, "32123456"},
	{i64 - 1, "2fff"},
	{Rev(i64 + 0), "cfff"},
	{Rev(i64 + 1), "cffe"},
	{"abc", "016162630000"},
	{"abc\x00def", "0161626300ff6465660000"},
}

func TestEncoding(t *testing.T) {
	for _, tt := range encodingTests {
		out := Encode(tt.in)
		hex := fmt.Sprintf("%x", out)
		if hex != tt.out {
			t.Errorf("Encode(%v) = %q, want %q", format(tt.in), hex, tt.out)
			continue
		}
		list, err := DecodeAny(out)
		if err != nil || len(list) != 1 || list[0] != tt.in {
			t.Errorf("DecodeAny(%s) = %v, %v, want %v, nil", hex, format(list...), err, format(tt.in))
		}
	}
}

func format(list ...any) string {
	var s []string
	for _, x := range list {
		f := fmt.Sprintf("%#v", x)
		if !strings.Contains(f, "Reverse") {
			f = fmt.Sprintf("%T(%s)", x, f)
		}
		s = append(s, f)
	}
	return strings.Join(s, ", ")
}

var values = []any{
	Rev(Inf),

	"",
	"a",
	"a\x00",
	"a\x00bc",
	"ab",
	"abc",
	"ac",
	"a\xff",
	"b",

	f32 - float32(math.Inf(+1)),
	f32 - 1e9,
	f32 - 1e3,
	f32 - 1,
	f32 - 1e-3,
	f32 - 1e-9,
	f32 + 1e-9,
	f32 + 1e-3,
	f32 + 1,
	f32 + 1e3,
	f32 + 1e9,
	f32 + float32(math.Inf(+1)),

	f64 - math.Inf(+1),
	f64 - 1e9,
	f64 - 1e3,
	f64 - 1,
	f64 - 1e-3,
	f64 - 1e-9,
	f64 + 1e-9,
	f64 + 1e-3,
	f64 + 1,
	f64 + 1e3,
	f64 + 1e9,
	f64 + math.Inf(+1),

	i64 + -0x8000000000000000,
	i64 - 0x7fffffffffffffff,
	i64 - 0x100000000000000,
	i64 - 0xffffffffffffff,
	i64 - 0x80000000000000,
	i64 - 0x1000000000000,
	i64 - 0xffffffffffff,
	i64 - 0x800000000000,
	i64 - 0x10000000000,
	i64 - 0xffffffffff,
	i64 - 0x8000000000,
	i64 - 0x100000000,
	i64 - 0xffffffff,
	i64 - 0x80000000,
	i64 - 0x1000000,
	i64 - 0xffffff,
	i64 - 0x800000,
	i64 - 0x10000,
	i64 - 0xffff,
	i64 - 0x8000,
	i64 - 0x100,
	i64 - 0xff,
	i64 - 0x80,
	i64 - 2,
	i64 - 1,

	i64 + 0,

	u64 + 1,
	i64 + 2,
	i64 + 0xfe,
	u64 + 0xff,

	i64 + 0x1234,
	u64 + 0x1235,
	u64 + 0xfffe,
	i64 + 0xffff,

	u64 + 0x123456,
	i64 + 0x123457,
	i64 + 0xfffffe,
	u64 + 0xffffff,

	i64 + 0x12345678,
	u64 + 0x12345679,
	u64 + 0xfffffffe,
	i64 + 0xffffffff,

	u64 + 0x123456789a,
	i64 + 0x123456789b,
	i64 + 0xfffffffffe,
	u64 + 0xffffffffff,

	i64 + 0x123456789abc,
	u64 + 0x123456789abd,
	u64 + 0xfffffffffffe,
	i64 + 0xffffffffffff,

	u64 + 0x123456789abcde,
	i64 + 0x123456789abcdf,
	i64 + 0xfffffffffffffe,
	u64 + 0xffffffffffffff,

	i64 + 0x123456789abcdef0,
	u64 + 0x123456789abcdef1,
	u64 + 0x7ffffffffffffffe,
	i64 + 0x7fffffffffffffff,

	u64 + 0x8000000000000000,
	u64 + 0x89abcdef12345678,
	u64 + 0xffffffffffffffff,
}

func init() {
	for i := len(values) - 1; i > 0; i-- {
		values = append(values, RevAny(values[i]))
	}
	values = append(values, Inf)
}

func TestRoundTrip(t *testing.T) {
	check := func(list ...any) {
		enc := Encode(list...)
		out, err := DecodeAny(enc)
		if err != nil {
			t.Errorf("Encode(%s) = %x; DecodeAny: %v", format(list...), enc, err)
			return
		}
		for i, x := range out {
			if x, ok := x.(int64); ok && x >= 0 {
				if _, ok := list[i].(uint64); ok {
					out[i] = uint64(x)
				}
			}
			if x, ok := x.(Reverse[int64]); ok && x.v >= 0 {
				if _, ok := list[i].(Reverse[uint64]); ok {
					out[i] = Rev(uint64(x.v))
				}
			}
		}
		if !reflect.DeepEqual(out, list) {
			t.Errorf("Encode(%s) = %x; DecodeAny = %s", format(list...), enc, format(out...))
		}

		var ptrs []any
		for _, x := range list {
			ptrs = append(ptrs, reflect.New(reflect.TypeOf(x)).Interface())
		}
		err = Decode(enc, ptrs...)
		if err != nil {
			t.Errorf("Encode(%s) = %x; Decode: %v", format(list...), enc, err)
		}
		out = nil
		for _, x := range ptrs {
			out = append(out, reflect.ValueOf(x).Elem().Interface())
		}
		if !reflect.DeepEqual(out, list) {
			t.Errorf("Encode(%s) = %x; Decode = %s", format(list...), enc, format(out...))
		}

		err = Decode(append(enc, 0), ptrs...)
		if err != errExtra {
			t.Errorf("Encode(%s) = %x; Decode(enc+00): %v, want %v", format(list...), enc, err, errExtra)
		}

		ptrs = nil
		for _, x := range list {
			ptrs = append(ptrs, reflect.New(reflect.TypeOf(x)).Interface())
		}
		rest, err := DecodePrefix(append(enc, 0), ptrs...)
		if err != nil || !bytes.Equal(rest, []byte{0}) {
			t.Errorf("Encode(%s) = %x; DecodePrefix(enc+00) = %x, %v, want 00, nil", format(list...), enc, rest, err)
		}
		out = nil
		for _, x := range ptrs {
			out = append(out, reflect.ValueOf(x).Elem().Interface())
		}
		if !reflect.DeepEqual(out, list) {
			t.Errorf("Encode(%s) = %x; DecodePrefix(enc+00) = %s", format(list...), enc, format(out...))
		}
	}

	for _, v1 := range values {
		check(v1)
	}
	if t.Failed() {
		return
	}

	for _, v1 := range values {
		for _, v2 := range values {
			check(v1, v2)
		}
	}
	if t.Failed() || testing.Short() {
		return
	}

	for _, v1 := range values {
		for _, v2 := range values {
			for _, v3 := range values {
				check(v1, v2, v3)
			}
		}
	}
	if t.Failed() {
		return
	}
}

func TestOrdered(t *testing.T) {
	for i, v := range values {
		ev := Encode(v)
		for _, w := range values[i+1:] {
			ew := Encode(w)
			if bytes.Compare(ev, ew) >= 0 {
				t.Errorf("order inversion: %T(%#v) < %T(%#v) but\nEncode(%#v)	= %x\nEncode(%#v) = %x", v, v, w, w, v, ev, w, ew)
			}
		}
	}
}

var golden = []struct {
	x   any
	enc string
}{
	{"", "\x01\x00\x00"},
	{[]byte(nil), "\x01\x00\x00"},
	{[]byte(""), "\x01\x00\x00"},
	{"hello", "\x01hello\x00\x00"},
	{[]byte("hello"), "\x01hello\x00\x00"},
	{"hello\x00world", "\x01hello\x00\xffworld\x00\x00"},
	{[]byte("hello\x00world"), "\x01hello\x00\xffworld\x00\x00"},
	{0x12345, "\x32\x01\x23\x45"},
	{1, "\x30\x01"},
	{0, "\x30\x00"},
	{-1, "\x2f\xff"},
	{-0x12345, "\x2d\xfe\xdc\xbb"},
	{uintptr(0x12345), "\x32\x01\x23\x45"},
	{uint64(0xffffffffffffffff), "\x37\xff\xff\xff\xff\xff\xff\xff\xff"},
	{uint64(0x123456789abcdef0), "\x37\x12\x34\x56\x78\x9a\xbc\xde\xf0"},
	{uint32(0x12345678), "\x33\x12\x34\x56\x78"},
	{uint16(0x2345), "\x31\x23\x45"},
	{uint8(0x45), "\x30\x45"},
	{uint(0x12345), "\x32\x01\x23\x45"},
	{int64(0x7fffffffffffffff), "\x37\x7f\xff\xff\xff\xff\xff\xff\xff"},
	{int64(-0x7fffffffffffffff), "\x28\x80\x00\x00\x00\x00\x00\x00\x01"},
	{int64(-0x8000000000000000), "\x28\x80\x00\x00\x00\x00\x00\x00\x00"},
	{int64(0x123456789abcdef0), "\x37\x12\x34\x56\x78\x9a\xbc\xde\xf0"},
	{int64(-0x123456789abcdef0), "\x28\xed\xcb\xa9\x87\x65\x43\x21\x10"},
	{int32(0x12345678), "\x33\x12\x34\x56\x78"},
	{int32(-0x12345678), "\x2c\xed\xcb\xa9\x88"},
	{int16(0x2345), "\x31\x23\x45"},
	{int16(-0x2345), "\x2e\xdc\xbb"},
	{int8(0x45), "\x30\x45"},
	{int8(-0x45), "\x2f\xbb"},
	{int(0x12345), "\x32\x01\x23\x45"},
	{int(-0x12345), "\x2d\xfe\xdc\xbb"},
	{float32(math.NaN()), "\x02\x00\x00\x00\x00"},
	{float32(math.Inf(-1)), "\x02\x00\x7f\xff\xff"},
	{float32(-1048576), "\x02\x36\x7f\xff\xff"},
	{float32(-1.0625), "\x02\x40\x77\xff\xff"},
	{float32(-1), "\x02\x40\x7f\xff\xff"},
	{float32(0.0), "\x02\x80\x00\x00\x00"},
	{float32(+1), "\x02\xbf\x80\x00\x00"},
	{float32(+1.0625), "\x02\xbf\x88\x00\x00"},
	{float32(+1048576), "\x02\xc9\x80\x00\x00"},
	{float32(math.Inf(+1)), "\x02\xff\x80\x00\x00"},
	{float64(math.NaN()), "\x03\x00\x00\x00\x00\x00\x00\x00\x00"},
	{float64(math.Inf(-1)), "\x03\x00\x0f\xff\xff\xff\xff\xff\xff"},
	{float64(-1048576), "\x03\x3e\xcf\xff\xff\xff\xff\xff\xff"},
	{float64(-1), "\x03\x40\x0f\xff\xff\xff\xff\xff\xff"},
	{float64(0.0), "\x03\x80\x00\x00\x00\x00\x00\x00\x00"},
	{float64(+1), "\x03\xbf\xf0\x00\x00\x00\x00\x00\x00"},
	{float64(+1048576), "\x03\xc10\x00\x00\x00\x00\x00\x00"},
	{float64(math.Inf(+1)), "\x03\xff\xf0\x00\x00\x00\x00\x00\x00"},
	{Inf, "\xff"},
	{StringOrInfinity{S: "x"}, "\x01x\x00\x00"},
	{StringOrInfinity{}, "\x01\x00\x00"},
	{StringOrInfinity{Inf: true}, "\xff"},
	{Raw("hello\x00world"), "\x04\x0bhello\x00world"},
}

func TestGolden(t *testing.T) {
	for _, tt := range golden {
		enc := Encode(tt.x)
		var desc string
		switch reflect.TypeOf(tt.x).Kind() {
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
			reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
			desc = fmt.Sprintf("%#x", tt.x)
		case reflect.String, reflect.Slice:
			desc = fmt.Sprintf("%q", tt.x)
		default:
			desc = fmt.Sprint(tt.x)
		}
		if string(enc) != tt.enc {
			t.Errorf("Encode(%T(%s)) = %q (%x), want %q (%x)", tt.x, desc, enc, enc, tt.enc, tt.enc)
			continue
		}

		// Typed Decode.
		ptr := reflect.New(reflect.TypeOf(tt.x)).Interface()
		err := Decode(enc, ptr)
		if err != nil {
			t.Errorf("Decode(Encode(%s), %T): %v", desc, ptr, err)
			continue
		}
		if v := reflect.ValueOf(ptr).Elem().Interface(); !equal(v, tt.x) {
			t.Errorf("Decode(Encode(%s), %T) = %v, want %v", desc, ptr, format(v), format(tt.x))
			continue
		}

		if _, ok := tt.x.(Raw); !ok {
			// Reverse.
			rev := RevAny(tt.x)
			enc = Encode(rev)
			want := []byte(tt.enc)
			for i, x := range want {
				want[i] = ^x
			}
			if string(enc) != string(want) {
				t.Errorf("Encode(Rev(%T(%s))) = %q (%x), want %q (%x)", tt.x, desc, enc, enc, want, want)
				continue
			}

			// Typed Decode
			ptr = reflect.New(reflect.TypeOf(rev)).Interface()
			err = Decode(enc, ptr)
			if err != nil {
				t.Errorf("Decode(Encode(%s), %T): %v", desc, ptr, err)
				continue
			}
			if v := reflect.ValueOf(ptr).Elem().Interface(); !equal(v, rev) {
				t.Errorf("Decode(Encode(%s), %T) = %v, want %v", desc, ptr, format(v), format(rev))
				continue
			}
		}

		// Try all possible shorter encodings and check that they return errors.
		for i := 1; i < len(enc); i++ {
			short := enc[:i]
			_, err := DecodeAny(short)
			if err != errCorrupt {
				t.Errorf("DecodeAny(%q) = %v, want %v", short, err, errCorrupt)
			}
		}
	}
}

func equal(x, y any) bool {
	if reflect.DeepEqual(x, y) {
		return true
	}
	if reflect.TypeOf(x) != reflect.TypeOf(y) {
		return false
	}
	// []byte(nil) and []byte("") are fine
	if x, ok := x.([]byte); ok {
		y := y.([]byte)
		if len(x) == 0 && len(y) == 0 {
			return true
		}
	}
	if x, ok := x.(Reverse[[]byte]); ok {
		x := x.Value()
		y := y.(Reverse[[]byte]).Value()
		if len(x) == 0 && len(y) == 0 {
			return true
		}
	}
	// float32 NaNs are fine
	if x, ok := x.(float32); ok {
		y := y.(float32)
		if (x != x) && (y != y) {
			return true
		}
	}
	if x, ok := x.(Reverse[float32]); ok {
		x := x.Value()
		y := y.(Reverse[float32]).Value()
		if (x != x) && (y != y) {
			return true
		}
	}
	// float64 NaNs are fine
	if x, ok := x.(float64); ok {
		y := y.(float64)
		if (x != x) && (y != y) {
			return true
		}
	}
	if x, ok := x.(Reverse[float64]); ok {
		x := x.Value()
		y := y.(Reverse[float64]).Value()
		if (x != x) && (y != y) {
			return true
		}
	}
	return false
}

func TestOpcodeString(t *testing.T) {
	tests := []struct {
		opcode opcode
		str    string
	}{
		{opString, "string"},
		{opFloat32, "float32"},
		{opFloat64, "float64"},
		{opRaw, "raw"},
		{opInt | opPosInt, "int"},
		{opInt, "int"},
		{opInf, "infinity"},
		{opString ^ rev, "rev string"},
		{opFloat32 ^ rev, "rev float32"},
		{opFloat64 ^ rev, "rev float64"},
		{opInt | opPosInt ^ rev, "rev int"},
		{opInt ^ rev, "rev int"},
		{opInf ^ rev, "rev infinity"},
		{0x60, "opcode(0x60)"},
	}
	for _, tt := range tests {
		if str := tt.opcode.String(); str != tt.str {
			t.Errorf("opcode(%#x).String() = %q, want %q", int(tt.opcode), str, tt.str)
		}
	}
}

var corruptTests = []string{
	"\x01\x00",
	"\x01hello\x00",
	"\x01hello\x00\xfeworld\x00\x00",
	"\x01hello\x00",
	"\x32\x00\x23\x45",
	"\x33\x01\x23\x45",
	"\x2d\xff\xdc\xbb",
	"\x2c\xff\xdc\xbb",
	"\x38\xff\xff\xff\xff\xff\xff\xff\xff\xff",
	"\x28\x7f\x00\x00\x00\x00\x00\x00\x00",
	"\x27\x7f\x00\x00\x00\x00\x00\x00\x00\x00",
	"\x02\x00\x00\x00",
	"\x03\x00\x00\x00\x00\x00\x00\x00",
	"\x20",
}

var corruptTypedTests = []struct {
	enc  []byte
	list []any
}{
	{Encode(1, 2), []any{new(int), new(int), new(int)}},
	{Encode(1), []any{new(string)}},
	{Encode(1), []any{new(complex64)}},
	{[]byte("\x0f"), []any{new(any)}},
	{Encode(1 << 8), []any{new(uint8)}},
	{Encode(1 << 16), []any{new(uint16)}},
	{Encode(1 << 32), []any{new(uint32)}},
	{Encode(-1), []any{new(uint8)}},
	{Encode(-1), []any{new(uint16)}},
	{Encode(-1), []any{new(uint32)}},
	{Encode(-1), []any{new(uint64)}},
	{Encode(-1), []any{new(uint)}},
	{Encode(-1), []any{new(uintptr)}},
	{Encode(Rev(-1)), []any{new(Reverse[uint8])}},
	{Encode(Rev(-1)), []any{new(Reverse[uint16])}},
	{Encode(Rev(-1)), []any{new(Reverse[uint32])}},
	{Encode(Rev(-1)), []any{new(Reverse[uint64])}},
	{Encode(Rev(-1)), []any{new(Reverse[uint])}},
	{Encode(Rev(-1)), []any{new(Reverse[uintptr])}},
	{Encode(uint64(1<<64 - 1)), []any{new(int8)}},
	{Encode(uint64(1<<64 - 1)), []any{new(int16)}},
	{Encode(uint64(1<<64 - 1)), []any{new(int32)}},
	{Encode(uint64(1<<64 - 1)), []any{new(int64)}},
	{Encode(uint64(1<<64 - 1)), []any{new(int)}},
	{Encode(Rev(uint64(1<<64 - 1))), []any{new(Reverse[int8])}},
	{Encode(Rev(uint64(1<<64 - 1))), []any{new(Reverse[int16])}},
	{Encode(Rev(uint64(1<<64 - 1))), []any{new(Reverse[int32])}},
	{Encode(Rev(uint64(1<<64 - 1))), []any{new(Reverse[int64])}},
	{Encode(Rev(uint64(1<<64 - 1))), []any{new(Reverse[int])}},
}

func TestCorrupt(t *testing.T) {
	for _, enc := range corruptTests {
		_, err := DecodeAny([]byte(enc))
		if err != errCorrupt {
			t.Errorf("DecodeAny(%x) = %v, want %v", enc, err, errCorrupt)
		}

		rev := make([]byte, len(enc))
		for i, x := range []byte(enc) {
			rev[i] = ^x
		}
		_, err = DecodeAny([]byte(rev))
		if err != errCorrupt {
			t.Errorf("DecodeAny(%x) = %v, want %v", rev, err, errCorrupt)
		}
	}

	for _, tt := range corruptTypedTests {
		err := Decode(tt.enc, tt.list...)
		if err == nil {
			var types []string
			for _, x := range tt.list {
				types = append(types, reflect.ValueOf(x).Elem().Type().String())
			}
			t.Errorf("Decode(%x, %s): success, want error", tt.enc, types)
		}
	}
}

var canEncodeTests = []any{
	[]byte("hello"),
	"hello",
	Inf,
	StringOrInfinity{},
	int(1),
	int8(1),
	int16(1),
	int32(1),
	int64(1),
	uint(1),
	uint8(1),
	uint16(1),
	uint32(1),
	uint64(1),
	float32(1),
	float64(1),
	Raw("hello"),
}

var canEncodeFalse = []any{
	1i,
}

func init() {
	for _, x := range canEncodeTests {
		if _, ok := x.(Raw); ok {
			return
		}
		canEncodeTests = append(canEncodeTests, RevAny(x))
	}
}

func TestCanEncode(t *testing.T) {
	for _, x := range canEncodeTests {
		if !CanEncode(x) {
			t.Errorf("CanEncode(%T) = false, want true", x)
		}
	}
	if !CanEncode(canEncodeTests...) {
		t.Errorf("CanEncode(all) = false, want true")
	}

	for _, x := range canEncodeFalse {
		if CanEncode(x) {
			t.Errorf("CanEncode(%T) = true, want false", x)
		}
	}
	if CanEncode(append(canEncodeTests, canEncodeFalse...)) {
		t.Errorf("CanEncode(good+bad) = true, want false")
	}
}

func TestPanics(t *testing.T) {
	func() {
		defer func() {
			recover()
		}()
		RevAny(1i)
		t.Errorf("RevAny did not panic")
	}()
	func() {
		defer func() {
			recover()
		}()
		Append(nil, 1i)
		t.Errorf("Append did not panic")
	}()
}

var decodeFmtTests = []struct {
	enc []byte
	s   string
}{
	{Encode(1, 2, uint64(1e19)), "(1, 2, 10000000000000000000)"},
	{Encode(float32(1.0), math.NaN()), "(float32(1), float64(NaN))"},
	{Encode("hello"), "(\"hello\")"},
	{Encode(Inf), "(Inf)"},
	{Encode(Rev(1), Rev(2), Rev(uint64(1e19))), "(Rev(1), Rev(2), Rev(10000000000000000000))"},
	{Encode(Rev(float32(1.0)), Rev(math.Inf(+1))), "(Rev(float32(1)), Rev(float64(+Inf)))"},
	{Encode(Rev("hello")), "(Rev(\"hello\"))"},
	{Encode(Rev(Inf)), "(Rev(Inf))"},
	{Encode(Raw("hello")), "(Raw(\"hello\"))"},
}

func TestDecodeFmt(t *testing.T) {
	for _, tt := range decodeFmtTests {
		s, err := DecodeFmt(tt.enc)
		if s != tt.s || err != nil {
			t.Errorf("DecodeFmt(%x) = %q, %v, want %q, nil", tt.enc, s, err, tt.s)
		}
	}

	if _, err := DecodeFmt([]byte("\x20")); err == nil {
		t.Errorf("DecodeFmt(04) success, want error")
	}
}

func TestDecodeNil(t *testing.T) {
	enc := Encode(1, 2, 3)
	var x int
	err := Decode(enc, nil, nil, &x)
	if err != nil || x != 3 {
		t.Errorf("Decode(enc, nil, nil, &x) = %v, x=%d; want nil, x=3", err, x)
	}
}
