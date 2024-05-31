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
			t.Errorf("Encode(%s) = %x; Split = %#v", format(list...), enc, out)
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
		enc = Encode(RevAny(tt.x))
		want := []byte(tt.enc)
		for i, x := range want {
			want[i] = ^x
		}
		if string(enc) != string(want) {
			t.Errorf("Encode(Rev(%T(%s))) = %q (%x), want %q (%x)", tt.x, desc, enc, enc, want, want)
		}
	}
}
