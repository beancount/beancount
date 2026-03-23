package core

import (
	"testing"
	"time"
)

func TestPositionFromString(t *testing.T) {
	tests := []struct {
		input    string
		expected Position
		wantErr  bool
	}{
		{"10 USD", NewPosition(NewAmount(D("10"), "USD"), nil), false},
		{" - 111.2934  CAD ", NewPosition(NewAmount(D("-111.2934"), "CAD"), nil), false},
		{"2.2 HOOL {532.43 USD}", NewPosition(NewAmount(D("2.2"), "HOOL"), &Cost{Number: D("532.43"), Currency: "USD"}), false},
		{"2.2 HOOL {532.43 USD, 2014-06-15}", NewPosition(NewAmount(D("2.2"), "HOOL"), &Cost{Number: D("532.43"), Currency: "USD", Date: time.Date(2014, 6, 15, 0, 0, 0, 0, time.UTC)}), false},
		{"2.2 HOOL {\"78c3f7f1315b\"}", NewPosition(NewAmount(D("2.2"), "HOOL"), &Cost{Label: "78c3f7f1315b"}), false},
		{"20 HOOL {532.43 # 20.00 USD, \"e4dc1a361022\", 2014-06-15}", NewPosition(NewAmount(D("20"), "HOOL"), &Cost{Number: D("27.6215"), Currency: "USD", Date: time.Date(2014, 6, 15, 0, 0, 0, 0, time.UTC), Label: "e4dc1a361022"}), false},
	}

	for _, tc := range tests {
		got, err := PositionFromString(tc.input)
		if (err != nil) != tc.wantErr {
			t.Errorf("PositionFromString(%q) error = %v, wantErr %v", tc.input, err, tc.wantErr)
			continue
		}
		if !tc.wantErr && !got.Equal(&tc.expected) {
			t.Errorf("PositionFromString(%q) = %v, expected %v", tc.input, got, tc.expected)
		}
	}
}

func TestPositionArithmetic(t *testing.T) {
	pos, _ := PositionFromString("2 HOOL {100.00 USD}")

	// Neg
	negPos := pos.Neg()
	if negPos.Units.Number.Cmp(D("-2")) != 0 {
		t.Errorf("Neg failed: units = %v", negPos.Units)
	}

	// Abs
	absPos := negPos.Abs()
	if absPos.Units.Number.Cmp(D("2")) != 0 {
		t.Errorf("Abs failed: units = %v", absPos.Units)
	}

	// Mul
	mulPos := pos.Mul(D("3"))
	if mulPos.Units.Number.Cmp(D("6")) != 0 {
		t.Errorf("Mul failed: units = %v", mulPos.Units)
	}

	// Commission
	p3, _ := PositionFromString("20 HOOL {532.43 # 20.00 USD}")
	if p3.Cost.Number.Cmp(D("27.6215")) != 0 {
		t.Errorf("PositionFromString with commission failed: got %v", p3.Cost.Number)
	}
}

func TestPositionSort(t *testing.T) {
	pos1, _ := PositionFromString("200 USD")
	pos2, _ := PositionFromString("201 USD")
	pos3, _ := PositionFromString("100 CAD")
	pos4, _ := PositionFromString("101 CAD")
	pos5, _ := PositionFromString("50 ZZZ")

	positions := []Position{pos5, pos4, pos3, pos2, pos1}
	SortPositions(positions)

	expected := []Position{pos1, pos2, pos3, pos4, pos5}
	for i := range positions {
		if !positions[i].Equal(&expected[i]) {
			t.Errorf("At index %d: got %v, expected %v", i, positions[i], expected[i])
		}
	}
}

func TestPositionEqualZeroToNil(t *testing.T) {
	pos := NewPosition(NewAmount(D("0"), "CAD"), nil)
	if !pos.Equal(nil) {
		t.Errorf("Position(0 CAD).Equal(nil) should be true")
	}
}
