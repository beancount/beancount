package core

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"reflect"
	"sort"
	"time"
)

// IGNORED_FIELD_NAMES is a set of field names that are being ignored for persistence.
var IGNORED_FIELD_NAMES = map[string]bool{
	"Meta":       true,
	"DiffAmount": true,
}

// HashEntry computes the stable hash of a single entry.
func HashEntry(entry Directive, excludeMeta bool) string {
	ignore := make(map[string]bool)
	if excludeMeta {
		for k, v := range IGNORED_FIELD_NAMES {
			ignore[k] = v
		}
	}
	return stableHash(reflect.ValueOf(entry), ignore)
}

func stableHash(v reflect.Value, ignore map[string]bool) string {
	if v.Kind() == reflect.Ptr || v.Kind() == reflect.Interface {
		if v.IsNil() {
			return "nil"
		}
		return stableHash(v.Elem(), ignore)
	}

	h := md5.New()

	switch v.Kind() {
	case reflect.Struct:
		// Handle special types
		if t, ok := v.Interface().(time.Time); ok {
			h.Write([]byte(t.Format("2006-01-02")))
			return hex.EncodeToString(h.Sum(nil))
		}
		// For decimal.Big, use String()
		if v.Type().Name() == "Big" {
			var method reflect.Value
			if v.CanAddr() {
				method = v.Addr().MethodByName("String")
			} else {
				method = v.MethodByName("String")
			}
			if method.IsValid() {
				h.Write([]byte(method.Call(nil)[0].String()))
				return hex.EncodeToString(h.Sum(nil))
			}
		}

		typ := v.Type()
		for i := 0; i < v.NumField(); i++ {
			fieldName := typ.Field(i).Name
			if ignore[fieldName] {
				continue
			}
			fieldValue := v.Field(i)
			h.Write([]byte(stableHash(fieldValue, ignore)))
		}
	case reflect.Slice, reflect.Array:
		var subhashes []string
		for i := 0; i < v.Len(); i++ {
			subhashes = append(subhashes, stableHash(v.Index(i), ignore))
		}
		sort.Strings(subhashes)
		for _, s := range subhashes {
			h.Write([]byte(s))
		}
	case reflect.Map:
		var keys []string
		for _, k := range v.MapKeys() {
			keys = append(keys, k.String())
		}
		sort.Strings(keys)
		for _, k := range keys {
			h.Write([]byte(k))
			h.Write([]byte(stableHash(v.MapIndex(reflect.ValueOf(k)), ignore)))
		}
	default:
		h.Write([]byte(fmt.Sprint(v.Interface())))
	}

	return hex.EncodeToString(h.Sum(nil))
}

// HashEntries computes unique hashes of each of the entries and return a map of them.
func HashEntries(entries []Directive, excludeMeta bool) (map[string]Directive, []error) {
	entryHashMap := make(map[string]Directive)
	var errors []error
	for _, entry := range entries {
		h := HashEntry(entry, excludeMeta)
		if other, ok := entryHashMap[h]; ok {
			if entry.Type() != TypePrice {
				errors = append(errors, fmt.Errorf("duplicate entry: %v == %v", entry, other))
			}
		}
		entryHashMap[h] = entry
	}
	return entryHashMap, errors
}

// CompareEntries compares two lists of entries.
func CompareEntries(entries1, entries2 []Directive) (bool, []Directive, []Directive, error) {
	hashes1, errs1 := HashEntries(entries1, true)
	if len(errs1) > 0 {
		return false, nil, nil, errs1[0]
	}
	hashes2, errs2 := HashEntries(entries2, true)
	if len(errs2) > 0 {
		return false, nil, nil, errs2[0]
	}

	keys1 := make(map[string]bool)
	for k := range hashes1 {
		keys1[k] = true
	}
	keys2 := make(map[string]bool)
	for k := range hashes2 {
		keys2[k] = true
	}

	var missing1 []Directive
	for k := range keys1 {
		if !keys2[k] {
			missing1 = append(missing1, hashes1[k])
		}
	}
	SortDirectives(missing1)

	var missing2 []Directive
	for k := range keys2 {
		if !keys1[k] {
			missing2 = append(missing2, hashes2[k])
		}
	}
	SortDirectives(missing2)

	same := len(missing1) == 0 && len(missing2) == 0
	return same, missing1, missing2, nil
}

// IncludesEntries checks if a list of entries is included in another list.
func IncludesEntries(subsetEntries, entries []Directive) (bool, []Directive, error) {
	subsetHashes, errs1 := HashEntries(subsetEntries, true)
	if len(errs1) > 0 {
		return false, nil, errs1[0]
	}
	hashes, errs2 := HashEntries(entries, true)
	if len(errs2) > 0 {
		return false, nil, errs2[0]
	}

	var missing []Directive
	for k, entry := range subsetHashes {
		if _, ok := hashes[k]; !ok {
			missing = append(missing, entry)
		}
	}
	SortDirectives(missing)

	return len(missing) == 0, missing, nil
}

// ExcludesEntries checks that a list of entries does not appear in another list.
func ExcludesEntries(subsetEntries, entries []Directive) (bool, []Directive, error) {
	subsetHashes, errs1 := HashEntries(subsetEntries, true)
	if len(errs1) > 0 {
		return false, nil, errs1[0]
	}
	hashes, errs2 := HashEntries(entries, true)
	if len(errs2) > 0 {
		return false, nil, errs2[0]
	}

	var extra []Directive
	for k, entry := range subsetHashes {
		if _, ok := hashes[k]; ok {
			extra = append(extra, entry)
		}
	}
	SortDirectives(extra)

	return len(extra) == 0, extra, nil
}
