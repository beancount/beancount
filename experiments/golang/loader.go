package v3

import (
	"fmt"
	"io/ioutil"
	"path/filepath"

	"github.com/beancount/beancount/v3/core"
	"github.com/beancount/beancount/v3/ops"
	"github.com/beancount/beancount/v3/parser"
)

// LoadError represents an error encountered during the loading process.
type LoadError struct {
	Source  core.Meta
	Message string
}

func (e LoadError) Error() string {
	filename, _ := e.Source["filename"].(string)
	lineno, _ := e.Source["lineno"].(int)
	return fmt.Sprintf("%s:%d: %s", filename, lineno, e.Message)
}

// LoadFile opens a Beancount input file, parses it, run transformations and validate.
func LoadFile(filename string) ([]core.Directive, []error, *parser.Options) {
	absFilename, err := filepath.Abs(filename)
	if err != nil {
		return nil, []error{err}, nil
	}

	entries, errors, optionsMap := parseRecursive([]string{absFilename})
	
	options, optErrors := parser.ProcessOptions(optionsMap)
	if len(optErrors) > 0 {
		errors = append(errors, optErrors...)
	}

	// Sort entries by date.
	core.SortDirectives(entries)

	// Run booking (interpolation).
	bookedEntries, bookingErrors := parser.Book(entries, options)
	if len(bookingErrors) > 0 {
		errors = append(errors, bookingErrors...)
	}

	// Run validation.
	validationErrors := ops.Validate(bookedEntries, options)
	if len(validationErrors) > 0 {
		errors = append(errors, validationErrors...)
	}

	return bookedEntries, errors, options
}

func parseRecursive(filenames []string) ([]core.Directive, []error, map[string][]string) {
	var allEntries []core.Directive
	var allErrors []error
	allOptions := make(map[string][]string)
	seen := make(map[string]bool)
	
	queue := make([]string, len(filenames))
	copy(queue, filenames)

	for len(queue) > 0 {
		filename := queue[0]
		queue = queue[1:]

		absPath, err := filepath.Abs(filename)
		if err != nil {
			allErrors = append(allErrors, err)
			continue
		}

		if seen[absPath] {
			continue
		}
		seen[absPath] = true

		content, err := ioutil.ReadFile(absPath)
		if err != nil {
			allErrors = append(allErrors, fmt.Errorf("failed to read file %q: %v", absPath, err))
			continue
		}

		p := parser.NewParser(string(content), absPath)
		entries, errs := p.Parse()
		if len(errs) > 0 {
			allErrors = append(allErrors, errs...)
		}

		allEntries = append(allEntries, entries...)

		// Merge options.
		for k, v := range p.GetOptions() {
			allOptions[k] = append(allOptions[k], v...)
		}

		// Handle includes.
		cwd := filepath.Dir(absPath)
		for _, includePattern := range p.GetOptions()["include"] {
			pattern := includePattern
			if !filepath.IsAbs(pattern) {
				pattern = filepath.Join(cwd, pattern)
			}
			
			matches, err := filepath.Glob(pattern)
			if err != nil {
				allErrors = append(allErrors, fmt.Errorf("failed to glob include pattern %q: %v", pattern, err))
				continue
			}
			
			if len(matches) == 0 {
				// Beancount issues an error if a glob doesn't match anything.
				allErrors = append(allErrors, fmt.Errorf("include glob %q did not match any files", includePattern))
			}

			for _, match := range matches {
				queue = append(queue, match)
			}
		}
	}

	return allEntries, allErrors, allOptions
}
