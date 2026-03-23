import ast
import re
import sys

def parse_python_file(filepath):
    with open(filepath, 'r') as f:
        source = f.read()
    
    tree = ast.parse(source)
    
    go_tests = []
    
    for node in tree.body:
        if isinstance(node, ast.ClassDef):
            class_name = node.name
            if class_name in ['TestBookCrossover', '_TestBookAmbiguousAVERAGE']:
                continue
            
            is_booking_base = any(b.id == '_BookingTestBase' for b in node.bases if isinstance(b, ast.Name))
            
            for item in node.body:
                if isinstance(item, ast.FunctionDef) and item.name.startswith('test_'):
                    test_name = item.name
                    docstring = ast.get_docstring(item)
                    
                    if not docstring:
                        continue
                    
                    # Extract booking method from decorator
                    booking_method = "core.BookingStrict"
                    for dec in item.decorator_list:
                        if isinstance(dec, ast.Call) and isinstance(dec.func, ast.Name) and dec.func.id == 'book_test':
                            if isinstance(dec.args[0], ast.Attribute):
                                booking_method = f"core.Booking{dec.args[0].attr.title().replace('Fifo', 'FIFO').replace('Lifo', 'LIFO').replace('Hifo', 'HIFO')}"
                    
                    go_name = f"Test{class_name}_{test_name}"
                    
                    if is_booking_base:
                        go_code = f"""
func {go_name}(t *testing.T) {{
	input := `
{docstring}
`
	runBookingTest(t, input, {booking_method})
}}
"""
                        go_tests.append(go_code)

    return go_tests

tests = parse_python_file('beancount/parser/booking_full_test.py')
with open('v3/parser/booking_full_parity_auto_test.go', 'w') as f:
    f.write("package parser\n\nimport (\n\t\"testing\"\n\t\"github.com/beancount/beancount/v3/core\"\n)\n\n")
    f.write("\n".join(tests))

print(f"Generated {len(tests)} tests.")
