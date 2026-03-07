import enum
import io

from beancount.core import data
from beancount.parser.grammar import ParserError

__plugins__ = ["read_literate_file"]

START_BEAN = "```beancount"
END_BEAN = "```"


class ParseState(enum.Enum):
    COMMENT = enum.auto()
    CODEBLOCK = enum.auto()


def find_next_state(state, line):
    stripped_line = line.strip()
    if state == ParseState.COMMENT:
        if stripped_line.startswith(START_BEAN):
            return ParseState.CODEBLOCK, None
    elif state == ParseState.CODEBLOCK:
        if stripped_line == END_BEAN:
            return ParseState.COMMENT, None

    return state, None


def read_literate_file(file_io, filename):
    """Parses markdown files and extracts Beancount code blocks."""
    if not filename or not filename.endswith(".md"):
        # We only process markdown files.
        yield (file_io, filename, [])
        return

    contents = file_io.read()
    if isinstance(contents, bytes):
        contents = contents.decode("utf-8")

    state = ParseState.COMMENT
    output = io.StringIO()
    line_number_start = 0
    errors = []

    lines = contents.splitlines(True)
    for line_number, line in enumerate(lines):
        stripped_line = line.strip()
        next_state, error_msg = find_next_state(state, line)

        if state == ParseState.COMMENT and next_state == ParseState.CODEBLOCK:
            # Check for remaining text after ```beancount
            remaining = stripped_line[len(START_BEAN) :].strip()
            if remaining:
                meta = data.new_metadata(filename, line_number + 1)
                errors.append(
                    ParserError(meta, f"Extra text after code block marker: '{remaining}'")
                )
            line_number_start = line_number
            # Use line_number + 2 because line_number is 0-indexed and the first line of content is the next line
            output.write(f"; from {filename} starting at {line_number + 2}\n")
            # We don't write the ```beancount line itself.

        elif state == ParseState.CODEBLOCK:
            if next_state == ParseState.CODEBLOCK:
                output.write(line)
            else:
                # Exiting code block
                block = f"Block{line_number_start + 1}-{line_number + 1}"
                yield (
                    io.BytesIO(output.getvalue().encode("utf-8")),
                    f"{filename}:{block}",
                    errors,
                )
                output = io.StringIO()
                errors = []  # Reset errors for next block

        state = next_state

    if state == ParseState.CODEBLOCK:
        # File ended while still inside a code block
        meta = data.new_metadata(filename, len(lines))
        errors.append(ParserError(meta, "Unclosed markdown code block"))
        block = f"Block{line_number_start + 1}-{len(lines)}"
        yield (io.BytesIO(output.getvalue().encode("utf-8")), f"{filename}:{block}", errors)
