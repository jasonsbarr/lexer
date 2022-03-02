from types import NoneType
from typing import Iterable, Iterator, Union, Generator
from dataclasses import dataclass, astuple
import re
from abc import ABC, abstractmethod

# A simple regex-based lexer with a fluent interface
# adapted from https://eli.thegreenplace.net/2013/06/25/regex-based-lexical-analysis-in-python-and-javascript


class LexerError(Exception):
    """Raise when lexer encounters an invalid character"""

    def __init__(self, char, line, col):
        super().__init__(f"Invalid token {char} at ({line}:{col})")


@dataclass
class Token:
    """Token object represents a single lexeme"""
    type: str
    val: str
    line: int
    col: int
    pos: int

    def __str__(self) -> str:
        return f"Token(type={self.type}, val={self.val})"

    def __repr__(self) -> str:
        return f"Token(type={self.type}, val={self.val}, line={self.line}, col={self.col}, pos={self.pos})"


@dataclass
class Rule:
    """Regex-based rule that produces a token when applied to a valid lexeme

    regex should be a raw string
    """
    name: str
    regex: str

    def __init__(self, name: str, regex: str):
        self.name = name
        self.regex = regex

    def __str__(self) -> str:
        return f"Rule(name={self.name}, re={self.regex})"

    def __repr__(self) -> str:
        return str(self)

    def __iter__(self):
        return iter(astuple(self))

    def __getitem__(self, keys):
        """Enable destructuring-like syntax on the object, e.g.: name, regex = rule["name", "regex"]"""
        return iter(getattr(self, k) for k in keys)


class InputStream:
    """Manages the state of the input stream as the lexer processes it"""

    def __init__(self, buf):
        self.buf = buf
        self.pos = 0
        self.line = 1
        self.col = 1

    def __len__(self):
        return len(self.buf)

    def __iter__(self):
        return iter(self.buf)

    def __getitem__(self, keys):
        """Enable destructuring-like syntax on the object, e.g.: buf, pos = rule["buf", "pos"]"""
        return iter(getattr(self, k) for k in keys)

    def advance(self, pos):
        self.pos = pos

        if pos >= len(self):
            return

        if re.match(r"\r?\n", self.buf[self.pos]):
            self.line += 1
            self.col = 0
        else:
            self.col += 1

    def eof(self) -> bool:
        return self.pos >= len(self)


def eof_token(line, col, pos):
    return Token("ENDOFINPUT", "EndOfInput", line, col, pos)


class LexerBase(ABC):
    """Abstract base class for Lexer, compiles rules into a single regex"""

    def __init__(self, rules: Iterable[Rule]):
        self.rules = list(rules)
        self.groups = {}

    @abstractmethod
    def token(self) -> Union[Token, NoneType]:
        pass

    @abstractmethod
    def tokenize(self) -> Iterator[Token]:
        pass

    def compile(self):
        """Compiles the provided rules into a single regular expression"""
        re_frags = []
        i = 1

        for name, regex in self.rules:
            groupname = f"{name}{i}"
            re_frags.append(f"(?P<{groupname}>{regex})")
            self.groups[groupname] = name
            i += 1

        self.regex = re.compile("|".join(re_frags))

        return self

    def extend(self, prepend_rules: Iterable[Rule] = None, append_rules: Iterable[Rule] = None):
        """
        Extend the lexer with additional rules that can be either prepended or appended to the original rules

        Must call self.compile after extending the rules list
        """
        if not prepend_rules is None:
            self.rules = list(prepend_rules) + self.rules

        if not append_rules is None:
            self.rules = self.rules + list(append_rules)

        return self


class Lexer(LexerBase):
    """The lexer itself"""

    def __init__(self, rules: Iterable[Rule], skip_ws=False):
        super().__init__(rules)
        self.skip_ws = skip_ws

        # Skipping WS will currently cause your tokens to have inaccurate line numbers
        if skip_ws:
            self.skip_ws_re = re.compile(r"\S")

    def input(self, input_str: str):
        self.input_str = InputStream(input_str)

        return self

    def token(self) -> Union[Token, NoneType]:
        buf, pos, line, col = self.input_str["buf", "pos", "line", "col"]

        if self.input_str.eof():
            return None

        if self.skip_ws:
            m = self.skip_ws_re.search(buf, pos)

            if m:
                pos = m.start()
                self.input_str.advance(pos)
            else:
                # We are at the end of the input
                self.input_str.advance(float("inf"))
                return None

        m = self.regex.match(buf, pos)

        if m:
            groupname = m.lastgroup
            tok_type = self.groups[groupname]
            tok = Token(tok_type, m.group(groupname), line, col, pos)
            self.input_str.advance(m.end())
            return tok

        # No rule matched
        raise LexerError(buf[pos], line, col)

    def tokenize(self) -> Generator[Token]:
        """Returns a generator of the tokens found in the input buffer"""
        while not self.input_str.eof():
            tok = self.token()
            if not tok is None:
                yield tok

        yield eof_token(self.input_str.line, self.input_str.col, self.input_str.pos)
