import re


class Functor:
    def __init__(self, name, tuple_):
        self.name = name
        self.tuple = tuple_
        self.arity = len(self.tuple)
    
    def __str__(self):
        return self.name + "(" + ", ".join(map(str, self.tuple)) + ")"


class List:
    def __init__(self, list_):
        self.list = list_
        self.length = len(list_)
    
    def __str__(self):
        return "[" + ", ".join(map(str, self.list)) + "]"


class Atom:
    def __init__(self, name):
        assert type(name) == int or name[0].islower()
        self.name = name
    
    def __str__(self):
        return str(self.name)


class Variable:
    def __init__(self, name):
        assert name[0].isupper()
        self.name = name
    
    def __str__(self):
        return str(self.name)


class Any:
    def __str__(self):
        return "?"


def unify_variable(context, x, y):
    if x.name in context:
        return unify(context, context[x.name], y)
    elif type(y) == Variable and y.name in context:
        return unify(context, x, context[y.name])
    elif not (type(y) == Variable and y.name == x.name):
        context[x.name] = y
        return True

def unify(context, x, y):
    tx = type(x)
    ty = type(y)
    if tx == Any or ty == Any:
        return True
    elif tx == Atom and ty == Atom:
        return x.name == y.name
    elif tx == Functor and ty == Functor:
        if x.arity != y.arity:
            return False
        elif x.name != y.name:
            return False
        for a, b in zip(x.tuple, y.tuple):
            if not unify(context, a, b):
                return False
        return True
    elif tx == List and ty == List:
        if x.length != y.length:
            return False
        for a, b in zip(x.list, y.list):
            if not unify(context, a, b):
                return False
        return True
    elif tx == Variable:
        return unify_variable(context, x, y)
    elif ty == Variable:
        return unify_variable(context, y, x)
    return False

def stringify_expr(context, expr_):
    def _stringify_expr(expr):
        if type(expr) in (Atom, Any):
            return str(expr)
        elif type(expr) == List:
            return "[" + ", ".join(map(_stringify_expr, expr.list)) + "]"
        elif type(expr) == Functor:
            return expr.name + "(" + ", ".join(map(_stringify_expr, expr.tuple)) + ")"
        elif type(expr) == Variable:
            if expr.name in context:
                return _stringify_expr(context[expr.name])
            return expr.name
    return _stringify_expr(expr_)


# LEXER + PARSER


name = re.compile(r"[A-Za-z][A-Za-z0-9_?!]*")
arith = re.compile(r"\+|\-|\*|/")
assign = re.compile(r"=")
ignore = re.compile(r"_")
integer = re.compile(r"[0-9]+")
lbracket = re.compile(r"\(")
rbracket = re.compile(r"\)")
lsquare = re.compile(r"\[")
rsquare = re.compile(r"\]")
comma = re.compile(r",")

TOKENS = [
    ("name", name),
    ("assign", assign),
    ("ignore", ignore),
    ("lbracket", lbracket),
    ("rbracket", rbracket),
    ("lsquare", lsquare),
    ("rsquare", rsquare),
    ("comma", comma),
    ("integer", integer)
]

def best_match(string, start):
    best_match = ("", "")
    for (name, regex) in TOKENS:
        match = regex.match(string, start)
        if not match:
            continue
        match = match.group()
        if len(match) > len(best_match[1]):
            best_match = (name, match)
    if len(best_match[0]):
        return best_match
    raise ValueError(f"unknown token at pos {start}")

def lex_string(string):
    tokens = []
    pos = 0
    while pos < len(string):
        while pos < len(string) and string[pos].isspace():
            pos += 1
        if pos == len(string):
            break
        ttyp, tval = best_match(string, pos)
        pos += len(tval)
        tokens.append((ttyp, tval))
    return tokens


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def is_empty(self):
        return self.pos >= len(self.tokens)

    def current(self):
        return self.tokens[self.pos]

    def comma_delimited_list(self, ending):
        args = []
        while not self.is_empty() and self.current()[0] != ending:
            args.append(self.expr())
            if self.is_empty():
                break
            elif self.current()[0] != ending and self.current()[0] != "comma":
                raise ValueError("expected comma")
            elif self.current()[0] == "comma":
                self.pos += 1
        if self.is_empty():
            raise ValueError("expected a closing {ending}")
        self.pos += 1
        return args

    def stmt(self):
        expr = self.expr()
        if self.is_empty():
            result = ["literal", expr]
        elif self.current()[0] == "assign":
            self.pos += 1
            result = ["=", expr, self.expr()]
        if not self.is_empty() or result is None:
            raise ValueError("syntax error")
        return result

    def expr(self):
        return self.functor()
    
    def functor(self):
        terminal = self.terminal()
        if not self.is_empty() and type(terminal) == Atom and self.current()[0] == "lbracket":
            name = terminal.name
            if type(name) == int:
                raise ValueError("cannot use an integer as a functor name")
            self.pos += 1
            return Functor(name, tuple(self.comma_delimited_list("rbracket")))
        return terminal

    def terminal(self):
        if self.is_empty():
            raise ValueError()
        typ, val = self.current()
        self.pos += 1
        if typ == "integer":
            return Atom(int(val))
        elif typ == "name":
            if val[0].isupper():
                return Variable(val)
            return Atom(val)
        elif typ == "lsquare":
            return List(self.comma_delimited_list("rsquare"))
        elif typ == "ignore":
            return Any()
        raise ValueError(f"unexpected token {typ}")


# MAIN


def gather_variables(into, expr):
    typ = type(expr)
    if typ == Variable:
        into.add(expr.name)
    elif typ == Functor:
        for sube in expr.tuple:
            gather_variables(into, sube)
    elif typ == List:
        for sube in expr.list:
            gather_variables(into, sube)
    return into


def evaluate_equality(e1, e2, context=None):
    context = context or {}
    if unify(context, e1, e2):
        vars_ = gather_variables(set(), e1) | gather_variables(set(), e2)
        for var in vars_:
            if var not in context:
                context[var] = Any()
        return True, context
    return False, context


def main():
    print("Enter 'quit' to quit")
    s = input("> ")
    while s != "quit":
        stmt = Parser(lex_string(s)).stmt()
        context = {}
        if stmt[0] == "literal":
            print(stmt[1])
        elif stmt[0] == "=":
            success, context = evaluate_equality(*stmt[1:])
            if success:
                for k, v in context.items():
                    try:
                        print(k, "=", stringify_expr(context, v))
                    except RecursionError:
                        print(k, "=", "(...)")
            else:
                print("does not unify")
        s = input("> ")


if __name__ == '__main__':
    main()
