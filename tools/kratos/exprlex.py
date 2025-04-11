try:
    import ply.lex as lex
except ImportError:
    import pycparser.ply.lex as lex


class ExprLexer(object):
    tokens = (
        'SYMBOL',
        'KEYWORD',
        'DOT',
        'GLOBALS',
        'INIT',
        'ENTRY',
        'FUNCTION',
        'LOCALS',
        'RETURN',
        'TYPE',
        'DEFINE',
        'VAR',
        'CONST',
        'OP',
        'CALL',
        'ASSIGN',
        'ASSUME',
        'TYPECAST',
        'CAST',
        'BITCAST',
        'SEQ',
        'JUMP',
        'LABEL',
        'HAVOC',
        'CONDJUMP',
        'VOID',
        'BOOL',
        'INT',
        'REAL',
        'BV',
        'SBV',
        'UBV',
        'FP',
        'MAP',
        'SYM',
        'FUN',
        'ENUM',
        )

    literals = "()!"
    t_ignore = ' \t'

    reserved = dict([(t.lower(), t) for t in tokens[3:]] +
                    [('.', 'DOT'), ('!', '!')])

    states = [
        ('quotedsymbol', 'exclusive'),
        ('stringsymbol', 'exclusive'),
        ]

    def __init__(self):
        self.lexer = None
        self.quoted_symbol_buf = None

    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)

    def t_comment(self, t):
        r';.*\n'
        t.lexer.lineno += 1

    def t_QUOTEDSYMBOL(self, t):
        r'[|]'
        self.quoted_symbol_buf = []
        t.lexer.begin('quotedsymbol')

    def t_quotedsymbol_quotedbar(self, t):
        r'\\\|'
        self.quoted_symbol_buf.append('|')

    def t_quotedsymbol_bar(self, t):
        r'[|]'
        t.lexer.begin('INITIAL')
        t.type = 'SYMBOL'
        t.value = "".join(self.quoted_symbol_buf)
        self.quoted_symbol_buf = None
        return t

    def t_quotedsymbol_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)
        self.quoted_symbol_buf.append(t.value)

    def t_quotedsymbol_any(self, t):
        r'[^|]'
        self.quoted_symbol_buf.append(t.value)

    t_quotedsymbol_ignore = ''
    
    def t_quotedsymbol_error(self, t):
        raise Exception("ERROR: %s" % t)

    def t_STRINGSYMBOL(self, t):
        r'"'
        self.quoted_symbol_buf = ['"']
        t.lexer.begin('stringsymbol')

    def t_stringsymbol_quotedbar(self, t):
        r'\\"'
        self.quoted_symbol_buf.append('"')

    def t_stringsymbol_bar(self, t):
        r'"'
        t.lexer.begin('INITIAL')
        t.type = 'SYMBOL'
        self.quoted_symbol_buf.append('"')
        t.value = "".join(self.quoted_symbol_buf)
        self.quoted_symbol_buf = None
        return t

    def t_stringsymbol_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)
        self.quoted_symbol_buf.append(t.value)

    def t_stringsymbol_any(self, t):
        r'[^"]'
        self.quoted_symbol_buf.append(t.value)

    t_stringsymbol_ignore = ''
    
    def t_stringsymbol_error(self, t):
        raise Exception("ERROR: %s" % t)

    def t_SYMBOL(self, t):
        r'[a-zA-Z0-9._+\-*=%/?!$_~&^<>@]+'
        t.type = self.reserved.get(t.value, 'SYMBOL')
        return t

    def t_BINCONSTANT(self, t):
        r'[#]b[0-1]+'
        t.type = 'SYMBOL'
        return t

    def t_HEXCONSTANT(self, t):
        r'[#]x[0-9a-fA-F]+'
        t.type = 'SYMBOL'
        return t

    def t_RATCONSTANT(self, t):
        r'[0-9]+\.[0-9]+'
        t.type = 'SYMBOL'
        return t

    def t_NUMERAL(self, t):
        r'[0-9]+'
        t.type = 'SYMBOL'
        return t

    def t_KEYWORD(self, t):
        r':[a-zA-Z0-9_+\-*=%/?!$_~&^<>@][a-zA-Z0-9._+\-*=%/?!$_~&^<>@]*'
        t.type = 'KEYWORD'
        return t

    def t_error(self, t):
        raise Exception("ERROR: %s" % t)

    def build(self):
        self.lexer = lex.lex(object=self, lextab="exprlextab")

    def input(self, text):
        self.lexer.input(text)

    def token(self):
        return self.lexer.token()

# end of class ExprLexer

