import os
import string
from strings_with_arrows import *

####CONSTANTS####

DIGITS = '0123456789'
LETTERS = string.ascii_uppercase + string.ascii_lowercase
LETTERS_DIGITS = LETTERS + DIGITS

KEYWORDS = ['var', 'fun', 'if', 'elif', 'else', 'for', 'while', 'return', 'break', 'continue']

####ERRORS####

class Error:
    def __init__(self, pos_start, pos_end, error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}: {self.details}'
        result += f'\nFile {self.pos_start.fn}, line {self.pos_start.ln + 1}'
        result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
        return result
    
class IllegalCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Illegal Character', details)

class ExpectedCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Expected Character', details)

class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Invalid Syntax', details)

class RTError(Error):
    def __init__(self, pos_start, pos_end, details, context):
        super().__init__(pos_start, pos_end, 'Runtime Error', details)
        self.context = context

    def as_string(self):
        result = self.generate_traceback()
        result += f'{self.error_name}: {self.details}'
        result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
        return result

    def generate_traceback(self):
        result = ''
        pos = self.pos_start
        ctx = self.context
        while ctx:
            result = f'    File {pos.fn}, line {str(pos.ln + 1)}, in {ctx.display_name}\n' + result
            pos = ctx.parent_entry_pos
            ctx = ctx.parent

        return 'Traceback (most recent call last):\n' + result

####POSITION####

class Position:
    def __init__(self, idx, ln, col, fn, ftxt):
        self.idx = idx
        self.ln = ln
        self.col = col
        self.fn = fn
        self.ftxt = ftxt

    def advance(self, current_char=None):
        self.idx += 1
        self.col += 1

        if current_char == '\n':
            self.ln += 1
            self.col = 0
        
        return self
    
    def copy(self):
        return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)

####TOKENS####

TT_INT = 'INT'
TT_FLOAT = 'FLOAT'
TT_STRING = 'STRING'

TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_POW = 'POW'
TT_MOD = 'MOD'
TT_EQ = 'EQ'

TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'
TT_LBRACK = 'LBRACK'
TT_RBRACK = 'RBRACK'
TT_COMMA = 'COMMA'
TT_QUOTE = 'QUOTE'
TT_SEMI = 'SEMI'

TT_EE = 'EE'
TT_NE = 'NE'
TT_LT = 'LT'
TT_GT = 'GT'
TT_LTE = 'LTE'
TT_GTE = 'GTE'
TT_AND = 'AND'
TT_OR = 'OR'
TT_NOT = 'NOT'
TT_IN = 'IN'
TT_LCURL = 'LCURL'
TT_RCURL = 'RCURL'

TT_KEYWORD = 'KEYWORD'
TT_IDENTIFIER = 'IDENTIFIER'
TT_EOF = 'EOF'

class Token:
    def __init__(self, type_, value=None, pos_start=None, pos_end=None):
        self.type = type_
        self.value = value

        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()
        
        if pos_end:
            self.pos_end = pos_end.copy()

    def matches(self, type_, value):
        return self.type == type_ and self.value in value

    def __repr__(self):
        if self.value:
            return f'{self.type}:{self.value}'
        return f'{self.type}'
    
####LEXER####

class Lexer:
    def __init__(self, fn, text):
        self.fn = fn
        self.text = text
        self.pos = Position(-1, 0, -1, fn, text)
        self.current_char = None
        self.advance()

    def advance(self):
        self.pos.advance(self.current_char)
        self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None

    def make_tokens(self):
        tokens = []

        while self.current_char != None:
            if self.current_char in ' \t\n':
                self.advance()
            elif self.current_char == '#':
                self.skip_comment()
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
            elif self.current_char in LETTERS:
                tokens.append(self.make_identifier())
            elif self.current_char == '"':
                tokens.append(self.make_string())
            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS, pos_start=self.pos))
                self.advance()
                if self.current_char == '+':
                    tokens.append(Token(TT_INT, 1, self.pos))
                    self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TT_MINUS, pos_start=self.pos))
                self.advance()
                if self.current_char == '-':
                    tokens.append(Token(TT_INT, 1, self.pos))
                    self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TT_MUL, pos_start=self.pos))
                self.advance()
            elif self.current_char == '/':
                tokens.append(Token(TT_DIV, pos_start=self.pos))
                self.advance()
            elif self.current_char == '^':
                tokens.append(Token(TT_POW, pos_start=self.pos))
                self.advance()
            elif self.current_char == '%':
                tokens.append(Token(TT_MOD, pos_start=self.pos))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_RPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == '[':
                tokens.append(Token(TT_LBRACK, pos_start=self.pos))
                self.advance()
            elif self.current_char == ']':
                tokens.append(Token(TT_RBRACK, pos_start=self.pos))
                self.advance()
            elif self.current_char == '=':
                tokens.append(self.make_equals())
            elif self.current_char == '<':
                tokens.append(self.make_less_than())
            elif self.current_char == '>':
                tokens.append(self.make_greater_than())
            elif self.current_char == '!':
                tokens.append(self.make_not_equals())
            elif self.current_char == '&':
                tokens.append(Token(TT_AND, pos_start=self.pos))
                self.advance()
            elif self.current_char == '|':
                tokens.append(Token(TT_OR, pos_start=self.pos))
                self.advance()
            elif self.current_char == '{':
                tokens.append(Token(TT_LCURL, pos_start=self.pos))
                self.advance()
            elif self.current_char == '}':
                tokens.append(Token(TT_RCURL, pos_start=self.pos))
                self.advance()
            elif self.current_char == ',':
                tokens.append(Token(TT_COMMA, pos_start=self.pos))
                self.advance()
            elif self.current_char in ';':
                tokens.append(Token(TT_SEMI, pos_start=self.pos))
                self.advance()
            else:
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")

        tokens.append(Token(TT_EOF, pos_start=self.pos))
        return tokens, None
    
    def make_number(self):
        num_str = ""
        dot_count = 0
        pos_start = self.pos.copy()

        while self.current_char != None and self.current_char in DIGITS + '.':
            if self.current_char == '.':
                if dot_count == 1:
                    break
                dot_count += 1
                num_str += '.'
            else:
                num_str += self.current_char
            self.advance()

        if dot_count == 0:
            return Token(TT_INT, int(num_str), pos_start, self.pos)
        else:
            return Token(TT_FLOAT, float(num_str), pos_start, self.pos)
        
    def make_string(self):
        string = ''
        pos_start = self.pos.copy()
        escape_character = False
        special_characters = {'n':'\n', 't':'\t'}
        self.advance()
        
        while self.current_char != None and (self.current_char != '"' or escape_character):
            if escape_character:
                string += special_characters.get(self.current_char, self.current_char)
            else:
                if self.current_char == '\\':
                    escape_character = True
                else:
                    string += self.current_char

            self.advance()
            escape_character = False

        self.advance()
        return Token(TT_STRING, string, pos_start, self.pos)
        
    def make_identifier(self):
        id_str = ''
        pos_start = self.pos.copy()

        while self.current_char != None and self.current_char in LETTERS_DIGITS + '_':
            id_str += self.current_char
            self.advance()

        tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFIER
        return Token(tok_type, id_str, pos_start, self.pos)

    def make_not_equals(self):
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            return Token(TT_NE, pos_start=pos_start, pos_end=self.pos)
        
        return Token(TT_NOT, pos_start=pos_start, pos_end=self.pos)
    
    def make_equals(self):
        tok_type = TT_EQ
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            tok_type = TT_EE

        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)
    
    def make_less_than(self):
        tok_type = TT_LT
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            tok_type = TT_LTE
        elif self.current_char == '>':
            self.advance()
            tok_type = TT_IN

        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)
    
    def make_greater_than(self):
        tok_type = TT_GT
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            tok_type = TT_GTE

        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

    def skip_comment(self):
        self.advance()

        while self.current_char != '\n':
            self.advance()

        self.advance()

####NODES####

class NumberNode:
    def __init__(self, tok):
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

    def __repr__(self):
        return f'{self.tok}'
    
class StringNode:
    def __init__(self, tok):
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

    def __repr__(self):
        return f'{self.tok}'

class ArrayNode:
    def __init__(self, element_nodes, pos_start, pos_end):
        self.element_nodes = element_nodes
        self.pos_start = pos_start
        self.pos_end = pos_end

class VarAccessNode:
    def __init__(self, var_name_tok):
        self.var_name_tok = var_name_tok
        self.pos_start = var_name_tok.pos_start
        self.pos_end = var_name_tok.pos_end

class VarAssignNode:
    def __init__(self, var_name_tok, value_node):
        self.var_name_tok = var_name_tok
        self.value_node = value_node
        self.pos_start = var_name_tok.pos_start
        self.pos_end = var_name_tok.pos_end

class BinOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node
        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end

    def __repr__(self):
        return f'({self.left_node}, {self.op_tok}, {self.right_node})'

class UnaryOpNode:
    def __init__(self, op_tok, node):
        self.op_tok = op_tok
        self.node = node
        self.pos_start = op_tok.pos_start
        self.pos_end = node.pos_end

    def __repr__(self):
        return f'({self.op_tok}, {self.node})'

class IfNode:
    def __init__(self, cases, else_case):
        self.cases = cases
        self.else_case = else_case
        self.pos_start = self.cases[0][0].pos_start
        self.pos_end = (self.else_case or self.cases[len(self.cases) - 1])[0].pos_end

class ForNode:
    def __init__(self, var_name_tok, start_value_node, end_value_node, step_value_node, body_node, should_return_null):
        self.var_name_tok = var_name_tok
        self.start_value_node = start_value_node
        self.end_value_node = end_value_node
        self.step_value_node = step_value_node
        self.body_node = body_node
        self.should_return_null = should_return_null
        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.body_node.pos_end

class WhileNode:
    def __init__(self, condition_node, body_node, should_return_null):
        self.condition_node = condition_node
        self.body_node = body_node
        self.should_return_null = should_return_null
        self.pos_start = self.condition_node.pos_start
        self.pos_end = self.body_node.pos_end

class FuncDefNode:
    def __init__(self, var_name_tok, arg_name_toks, body_node, should_auto_return):
        self.var_name_tok = var_name_tok
        self.arg_name_toks = arg_name_toks
        self.body_node = body_node
        self.should_auto_return = should_auto_return
        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.body_node.pos_end

class CallNode:
    def __init__(self, node_to_call, arg_nodes):
        self.node_to_call = node_to_call
        self.arg_nodes = arg_nodes
        self.pos_start = self.node_to_call.pos_start
        if len(self.arg_nodes) > 0:
            self.pos_end = self.arg_nodes[len(self.arg_nodes) - 1].pos_end
        else:
            self.pos_end = self.node_to_call.pos_end

class ReturnNode:
    def __init__(self, node_to_return, pos_start, pos_end):
        self.node_to_return = node_to_return
        self.pos_start = pos_start
        self.pos_end = pos_end

class ContinueNode:
    def __init__(self, pos_start, pos_end):
        self.pos_start = pos_start
        self.pos_end = pos_end

class BreakNode:
    def __init__(self, pos_start, pos_end):
        self.pos_start = pos_start
        self.pos_end = pos_end

####PARSERESULTS####

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.advance_count = 0
        self.to_reverse_count = 0

    def register_advancement(self):
        self.advance_count += 1

    def register(self, res):
        self.advance_count = res.advance_count
        if res.error:
            self.error = res.error
        return res.node        

    def try_register(self, res):
        if res.error:
            self.to_reverse_count = res.advance_count
            return None
        return self.register(res)

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        if not self.error or self.advance_count == 0:
            self.error = error
        return self

####PARSER####

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.tok_idx = -1
        self.advance()

    def advance(self):
        self.tok_idx += 1
        if self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]
        return self.tok_idx

    def reverse(self, amount=1):
        self.tok_idx -= 1
        self.update_current_tok()
        return self.current_tok
    
    def update_current_tok(self):
        if self.tok_idx >= 0 and self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]

    def parse(self):
        res = self.statements()
        if not res.error and self.current_tok.type != TT_EOF:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected '+', '-', '*', or '/'"))
        return res

    ####
    
    def fun_expr(self):
        res = ParseResult()

        if not self.current_tok.matches(TT_KEYWORD, 'fun'):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected 'fun'"))
        
        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_IDENTIFIER:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected Function Name"))
        
        var_name_tok = self.current_tok
        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_LPAREN:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected '('"))

        res.register_advancement()
        self.advance()
        arg_name_toks = []

        if self.current_tok.type == TT_IDENTIFIER:
            arg_name_toks.append(self.current_tok)
            res.register_advancement()
            self.advance()

            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()

                if self.current_tok.type != TT_IDENTIFIER:
                    return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected Identifier"))
                
                arg_name_toks.append(self.current_tok)
                res.register_advancement()
                self.advance()

            if self.current_tok.type != TT_RPAREN:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected ')' or ','"))
        else:
            if self.current_tok.type != TT_RPAREN:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected Identifier, or ')'"))
            
        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_LCURL:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected '{'"))
        
        res.register_advancement()
        self.advance()

        node_to_return = res.register(self.statements())
        if res.error: return res

        if self.current_tok.type != TT_RCURL:
            return res.failure(InvalidSyntaxError(self.update_current_tok.pos_start, self.current_tok.pos_end, "Expected '}'"))

        res.register_advancement()
        self.advance()
        return res.success(FuncDefNode(var_name_tok, arg_name_toks, node_to_return, False))

    def while_expr(self):
        res = ParseResult()

        if not self.current_tok.matches(TT_KEYWORD, 'while'):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected 'while'"))

        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_LPAREN:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected '('"))

        res.register_advancement()
        self.advance()
        condition = res.register(self.expr())
        if res.error: return res

        if self.current_tok.type != TT_RPAREN:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected ')'"))

        res.register_advancement()
        self.advance()

        if not self.current_tok.type == TT_LCURL:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected '{'"))
        
        res.register_advancement()
        self.advance()

        body = res.register(self.statements())
        if res.error: return res

        if self.current_tok.type != TT_RCURL:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected '{'"))

        res.register_advancement()
        self.advance()
        return res.success(WhileNode(condition, body, True))

    def for_expr(self):
        res = ParseResult()

        if not self.current_tok.matches(TT_KEYWORD, 'for'):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected 'for'"))
        
        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_LPAREN:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected '('"))

        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_IDENTIFIER:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected Identifier"))
        
        var_name = self.current_tok
        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_EQ:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected '='"))
        
        res.register_advancement()
        self.advance()
        start_value = res.register(self.expr())
        if res.error: return res

        if self.current_tok.type != TT_COMMA:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected ','"))
        
        res.register_advancement()
        self.advance()
        end_value = res.register(self.expr())
        if res.error: return res

        if self.current_tok.type == TT_COMMA:
            res.register_advancement()
            self.advance()
            step_value = res.register(self.expr())
            if res.error: return res
        else:
            step_value = None

        if self.current_tok.type != TT_RPAREN:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected ')'"))

        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_LCURL:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected '{'"))

        res.register_advancement()
        self.advance()

        body = res.register(self.statements())
        if res.error: return res

        if self.current_tok.type != TT_RCURL:
            return res.failure(InvalidSyntaxError(self.update_current_tok.pos_start, self.current_tok.pos_end, "Expected '}'"))

        res.register_advancement()
        self.advance()
        return res.success(ForNode(var_name, start_value, end_value, step_value, body, True))

    def if_expr(self):
        res = ParseResult()
        all_cases = res.register(self.if_expr_cases('if'))
        if res.error: return res
        cases, else_case = all_cases
        return res.success(IfNode(cases, else_case))

    def elif_expr(self):
        return self.if_expr_cases('elif')
    
    def else_expr(self):
        res = ParseResult()
        else_case = None

        if self.current_tok.matches(TT_KEYWORD, 'else'):
            res.register_advancement()
            self.advance()

            if self.current_tok.type != TT_LCURL:
                return res.failure(InvalidSyntaxError(self.update_current_tok.pos_start, self.current_tok.pos_end, "Expected '{'"))

            res.register_advancement()
            self.advance()
            statements = res.register(self.statements())
            if res.error: return res
            else_case = (statements, True)

            if self.current_tok.type != TT_RCURL:
                return res.failure(InvalidSyntaxError(self.update_current_tok.pos_start, self.current_tok.pos_end, "Expected '}'"))
            
            res.register_advancement()
            self.advance()

        return res.success(else_case)
    
    def elif_or_else(self):
        res = ParseResult()
        cases, else_case = [], None

        if self.current_tok.matches(TT_KEYWORD, 'elif'):
            all_cases = res.register(self.elif_expr())
            if res.error: return res
            cases, else_case = all_cases
        else:
            else_case = res.register(self.else_expr())
            if res.error: return res

        return res.success((cases, else_case))

    def if_expr_cases(self, keyword):
        res = ParseResult()
        cases = []
        else_case = None

        if not self.current_tok.matches(TT_KEYWORD, keyword):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected {keyword}"))
        
        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_LPAREN:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected '('"))

        res.register_advancement()
        self.advance()
        condition = res.register(self.expr())
        if res.error: return res

        if self.current_tok.type != TT_RPAREN:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected ')'"))

        res.register_advancement()
        self.advance()

        if not self.current_tok.type == TT_LCURL:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected '{'"))

        res.register_advancement()
        self.advance()

        statements = res.register(self.statements())
        if res.error: return res
        cases.append((condition, statements, True))

        if self.current_tok.type != TT_RCURL:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected '}'"))
        
        res.register_advancement()
        self.advance()

        if self.current_tok.matches(TT_KEYWORD, 'elif') or self.current_tok.matches(TT_KEYWORD, 'else'):
            all_cases = res.register(self.elif_or_else())
            if res.error: return res
            new_cases, else_case = all_cases
            cases.extend(new_cases)
            return res.success((cases, else_case))

        return res.success((cases, None))

    def array(self):
        res = ParseResult()
        element_nodes = []
        pos_start = self.current_tok.pos_start.copy()

        if not self.current_tok != TT_LBRACK:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected '['"))
        
        res.register_advancement()
        self.advance()

        if self.current_tok == TT_RBRACK:
            res.register_advancement()
            self.advance()
        else:
            element_nodes.append(res.register(self.expr()))
            if res.error: 
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected '[', or Value"))

            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()
                element_nodes.append(res.register(self.expr()))
                if res.error: return res

            if self.current_tok.type != TT_RBRACK:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected ']' or ','"))
            
            res.register_advancement()
            self.advance()

        return res.success(ArrayNode(element_nodes, pos_start, self.current_tok.pos_end.copy()))
        
    def atom(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TT_INT, TT_FLOAT):
            res.register_advancement()
            self.advance()
            return res.success(NumberNode(tok))
        elif tok.type == TT_STRING:
            res.register_advancement()
            self.advance()
            return res.success(StringNode(tok))
        elif tok.type == TT_IDENTIFIER:
            res.register_advancement()
            self.advance()
            return res.success(VarAccessNode(tok))
        elif tok.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error:
                return res
            if self.current_tok.type == TT_RPAREN:
                res.register_advancement()
                self.advance()
                return res.success(expr)
            else:
                return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected ')'"))
        elif tok.type == TT_LBRACK:
            array = res.register(self.array())
            if res.error: return res
            return res.success(array) 
        elif tok.matches(TT_KEYWORD, 'if'):
            if_expr = res.register(self.if_expr())
            if res.error: return res
            return res.success(if_expr) 
        elif tok.matches(TT_KEYWORD, 'for'):
            for_expr = res.register(self.for_expr())
            if res.error: return res
            return res.success(for_expr) 
        elif tok.matches(TT_KEYWORD, 'while'):
            while_expr = res.register(self.while_expr())
            if res.error: return res
            return res.success(while_expr) 
        elif tok.matches(TT_KEYWORD, 'fun'):
            fun_expr = res.register(self.fun_expr())
            if res.error: return res
            return res.success(fun_expr) 
            
        return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected Keyword, Int, Float, Identifier, '+', '-', '(', or '['"))

    def call(self):
        res = ParseResult()

        atom = res.register(self.atom())
        if res.error: return res

        if self.current_tok.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            arg_nodes = []

            if self.current_tok.type == TT_RPAREN:
                res.register_advancement()
                self.advance()
            else:
                arg_nodes.append(res.register(self.expr()))
                if res.error: 
                    return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected Keyword, Int, Float, Identifier, '+', '-', '(', or ')'"))
                
                while self.current_tok.type == TT_COMMA:
                    res.register_advancement()
                    self.advance()
                    arg_nodes.append(res.register(self.expr()))
                    if res.error: return res

                if self.current_tok.type != TT_RPAREN:
                    return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"Expected ')' or ','"))
                
                res.register_advancement()
                self.advance()

            return res.success(CallNode(atom, arg_nodes))
        
        return res.success(atom)

    def power(self):
        return self.bin_op(self.call, (TT_POW, ), self.factor)

    def factor(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TT_PLUS, TT_MINUS):
            res.register_advancement()
            self.advance()
            factor = res.register(self.factor())
            if res.error:
                return res
            return res.success(UnaryOpNode(tok, factor))
        
        return self.power()

    def term(self):
        return self.bin_op(self.factor, (TT_MUL, TT_DIV, TT_MOD))
    
    def arith(self):
        return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

    def compare(self):
        res = ParseResult()

        if self.current_tok.type == TT_NOT:
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()

            node = res.register(self.compare())
            if res.error: return res
            return res.success(UnaryOpNode(op_tok, node))
        
        node = res.register(self.bin_op(self.arith, (TT_EE, TT_NE, TT_LT, TT_GT, TT_LTE, TT_GTE)))

        if res.error: return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected Int, Float, Identifier, '+', '-', '(', '[', or '!"))
        return res.success(node)

    def expr(self):
        res = ParseResult()

        if self.current_tok.matches(TT_KEYWORD, ('var', )):
            res.register_advancement()
            self.advance()

            if self.current_tok.type != TT_IDENTIFIER:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, 'Expected Identifier'))
            
            var_name = self.current_tok
            res.register_advancement()
            self.advance()

            if self.current_tok.type != TT_EQ:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected '='"))
            
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            return res.success(VarAssignNode(var_name, expr))
        #elif self.current_tok.type == TT_IDENTIFIER:
            if not global_symbol_table.get(self.current_tok.value):
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, f"'{self.current_tok.value}' not found"))
            
            var_name = self.current_tok

            res.register_advancement()
            self.advance()

            if self.current_tok.type != TT_EQ:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected '='"))
            
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            global_symbol_table.set(var_name.value, expr)
            return res.success(global_symbol_table.get(var_name.value))

        node = res.register(self.bin_op(self.compare, (TT_AND, TT_OR)))

        if res.error:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected Keyword, Int, Float, Identifier, '+', '-', '(', or '['"))

        return res.success(node)
    
    def state(self):
        res = ParseResult()
        pos_start = self.current_tok.pos_start.copy()

        if self.current_tok.matches(TT_KEYWORD, 'return'):
            res.register_advancement()
            self.advance()

            expr = res.try_register(self.expr())
            if not expr:
                self.reverse(res.to_reverse_count)
            return res.success(ReturnNode(expr, pos_start, self.current_tok.pos_start.copy()))
        elif self.current_tok.matches(TT_KEYWORD, 'continue'):
            res.register_advancement()
            self.advance()
            return res.success(ContinueNode(pos_start, self.current_tok.pos_start.copy()))
        elif self.current_tok.matches(TT_KEYWORD, 'break'):
            res.register_advancement()
            self.advance()
            return res.success(BreakNode(pos_start, self.current_tok.pos_start.copy()))
        
        expr = res.register(self.expr())
        if res.error: 
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected Keyword, Int, Float, Identifier, '+', '-', '(', or '['"))
        return res.success(expr)

    def statements(self):
        res = ParseResult()
        statements = []
        pos_start = self.current_tok.pos_start.copy()

        while self.current_tok.type == TT_SEMI:
            res.register_advancement()
            self.advance()

        statement = res.register(self.state())
        if res.error: return res
        statements.append(statement)

        more_statements = True

        while True:
            newline_count = 0
            while self.current_tok.type == TT_SEMI:
                res.register_advancement()
                self.advance()
                newline_count += 1
            if newline_count == 0:
                more_statements = False

            if not more_statements: break
            statement = res.try_register(self.state())
            if not statement:
                self.reverse(res.to_reverse_count)
                more_statements = False
                continue
            statements.append(statement)

        return res.success(ArrayNode(statements, pos_start, self.current_tok.pos_end.copy()))

    ####

    def bin_op(self, func_a, ops, func_b=None):
        if func_b == None:
           func_b = func_a
        res = ParseResult()
        left = res.register(func_a())

        if res.error:
            return res
        
        while self.current_tok.type in ops or (self.current_tok.type, self.current_tok.value) in ops:
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()
            right = res.register(func_b())

            if res.error:
                return res
            
            left = BinOpNode(left, op_tok, right)

        return res.success(left)

####RUNTIMERESULT####

class RTResult:
    def __init__(self):
        self.reset()

    def reset(self):
        self.value = None
        self.error = None
        self.func_return_value = None
        self.loop_should_continue = False
        self.loop_should_break = False

    def register(self, res):
        self.error = res.error
        self.func_return_value = res.func_return_value
        self.loop_should_continue = res.loop_should_continue
        self.loop_should_break = res.loop_should_break
        return res.value
    
    def success(self, value):
        self.reset()
        self.value = value
        return self
    
    def success_return(self, value):
        self.reset()
        self.func_return_value = value
        return self
    
    def success_continue(self):
        self.reset()
        self.loop_should_continue = True
        return self
    
    def success_break(self):
        self.reset()
        self.loop_should_break = True
        return self

    def failure(self, error):
        self.reset()
        self.error = error
        return self
    
    def should_return(self):
        return (self.error or self.func_return_value or self.loop_should_continue or self.loop_should_break)

####VALUES####

class Value:
    def __init__(self):
        self.set_pos()
        self.set_context()

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def set_context(self, context=None):
        self.context = context
        return self
    
    def added_to(self, other):
        return None, self.illegal_operation(other)
    
    def subbed_by(self, other):
        return None, self.illegal_operation(other)
    
    def multed_by(self, other):
        return None, self.illegal_operation(other)
    
    def dived_by(self, other):
        return None, self.illegal_operation(other)
    
    def powed_by(self, other):
        return None, self.illegal_operation(other)
    
    def modded_by(self, other):
        return None, self.illegal_operation(other)
    
    def comparison_ee(self, other):
        return None, self.illegal_operation(other)
    
    def comparison_ne(self, other):
        return None, self.illegal_operation(other)
    
    def comparison_lt(self, other):
        return None, self.illegal_operation(other)
    
    def comparison_gt(self, other):
        return None, self.illegal_operation(other)
    
    def comparison_lte(self, other):
        return None, self.illegal_operation(other)
    
    def comparison_gte(self, other):
        return None, self.illegal_operation(other)
    
    def anded_by(self, other):
        return None, self.illegal_operation(other)
    
    def ored_by(self, other):
        return None, self.illegal_operation(other)
    
    def notted(self):
        return None, self.illegal_operation()
    
    def execute(self, args):
        return None, self.illegal_operation()
    
    def copy(self):
        raise Exception('No copy method defined')
    
    def is_true(self):
        return False
    
    def illegalOperation(self, other=None):
        if not other: other = self
        return RTError(self.pos_start, self.pos_end, 'Illegal Operation', self.context)

class Number(Value):

    def __init__(self, value):
        super().__init__()
        self.value = value
        self.set_pos()
        self.set_context()

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self
    
    def set_context(self, context=None):
        self.context = context
        return self
    
    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self.pos_start, self.pos_end)
        
    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self.pos_start, self.pos_end)
        
    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self.pos_start, self.pos_end)
        
    def dived_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(other.pos_start, other.pos_end, 'Division by Zero', self.context)
            return Number(self.value / other.value).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self.pos_start, self.pos_end)
        
    def powed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value ** other.value).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self.pos_start, self.pos_end)
        
    def modded_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(other.pos_start, other.pos_end, 'Modular Division by Zero', self.context)
            return Number(self.value % other.value).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self.pos_start, self.pos_end)
        
    def comparison_ee(self, other):
        if isinstance(other, Number):
            return Number(int(self.value == other.value)).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self.pos_start, self.pos_end)
        
    def comparison_ne(self, other):
        if isinstance(other, Number):
            return Number(int(self.value != other.value)).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self.pos_start, self.pos_end)
        
    def comparison_lt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value < other.value)).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self.pos_start, self.pos_end)
        
    def comparison_gt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value > other.value)).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self.pos_start, self.pos_end)
        
    def comparison_lte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value <= other.value)).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self.pos_start, self.pos_end)
        
    def comparison_gte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value >= other.value)).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self.pos_start, self.pos_end)
        
    def anded_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value and other.value)).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self.pos_start, self.pos_end)
        
    def ored_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value or other.value)).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self.pos_start, self.pos_end)
        
    def notted(self):
        return Number(1 if self.value == 0 else 0).set_context(self.context), None
    
    def is_true(self):
        return self.value != 0

    def copy(self):
        copy = Number(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy
        
    def __repr__(self):
        return str(self.value)

Number.true = Number(1)
Number.false = Number(0)

class String(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value

    def added_to(self, other):
        if isinstance(other, String):
            return String(self.value + other.value).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self, other)
        
    def multed_by(self, other):
        if isinstance(other, Number):
            return String(self.value * other.value).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self, other)
        
    def comparison_ee(self, other):
        if isinstance(other, String):
            return Number(int(self.value == other.value)).set_context(self.context), None
        else:
            return None, Value.illegalOperation(self.pos_start, self.pos_end)
    
    def is_true(self):
        return len(self.value) > 0
    
    def __str__(self):
        return self.value

    def copy(self):
        copy = String(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy
    
    def __repr__(self):
        return f'"{self.value}"'

class Array(Value):
    def __init__(self, elements):
        super().__init__()
        self.elements = elements

    def added_to(self, other):
        new_list = self.copy()
        new_list.elements.append(other)
        return new_list, None
    
    def subbed_by(self, other):
        if isinstance(other, Number):
            new_list = self.copy()
            try:
                new_list.elements.pop(other.value)
                return new_list, None
            except:
                return None, RTError(other.pos_start, other.pos_end, 'Index out of range exception', self.context)
        else:
            return None, Value.illegalOperation(self, other)

    def multed_by(self, other):
        if isinstance(other, Array):
            new_list = self.copy()
            new_list.elements.extend(other.elements)
            return new_list, None
        else:
            return None, Value.illegalOperation(self, other)
        
    def dived_by(self, other):
        if isinstance(other, Number):
            try:
                return self.elements[other.value], None
            except:
                return None, RTError(other.pos_start, other.pos_end, 'Index out of range exception', self.context)
        else:
            return None, Value.illegalOperation(self, other)
    
    def copy(self):
        copy = Array(self.elements)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy
    
    def __str__(self):
        return f'{", ".join([str(x) for x in self.elements])}'
    
    def __repr__(self):
        return f'[{", ".join([str(x) for x in self.elements])}]'

class BaseFunction(Value):
    def __init__(self, name):
        super().__init__()
        self.name = name

    def generate_new_context(self):
        new_context = Context(self.name, self.context, self.pos_start)
        new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
        return new_context
    
    def check_args(self, arg_names, args):
        res = RTResult()
        if len(args) > len(arg_names):
            return res.failure(RTError(self.pos_start, self.pos_end, f"{len(args) - len(arg_names)} too many Arguments passed into '{self.name}", self.context))
        elif len(args) < len(arg_names):
            return res.failure(RTError(self.pos_start, self.pos_end, f"{len(arg_names) - len(args)} too few Arguments passed into '{self.name}", self.context))
        return res.success(None)
    
    def populate_args(self, arg_names, args, exec_ctx):
        for i in range(len(args)):
            arg_name = arg_names[i]
            arg_value = args[i]
            arg_value.set_context(exec_ctx)
            exec_ctx.symbol_table.set(arg_name, arg_value)

    def check_and_populate_args(self, arg_names, args, exec_ctx):
        res = RTResult()
        res.register(self.check_args(arg_names, args))
        if res.should_return(): return res
        self.populate_args(arg_names, args, exec_ctx)
        return res.success(None)

class Function(BaseFunction):
    def __init__(self, name, body_node, arg_names, should_auto_return):
        super().__init__(name)
        self.body_node = body_node
        self.arg_names = arg_names
        self.should_auto_return = should_auto_return

    def execute(self, args):
        res = RTResult()
        interpreter = Interpreter()
        new_context = self.generate_new_context()
        
        res.register(self.check_and_populate_args(self.arg_names, args, new_context))
        if res.should_return(): return res

        value = res.register(interpreter.visit(self.body_node, new_context))
        if res.should_return() and res.func_return_value == None: return res
        ret_value = (value if self.should_auto_return else None) or res.func_return_value or Number.false
        return res.success(ret_value)
    
    def copy(self):
        copy = Function(self.name, self.body_node, self.arg_names, self.should_auto_return)
        copy.set_context(self.context)
        copy.set_pos(self.pos_start, self.pos_end)
        return copy
    
    def __repr__(self):
        return f"<Function {self.name}>"

class BuiltInFunction(BaseFunction):
    def __init__(self, name):
        super().__init__(name)

    def execute(self, args):
        res = RTResult()
        exec_ctx = self.generate_new_context()

        method_name = f'execute_{self.name}'
        method = getattr(self, method_name, self.no_visit_method)

        res.register(self.check_and_populate_args(method.arg_names, args, exec_ctx))
        if res.should_return(): return res
        return_value = res.register(method(exec_ctx))
        if res.should_return(): return res
        return res.success(return_value)

    def no_visit_method(self, node, context):
        raise Exception(f'No execute_{self.name} method defined')
    
    def copy(self):
        copy = BuiltInFunction(self.name)
        copy.set_context(self.context)
        copy.set_pos(self.pos_start, self.pos_end)
        return copy
    
    def __repr__(self):
        return f"<built-in function {self.name}"
    
    ####

    def execute_Run(self, exec_ctx):
        fn = exec_ctx.symbol_table.get("fn")
        if not isinstance(fn, String):
            return RTResult().failure(RTError(self.pos_start, self.pos_end, "Argument must be a string", exec_ctx))

        fn = fn.value

        try:
            with open(fn, "r") as f:
                script = f.read()
        except Exception as e:
            return RTResult().failure(RTError(self.pos_start, self.pos_end, f"Failed to load script \"{fn}\"\n" + str(e), exec_ctx))
        
        _, error = run(fn, script)

        if error:
            return RTResult().failure(RTError(self.pos_start, self.pos_end, f"Failed to execute script \"{fn}\"\n" + error.as_string(), exec_ctx))
        
        return RTResult().success(Number.false)
    execute_Run.arg_names = ['fn']

    def execute_Print(self, exec_ctx):
        print(str(exec_ctx.symbol_table.get("value")))
        return RTResult().success(Number.false)
    execute_Print.arg_names = ['value']

    def execute_PrintRet(self, exec_ctx):
        return RTResult().success(String(str(exec_ctx.symbol_table.get("value"))))
    execute_PrintRet.arg_names = ['value']

    def execute_Input(self, exec_ctx):
        text = input()
        return RTResult().success(String(str(text)))
    execute_Input.arg_names = []

    def execute_InputInt(self, exec_ctx):
        while True:
            text = input()
            try:
                number = int(text)
                break
            except ValueError:
                print(f"'{text}' must be an integer")
        return RTResult().success(Number(number))
    execute_InputInt.arg_names = []

    def execute_InputFloat(self, exec_ctx):
        while True:
            text = input()
            try:
                number = float(text)
                break
            except ValueError:
                print(f"'{text}' must be a float or integer")
        return RTResult().success(Number(number))
    execute_InputFloat.arg_names = []

    def execute_Clear(self, exec_ctx):
        os.system('cls' if os.name == 'nt' else 'clear')
        return RTResult().success(Number.false)
    execute_Clear.arg_names = []

    def execute_IsNumber(self, exec_ctx):
        is_number = isinstance(exec_ctx.symbol_table.get("value"), Number)
        return RTResult().success(Number.true if is_number else Number.false)
    execute_IsNumber.arg_names = ['value']

    def execute_IsString(self, exec_ctx):
        is_number = isinstance(exec_ctx.symbol_table.get("value"), String)
        return RTResult().success(Number.true if is_number else Number.false)
    execute_IsString.arg_names = ['value']

    def execute_IsArray(self, exec_ctx):
        is_number = isinstance(exec_ctx.symbol_table.get("value"), Array)
        return RTResult().success(Number.true if is_number else Number.false)
    execute_IsArray.arg_names = ['value']

    def execute_IsFunction(self, exec_ctx):
        is_number = isinstance(exec_ctx.symbol_table.get("value"), BaseFunction)
        return RTResult().success(Number.true if is_number else Number.false)
    execute_IsFunction.arg_names = ['value']

    def execute_Append(self, exec_ctx):
        array = exec_ctx.symbol_table.get('array')
        value = exec_ctx.symbol_table.get('value')

        if not isinstance(array, Array):
            return RTResult().failure(RTError(self.pos_start, self.pos_end, "First argument must be an array", exec_ctx))
        
        new = array.copy()
        new.elements.append(value)
        array = new
        return RTResult().success(Number.false)
    execute_Append.arg_names = ['array', 'value']

    def execute_Extend(self, exec_ctx):
        array = exec_ctx.symbol_table.get('arrayOne')
        value = exec_ctx.symbol_table.get('arrayTwo')

        if not isinstance(array, Array):
            return RTResult().failure(RTError(self.pos_start, self.pos_end, "First argument must be an array", exec_ctx))
        
        if not isinstance(value, Array):
            return RTResult().failure(RTError(self.pos_start, self.pos_end, "Second argument must be an array", exec_ctx))
        
        new = array.copy()
        val_new = value.copy()
        new.elements.extend(val_new.elements)
        array = new
        return RTResult().success(Number.false)
    execute_Extend.arg_names = ['arrayOne', 'arrayTwo']

    def execute_Pop(self, exec_ctx):
        array = exec_ctx.symbol_table.get('array')
        value = exec_ctx.symbol_table.get('index')

        if not isinstance(array, Array):
            return RTResult().failure(RTError(self.pos_start, self.pos_end, "First argument must be an array", exec_ctx))
        
        if not isinstance(value, Number):
            return RTResult().failure(RTError(self.pos_start, self.pos_end, "Second argument must be an integer", exec_ctx))
        
        try:
            element = array.elements.pop(value.value)
        except:
            return RTResult().failure(RTError(self.pos_start, self.pos_end, "Index out of Range Exception", exec_ctx))

        return RTResult().success(element)
    execute_Pop.arg_names = ['array', 'index']

    def execute_Len(self, exec_ctx):
        array = exec_ctx.symbol_table.get('array')

        if not isinstance(array, Array):
            return RTResult().failure(RTError(self.pos_start, self.pos_end, "First argument must be an array", exec_ctx))
        
        return RTResult().success(Number(len(array.elements)))
    execute_Append.arg_names = ['array']

BuiltInFunction.Run = BuiltInFunction("Run")
BuiltInFunction.Print = BuiltInFunction("Print")
BuiltInFunction.PrintRet = BuiltInFunction("PrintRet")
BuiltInFunction.Input = BuiltInFunction("Input")
BuiltInFunction.InputInt = BuiltInFunction("InputInt")
BuiltInFunction.InputFloat = BuiltInFunction("InputFloat")
BuiltInFunction.Clear = BuiltInFunction("Clear")
BuiltInFunction.IsNumber = BuiltInFunction("IsNumber")
BuiltInFunction.IsString = BuiltInFunction("IsString")
BuiltInFunction.IsArray = BuiltInFunction("IsArray")
BuiltInFunction.IsFunction = BuiltInFunction("IsFunction")
BuiltInFunction.Append = BuiltInFunction("Append")
BuiltInFunction.Extend = BuiltInFunction("Extend")
BuiltInFunction.Pop = BuiltInFunction("Pop")
BuiltInFunction.Len = BuiltInFunction("Len")

####CONTEXT####

class Context:
    def __init__(self, display_name, parent=None, parent_entry_pos=None):
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None

####SYMBOLTABLE####

class SymbolTable:
    def __init__(self, parent=None):
        self.symbols = {}
        self.parent = parent

    def get(self, name):
        value = self.symbols.get(name, None)
        if value == None and self.parent:
            return self.parent.get(name)
        return value
    
    def set(self, name, value):
        self.symbols[name] = value

    def remove(self, name):
        del self.symbols[name]

####INTERPRETER####

class Interpreter:
    def visit(self, node, context):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node, context)
    
    def no_visit_method(self, node, context):
        raise Exception(f'No visit_{type(node).__name__} method defined')
    
    ####

    def visit_NumberNode(self, node, context):
        return RTResult().success(Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end))
    
    def visit_StringNode(self, node, context):
        return RTResult().success(String(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end))

    def visit_ArrayNode(self, node, context):
        res = RTResult()
        elements = []

        for element_node in node.element_nodes:
            elements.append(res.register(self.visit(element_node, context)))
            if res.should_return(): return res

        return res.success(Array(elements).set_context(context).set_pos(node.pos_start, node.pos_end))

    def visit_VarAccessNode(self, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name)

        if not value:
            return res.failure(RTError(node.pos_start, node.pos_end, f"'{var_name}' is not Defined", context))
        
        value = value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
        return res.success(value)
    
    def visit_VarAssignNode(self, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = res.register(self.visit(node.value_node, context))
        if res.should_return(): return res

        context.symbol_table.set(var_name, value)
        return res.success(value)

    def visit_BinOpNode(self, node, context):
        res = RTResult()
        left = res.register(self.visit(node.left_node, context))
        if res.should_return(): return res
        right = res.register(self.visit(node.right_node, context))
        if res.should_return(): return res

        if node.op_tok.type == TT_PLUS:
            result, error = left.added_to(right)
        elif node.op_tok.type == TT_MINUS:
            result, error = left.subbed_by(right)
        elif node.op_tok.type == TT_MUL:
            result, error = left.multed_by(right)
        elif node.op_tok.type == TT_DIV:
            result, error = left.dived_by(right)
        elif node.op_tok.type == TT_POW:
            result, error = left.powed_by(right)
        elif node.op_tok.type == TT_MOD:
            result, error = left.modded_by(right)
        elif node.op_tok.type == TT_EE:
            result, error = left.comparison_ee(right)
        elif node.op_tok.type == TT_NE:
            result, error = left.comparison_ne(right)
        elif node.op_tok.type == TT_LT:
            result, error = left.comparison_lt(right)
        elif node.op_tok.type == TT_GT:
            result, error = left.comparison_gt(right)
        elif node.op_tok.type == TT_LTE:
            result, error = left.comparison_lte(right)
        elif node.op_tok.type == TT_GTE:
            result, error = left.comparison_gte(right)
        elif node.op_tok.type == TT_AND:
            result, error = left.anded_by(right)
        elif node.op_tok.type == TT_OR:
            result, error = left.ored_by(right)

        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))

    def visit_UnaryOpNode(self, node, context):
        res = RTResult()
        number = res.register(self.visit(node.node, context))
        if res.should_return(): return res

        if node.op_tok.type == TT_MINUS:
            number, error = number.multed_by(Number(-1))
        elif node.op_tok.type == TT_NOT:
            number, error = number.notted()
        
        if res.should_return():
            return res.failure(error)
        else:
            return res.success(number.set_pos(node.pos_start, node.pos_end))
        
    def visit_IfNode(self, node, context):
        res = RTResult()

        for condition, expr, should_return_null in node.cases:
            condition_value = res.register(self.visit(condition, context))
            if res.should_return(): return res

            if condition_value.is_true():
                expr_value = res.register(self.visit(expr, context))
                if res.should_return(): return res
                return res.success(Number.false if should_return_null else expr_value)
            
        if node.else_case:
            expr, should_return_null = node.else_case
            expr_value = res.register(self.visit(expr, context))
            if res.should_return(): return res
            return res.success(Number.false if should_return_null else expr_value)
            
        return res.success(Number.false)
    
    def visit_ForNode(self, node, context):
        res = RTResult()
        elements = []

        start_value = res.register(self.visit(node.start_value_node, context))
        if res.should_return(): return res
        end_value = res.register(self.visit(node.end_value_node, context))
        if res.should_return(): return res
        i = start_value.value

        if node.step_value_node:
            step_value = res.register(self.visit(node.step_value_node, context))
            if res.should_return(): return res
        else:
            step_value = Number(1)

        if step_value.value >= 0:
            condition = lambda: i < end_value.value
        else:
            condition = lambda: i > end_value.value
        
        while condition():
            context.symbol_table.set(node.var_name_tok.value, Number(i))
            i += step_value.value
            value = res.register(self.visit(node.body_node, context))

            if res.should_return() and res.loop_should_continue == False and res.loop_should_break == False: return res

            if res.loop_should_continue: continue
            if res.loop_should_break: break

            elements.append(value)

        return res.success(Number.false if node.should_return_null else Array(elements).set_context(context).set_pos(node.pos_start, node.pos_end))
    
    def visit_WhileNode(self, node, context):
        res = RTResult()
        elements = []

        while True:
            condition = res.register(self.visit(node.condition_node, context))
            if res.should_return(): return res
            if not condition.is_true(): break
            value = res.register(self.visit(node.body_node, context))
            if res.should_return() and res.loop_should_continue == False and res.loop_should_break == False: return res
            if res.loop_should_continue: continue
            if res.loop_should_break: break
            elements.append(value)

        return res.success(Number.false if node.should_return_null else Array(elements).set_context(context).set_pos(node.pos_start, node.pos_end))
    
    def visit_FuncDefNode(self, node, context):
        res = RTResult()
        func_name = node.var_name_tok.value
        body_node = node.body_node
        arg_names = [arg_name.value for arg_name in node.arg_name_toks]
        func_value = Function(func_name, body_node, arg_names, node.should_auto_return).set_context(context).set_pos(node.pos_start, node.pos_end)
        context.symbol_table.set(func_name, func_value)
        return res.success(func_value)
    
    def visit_CallNode(self, node, context):
        res = RTResult()
        args = []
        value_to_call = res.register(self.visit(node.node_to_call, context))
        if res.should_return(): return res
        value_to_call = value_to_call.copy().set_pos(node.pos_start, node.pos_end)

        for arg_node in node.arg_nodes:
            args.append(res.register(self.visit(arg_node, context)))
            if res.should_return(): return res

        return_value = res.register(value_to_call.execute(args))
        if res.should_return(): return res
        return_value = return_value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
        return res.success(return_value)

    def visit_ReturnNode(self, node, context):
        res = RTResult()

        if node.node_to_return:
            value = res.register(self.visit(node.node_to_return, context))
            if res.should_return(): return res
        else:
            value = Number.false

        return res.success_return(value)
    
    def visit_ContinueNode(self, node, context):
        return RTResult().success_continue()
    
    def visit_BreakNode(self, node, context):
        return RTResult().success_break()

####RUN####

global_symbol_table = SymbolTable()
global_symbol_table.set("true", Number.true)
global_symbol_table.set("false", Number.false)

global_symbol_table.set("Run", BuiltInFunction.Run)
global_symbol_table.set("Print", BuiltInFunction.Print)
global_symbol_table.set("PrintRet", BuiltInFunction.PrintRet)
global_symbol_table.set("Input", BuiltInFunction.Input)
global_symbol_table.set("InputInt", BuiltInFunction.InputInt)
global_symbol_table.set("InputFloat", BuiltInFunction.InputFloat)
global_symbol_table.set("Clear", BuiltInFunction.Clear)
global_symbol_table.set("IsNum", BuiltInFunction.IsNumber)
global_symbol_table.set("IsStr", BuiltInFunction.IsString)
global_symbol_table.set("IsArray", BuiltInFunction.IsArray)
global_symbol_table.set("IsFun", BuiltInFunction.IsFunction)
global_symbol_table.set("Append", BuiltInFunction.Append)
global_symbol_table.set("Extend", BuiltInFunction.Extend)
global_symbol_table.set("Pop", BuiltInFunction.Pop)
global_symbol_table.set("Len", BuiltInFunction.Len)

def run(fn, text):
    #Generate Tokens
    lexer = Lexer(fn, text)
    tokens, error = lexer.make_tokens()
    if error:
        return None, error
    #print(tokens)

    #Generate AST (Abstract System Tree)
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error:
        return None, ast.error
    #print(ast)
    
    #Run Program
    interpreter = Interpreter()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    result = interpreter.visit(ast.node, context)

    return result.value, result.error