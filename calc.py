# -----------------------------------------------------------------------------
# calc.py 
#
# A calculator parser that makes use of closures. The function make_calculator()
# returns a function that accepts an input string and returns a result.  All
# lexing rules, parsing rules, and internal state are held inside the function.
# Extended to accept lists for this homework assignment 
# -----------------------------------------------------------------------------
def make_calculator():
    import ply.lex as lex
    import ply.yacc as yacc

    # ------- Internal calculator state

    variables = {}       # Dictionary of stored variables

    # ------- Calculator tokenizing rules

    # We define regular tokens (here), and literal tokens (below)
    # regular tokens come with some value or auxiliary data that is not as trivial as a literal (like being set to "hey")
    # ------- Calculator tokenizing rules
    tokens = (
        'NAME', 'NUMBER',
    )
    literals = ['=', '+', '-', '*', '/', '(', ')','%',',',]
    t_ignore = " \t"

    # A name can be any sequence of letters and digits, starting with a letter or underscore
    def t_NAME(t):
        r'[a-zA-Z_][a-zA-Z0-9_]*'
        return t

    # A number is a sequence of digits
    def t_NUMBER(t):
        r'\d+'
        t.value = int(t.value)
        return t
    
    # Ignore newlines
    def t_newline(t):
        r'\n+'
        t.lexer.lineno += len(t.value)

    # Error handling
    def t_error(t):
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    # Build the lexer
    lexer = lex.lex()

    # ------- Calculator parsing rules

    # Define the precedence rules for operators
    precedence = (
        ('left','(', ')', '+', '-',),
        ('left', '*', '/', '%',','),
        ('right', 'UMINUS',),
    )

    # Evaluate an expression
    def p_statement_expr(p):
        'statement : expression'
        p[0] = p[1]
    
    # Assign a value to a variable
    def p_statement_assign(p):
        '''statement : NAME "=" expression'''
        variables[p[1]] = p[3]
        p[0] = variables[p[1]]

    # Evaluate a binary operation
    def p_expression_binop(p):
        '''expression : expression '+' expression
                      | expression '-' expression
                      | expression '*' expression
                      | expression '/' expression
                      | expression '%' expression
                      | expression '/' '/' expression'''
        # First we check if the input is a touple if so this will scrape the ints and return a touple of the full input
        p[0] = []
        for i in range(0, len(p)):
            if isinstance(p[i], int):
                p[0].append(p[i])
            elif isinstance(p[i], tuple):
                p[0].extend(extract_numbers(p[i]))
            else:
                p[0] = p[0]
        p[0] = tuple(p[0])   
        #Check if p[2] is just an int for binary op else is touple for binary op zip
        if p[2] == '+' and isinstance(p[1],int):
            p[0] = p[1] + p[3]
        elif p[2] == '+':
            p[0] = tuple(x + y for half_tup1, half_tup2 in zip(splitTup(p[0]), nextTup(splitTup(p[0]))) for x, y in zip(half_tup1, half_tup2))
        if p[2] == '-' and isinstance(p[1],int):
            p[0] = p[1] - p[3]
        elif p[2] == '-':
            p[0] = tuple(x - y for half_tup1, half_tup2 in zip(splitTup(p[0]), nextTup(splitTup(p[0]))) for x, y in zip(half_tup1, half_tup2))
        elif p[2] == '*' and isinstance(p[1],int):
            p[0] = p[1] * p[3]
        elif p[2] == '*':
            p[0] = tuple(x * y for half_tup1, half_tup2 in zip(splitTup(p[0]), nextTup(splitTup(p[0]))) for x, y in zip(half_tup1, half_tup2))
        elif p[2] == '/':
            if p[3] == '/' and isinstance(p[1],int):
                p[0] = p[1] // p[4]
            elif p[3] == '/':
                p[0] = tuple(x // y for half_tup1, half_tup2 in zip(splitTup(p[0]), nextTup(splitTup(p[0]))) for x, y in zip(half_tup1, half_tup2))
            else:
                if isinstance(p[1],int):
                    p[0] = p[1] / p[3]
                else:
                    p[0] = tuple(x / y for half_tup1, half_tup2 in zip(splitTup(p[0]), nextTup(splitTup(p[0]))) for x, y in zip(half_tup1, half_tup2))
        elif p[2] == '%':
            p[0] = tuple(x % y for half_tup1, half_tup2 in zip(splitTup(p[0]), nextTup(splitTup(p[0]))) for x, y in zip(half_tup1, half_tup2))

    def p_expr_list(p):
        'expression : list'
        p[0] = p[1]
                
    # Negates an expression
    def p_expression_uminus(p):
        "expression : '-' expression %prec UMINUS"
        p[0] = -p[2]

    # An expression may consist of an expression within parenthesis
    def p_expression_group(p):
        "expression : '(' expression ')'"
        p[0] = p[2]

    # An expression may consist of an expression within parenthesis
    def p_list(p):
        '''list : '(' expression ',' 
                | list expression ','
                | list expression ')'
                | list ')'
                | list ',' ')' 
                | '(' ')' '''
        #Check for a list if so extract numbers into an initial list value for p[0] and make p[0] a list that can hold numbers
        tCom = False
        if p[len(p) - 1] == ',':
            tCom = True
        p[0] = []
        for i in range(0, len(p)):
            if isinstance(p[i], int):
                p[0].append(p[i])
            elif isinstance(p[i], tuple):
                p[0].extend(extract_numbers(p[i]))
            else:
                p[0] = p[0]
        p[0] = tuple(p[0])
        if tCom == True:
            p[0] = add_com(p[0])

    # Function to clean the list into a tuple which can be operated on
    # Search through an input object and take out any instance of int and
    def extract_numbers(t):
        numbers = []
        for i in t:
            if isinstance(i, int): 
                numbers.append(i)
            elif isinstance(i, tuple):
                numbers.extend(extract_numbers(i))
        return numbers
    
    #Functions to take in the parsed input and return two tuples representing each
    def splitTup(tup):
        mid = len(tup) // 2
        tup1 = tup[:mid]
        tup2 = tup[mid:]
        return tup1, tup2
    
    def nextTup(tup):
        _, nextT = splitTup(tup)
        return nextT
    
    #If terminal comma send through this function to add to terminal comma
    def add_com(tup):
        result = tup + (',',)
        return result
    #Expression may be a number
    def p_expression_number(p):
        "expression : NUMBER"
        p[0] = p[1]

    #Expression may be a name, and  may not have been defined yet
    def p_expression_name(p):
        '''expression : NAME'''
        try:
            p[0] = variables[p[1]]
        except LookupError:
            print("Undefined name '%s'" % p[1])
            p[0] = 0

    def p_error(p):
        if p:
            print("Syntax error at '%s'" % p.value)
        else:
            print("Syntax error at EOF")
            
    # Build the parser
    parser = yacc.yacc()

    # ------- Input function

    def input(text):
        result = parser.parse(text, lexer=lexer)
        return result

    return input

# Make a calculator object and use it
calc = make_calculator()

while True:
    try:
        s = input("")
    except EOFError:
        break
    r = calc(s)
    if r:
        print(r)
