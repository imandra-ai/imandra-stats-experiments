import re

def get_terms(s):
    s = s.strip()
    split = re.split('([+-])', s)
    if split[0] == '':
        split = split[1:]
    else:
        split = ['+'] + split
    
    terms = {}
    for sign , term in zip(*[iter(split)]*2):
        sign = {'+':1, '-':-1}[sign]
        if '*' in term:
            coeff, var = term.split('*')
        else:
            coeff, var = 1, term
        coeff = sign * float(coeff)
        terms[var.strip().rstrip()] = coeff
    return terms

def parse_constraints(constraints, variables):
    aret = {v:[] for v in variables}
    bret = []
    for c in constraints.splitlines():
        c = c.strip()
        if c == '': continue
        if '<=' in c:
            gsign = 1
        elif '>=' in c:
            gsign = -1
        a, b = re.split('[><]=', c)
        a, b = get_terms(a) , gsign * float(b)
        bret.append(b)
        for var in aret:
            aret[var].append(gsign * a.get(var,0.0))
    return aret , bret