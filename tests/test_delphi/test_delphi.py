
""" original file had camelCase identifiers that aren't supported in lark
import re

def decamel(m):
    return m.group(1)+'_'+m.group(2).lower()

with open('delphi.g', 'r+') as r:
    with open('delphi.g2', 'wt') as w:
        for row in r:
            row= re.sub('([a-z]+)([A-Z])', decamel, row)
            w.write(row)
"""

from lark import Lark
parser = Lark(open('../../lark/grammars/delphi.g').read(), start='file')
_=parser.parse(open('cclasses.pas').read())
print( _.pretty() )