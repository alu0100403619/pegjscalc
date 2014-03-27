/*
 * Classic example grammar, which recognizes simple arithmetic expressions like
 * "2*(3+4)". The parser generated from this grammar then AST.
 */

{
  var tree = function(f, r) {
    if (r.length > 0) {
      var last = r.pop();
      var result = {
        type:  last[0],
        left: tree(f, r),
        right: last[1]
      };
    }
    else {
      var result = f;
    }
    return result;
  }
}

program = bl:block PUNTO
          { return {type: 'program', block: b}; }

block = (c:declaracion_constante)? (v:declaracion_variable)? (pr:(proc)*)? st
        {
           return {
              type: 'BLOCK',
              consts: c,
              vars: v,
              procs: pr,
              sts: st
           };
        }

declaracion_constante = CONST i:ID ASSIGN n:NUMBER (COMA i:ID ASSIGN n:NUMBER)* PTO_COMA
                        { return {type: 'CONST', ids: i, values: n};}

declaracion_variable = VAR i:ID (COMA i:ID)* PTO_COMA
                       {return {type: 'VAR', vars: i};}

proc = PROC ID PTO_COMA block PTO_COMA

st     = i:ID ASSIGN e:exp            
            { return {type: '=', left: i, right: e}; }
       / IF e:exp THEN st:st ELSE sf:st
           {
             return {
               type: 'IFELSE',
               c:  e,
               st: st,
               sf: sf
             };
           }
       / IF e:exp THEN st:st    
           {
             return {
               type: 'IF',
               c:  e,
               st: st
             };
           }
exp    = t:term   r:(ADD term)*   { return tree(t,r); }
term   = f:factor r:(MUL factor)* { return tree(f,r); }

factor = NUMBER
       / ID
       / LEFTPAR t:exp RIGHTPAR   { return t; }

_ = $[ \t\n\r]*

ASSIGN   = _ op:'=' _  { return op; }
ADD      = _ op:[+-] _ { return op; }
MUL      = _ op:[*/] _ { return op; }
LEFTPAR  = _"("_
RIGHTPAR = _")"_
PUNTO    = _ '.' _
COMA     = _ ',' _
PTO_COMA = _ ';' _
CONST    = _ "const" _
VAR      = _ "var" _
PROC     = _ "procedure" _
IF       = _ "if" _
THEN     = _ "then" _
ELSE     = _ "else" _
ID       = _ id:$([a-zA-Z_][a-zA-Z_0-9]*) _ 
            { 
              return { type: 'ID', value: id }; 
            }
NUMBER   = _ digits:$[0-9]+ _ 
            { 
              return { type: 'NUM', value: parseInt(digits, 10) }; 
            }
