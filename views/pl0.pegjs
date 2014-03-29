{
  var tree = function(f, r) {
    if (r.length > 0) {
      var last = r.pop();
      var result = {
        type: last[0],
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

program = b:block PTO
             { return {
                type: "PROGRAM",
                bloque: b };
             }

block = c:(cons)? v:(var)? p:(proc)* s:st
             {
                var result = new Array()
                if (c){
                   result.push({type: "CONST", consts: c})
                }
                if (v){
                   result.push({type: "VAR", vars: v})
                }
                if (p) {
                   result.push(p)
                }
                if (s){
                   result.push(s)
                }
                return result
             }

cons = CONST u:assin c:(COMA (assin))* PTO_COMA
             {
                var result = new Array();
                result.push(u)
                for ( var i = 0; i < c.length; i++)
                   result.push(c[i][1]);
                return result;
             }

var = VAR id:ID i:(COMA ID)* PTO_COMA
             {
                var result = new Array();
                r.push(id);
                for ( var k = 0; k < i.length; k++)
                   result.push(i[k][1]);
                return result;
             }

proc = PROCEDURE id:ID a:(arg)? PTO_COMA b:block PTO_COMA
             {
                if (a)
                   return { type: "PROCEDURE", name: id, argumentos: a, subrutina: b[1]}
                else
                   return { type: "PROCEDURE", name: id, subrutina: b[1]}
             }

assin = i:ID ASSIGN e:exp {return {type: '=', left: i, right: e}; }

arg = LEFTPAR e:exp a:(COMA exp)* RIGHTPAR
             {
                var result = new Array();
                result.push(e);
                for (var i = 0; i < a.length; i++)
                   result.push(a[i][1]);
                return {lista: result}
             }

st = CALL r:ID a:(arg)?
             { return {type:'CALL', argumentos: a, right: r }; }
   / i:ID ASSIGN e:exp
             { return {type: '=', left: i, right: e}; }
   / IF e:cond THEN st:st ELSE sf:st
             {
                return {
                   type: 'IFELSE',
                   c: e,
                   st: st,
                   sf: sf,
                };
             }
   / IF e:cond THEN st:st
             {
                return {
                   type: 'IF',
                   c: e,
                   st: st
                };
             }
   / BEGIN stt:st ar:(PTO_COMA st)* END
             {
                var result = [stt];
                for ( var k = 0; k < ar.length; k++)
                   result.push(ar[k][1]);
                return result;
             }
   / WHILE c:cond DO s:st
             {
                return {
                   type: 'WHILE',
                   condicion: c,
                   stat: s
                };
             }

cond = ODD e:exp
             {
                return {
                   type: 'ODD',
                   cond: e
                }
             }
     / e:exp t:COMPARISON ee:exp
             {
                return {
                   type: t,
                   left: e,
                   right: ee
                };
             }
             
exp = t:term r:(ADD term)* { return tree(t,r); }

term = f:factor r:(MUL factor)* { return tree(f,r); }

factor = NUMBER
       / ID
       / LEFTPAR t:exp RIGHTPAR { return t; }

_ = $[ \t\n\r]*

COMPARISON = _ op:('==' / [#|<|<=|>|>=]) _ { return op; }
ASSIGN = _ op:'=' _ { return op; }
ADD = _ op:[+-] _ { return op; }
MUL = _ op:[*/] _ { return op; }
PTO = _"."_
PTO_COMA = _";"_
COMA = _","_
LEFTPAR = _"("_
RIGHTPAR = _")"_
VAR = _"var"_
CONST = _"const"_
PROCEDURE= _"procedure"_
IF = _ "if" _
THEN = _ "then" _
ELSE = _ "else" _
CALL = _ "call" _
BEGIN = _ "begin" _
END = _ "end" _
WHILE = _ "while" _
DO = _ "do" _
ODD = _ "odd" _
ID = _ id:$([a-zA-Z_][a-zA-Z_0-9]*) _
            {
              return { type: 'ID', value: id };
            }
NUMBER = _ digits:$[0-9]+ _
            {
              return { type: 'NUM', value: parseInt(digits, 10) };
            }