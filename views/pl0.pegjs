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

block = c:(decl_const)? v:(decl_var)? pr:(proc)? sts:st
        {
           return {
              type: 'BLOCK',
              consts: c,
              vars: v,
              procs: pr,
              sts: st
           };
        }

decl_const = CONST i:ID ASSIGN n:NUMBER c:(COMA ID ASSIGN NUMBER)* PTO_COMA
             {
                var result = [{type: '=', left: i, right: n}];
                for (var x = 0; x < c.length; x++)
                   result.push({type: '=', left: c[x][1], right: c[x][3]});
                
                return result;
             }

decl_var = VAR i:ID v:(COMA i:ID)* PTO_COMA
           {
              var result = [{type: 'VAR', value: i}];
              for (var x = 0; x < v.length; x++)
                 result.push({type: 'VAR', value: v[x][1]});
              return result;
           }

proc = pr:(PROC ID args? PTO_COMA block PTO_COMA)*
       {
          var result = [];
          for (var x = 0; x < pr.length; x++)
             result.push({type: 'PROCEDURE', id: pr[x][1], arguments: pr[x][2], block: p[x][4]});
          return result;
       }

args = LEFTPAR i:ID ids:(COMA ID)* RIGHTPAR
       {
          var result = [i];
          for (var x = 0; x < ids.length; x++)
             result.push({ids[x][1]});
          return result;
        }

cond = ODD e:exp
          {return {type: 'ODD', expression: e};}
     / e1:exp c:COMP e2:exp
          { return {type: c, left: e1, right: e2}; }
        
st     = i:ID ASSIGN e:exp            
           { return {type: '=', left: i, right: e}; }
       / CALL i:ID
           {
              return {
                 type: 'CALL',
                 id: i
              };
           }
       / BEGIN l:st r:(PTO_COMA st)* END
           {
              var result = [l]
              for (var i = 0; i < r.length; i++)
                 result.push(r[i][1]);
         }
       / IF e:cond THEN st:st ELSE sf:st
           {
             return {
               type: 'IFELSE',
               c:  e,
               st: st,
               sf: sf
             };
           }
       / IF e:cond THEN st:st    
           {
             return {
               type: 'IF',
               c:  e,
               st: st
             };
           }
       / WHILE e:cond DO st:st
           {
              return {
                 type: 'WHILE',
                 c: e,
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
COMP     = _ op:$([<>=!][=]/[<>]) _ {return op;}
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
CALL     = _ "call" _
BEGIN    = _ "begin" _
END      = _ "end" _
ODD      = _ "odd" _
WHILE    = _ "while" _
DO       = _ "do" _
ID       = _ id:$([a-zA-Z_][a-zA-Z_0-9]*) _ 
            { 
              return { type: 'ID', value: id }; 
            }
NUMBER   = _ digits:$[0-9]+ _ 
            { 
              return { type: 'NUM', value: parseInt(digits, 10) }; 
            }
