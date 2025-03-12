open CS17SetupRackette;
open Read.Reader;
open Types;



/* deNameE: expression => name 
Input: a NameE() expression id
Output: the name content inside () of NameE() */

let deNameE: expression => name = id => switch(id) {
  | NameE(s) => s
  | _ => failwith("domain")
};

checkExpect(deNameE(NameE(Name("x"))), Name("x"), "deNameE works")
checkExpect(deNameE(NameE(Name("alod"))), Name("alod"), "deNameE works")

/* 
parseExpression: concreteProgramPiece => expression
input: a concreteProgramPiece input
output: the expression 'equivalent' of the concreteProgramPiece */

let rec parseExpression: concreteProgramPiece => expression =
  input => switch(input) {
  | NumberC(i) => NumE(i)
  | SymbolC("true") => BoolE(true)
  | SymbolC("false") => BoolE(false)
  | SymbolC("empty") => EmptyE
  | ListC([SymbolC("if"), cPP1, cPP2, cPP3]) => IfE({boolExpr: parseExpression(cPP1), trueExpr: parseExpression(cPP2), falseExpr:parseExpression(cPP3)})
  | ListC([SymbolC("if"), ... _]) => failwith("invalid input: if should have three expressions after it")
  | ListC([SymbolC("or"), cPP1, cPP2]) => OrE(parseExpression(cPP1), parseExpression(cPP2))
  | ListC([SymbolC("cond"), ... tl]) => 
    let condHelper: concreteProgramPiece => condData = 
    cPP => switch(cPP) {
     | ListC([cPP1, cPP2]) => {conditionExpr: parseExpression(cPP1), resultExpr: parseExpression(cPP2)}
     | _ => failwith("domain")
     }
 CondE(List.map(condHelper, tl)) 
  | ListC([SymbolC("and"), cPP1, cPP2]) => AndE(parseExpression(cPP1), parseExpression(cPP2))
  | ListC([SymbolC("and"), ... _]) => failwith("error: and should only have two arguments")
  | ListC([SymbolC("lambda"), lst, cPPBody]) => 
    let lambdaHelper: concreteProgramPiece => list(name) = cPP => switch(cPP) {
      | ListC(listId) => List.map(deNameE, List.map(parseExpression, listId))
      | _ => failwith("domain")};
    LambdaE({nameList: lambdaHelper(lst), lambdaBody: parseExpression(cPPBody)})
  | ListC([SymbolC("lambda"), ... _]) => failwith("invalid input: lambda must have a list of names and a body expression")
  | ListC([SymbolC("let"), ListC(lst), cPPBody]) => 
    let letHelper: concreteProgramPiece => letPair = cPP => switch(cPP) {
      | ListC([SymbolC(s), cPP]) => {pairName: Name(s), pairExpr: parseExpression(cPP)}
      | _ => failwith("domain")
      }
    LetE({letPairs: List.map(letHelper, lst), letBody: parseExpression(cPPBody)})
  | ListC([SymbolC("let"), ... _]) => failwith("invalid input: let must have a list of name-'expr' 'binding pairs' 
    and a body expression")
  | ListC([ListC([SymbolC("lambda"), lst, cPPBody]), ... tl]) => 
    let lambdaHelper: concreteProgramPiece => list(name) = cPP => switch(cPP) {
      | ListC(listId) => List.map(deNameE, List.map(parseExpression, listId))
      | _ => failwith("domain")};
  ApplicationE([LambdaE({nameList: lambdaHelper(lst), lambdaBody: parseExpression(cPPBody)}), ... List.map(parseExpression, tl)])
  | ListC([SymbolC(s), ... tl]) => ApplicationE([parseExpression(SymbolC(s)), ... List.map(parseExpression, tl)])
  | SymbolC(s) => switch(s) {
    | "define" => failwith("keyword")
    | "if" => failwith("keyword")
    | "cond" => failwith("keyword")
    | "and" => failwith("keyword")
    | "lambda" => failwith("keyword")
    | "let" => failwith("keyword")
    | "or" => failwith("keyword")
    | "empty" => failwith("keyword")
    | "true" => failwith("keyword")
    | "false" => failwith("keyword")
    | _ => NameE(Name(s))}  
  | _ => failwith("invalid input")
};


checkExpectExpression(parseExpression(NumberC(5)), NumE(5), "parseExpression works on numbers")



let aIfECPP = ListC([SymbolC("if"), ListC([SymbolC(">="), SymbolC("x"), NumberC(3)]), NumberC(10), NumberC(3)]);

checkExpectExpression(parseExpression(aIfECPP), 
IfE({boolExpr: ApplicationE([NameE((Name(">="))), NameE((Name("x"))), NumE(3)]),
     trueExpr: NumE(10), falseExpr: NumE(3)}), "parseExpression works on ifs");

let aAndECPP = ListC([SymbolC("and"), ListC([SymbolC(">"), NumberC(5), NumberC(3)]), ListC([SymbolC(">"), NumberC(4), NumberC(2)])]);

checkExpectExpression(parseExpression(aAndECPP), AndE(ApplicationE([NameE((Name(">"))), NumE(5), NumE(3)]),
 ApplicationE([NameE((Name(">"))), NumE(4), NumE(2)])), "parseExpression works on ands")



let aCondECPP = ListC([SymbolC("cond"),  ListC([ListC([SymbolC(">"), SymbolC("x"), 
NumberC(3)]),  ListC([SymbolC("+"), SymbolC("x"), NumberC(3)])])]);

checkExpectExpression(parseExpression(aCondECPP), CondE([{conditionExpr:
         ApplicationE([NameE((Name(">"))), NameE((Name("x"))), NumE(3)]),
        resultExpr:
         ApplicationE([NameE((Name("+"))), NameE((Name("x"))), NumE(3)])}]), "parseExpression works on conds");



let aLambdaECPP = ListC([SymbolC("lambda"), ListC([SymbolC("x"), SymbolC("y")]), ListC([SymbolC("+"), SymbolC("x"), SymbolC("y")])]);

checkExpectExpression(parseExpression(aLambdaECPP),
LambdaE({nameList: [Name("x"), Name("y")],
         lambdaBody:
          ApplicationE([NameE((Name("+"))), NameE((Name("x"))),
                        NameE((Name("y")))])}), "parseExpression works on Lambda exprs")


let aLambdaAppCPP = ListC([ListC([SymbolC("lambda"), 
ListC([SymbolC("x"), SymbolC("y")]), ListC([SymbolC("+"), SymbolC("x"), SymbolC("y")])]), NumberC(2), NumberC(3)])

checkExpectExpression(parseExpression(aLambdaAppCPP), 
ApplicationE([LambdaE({nameList: [Name("x"), Name("y")],
                       lambdaBody:
                        ApplicationE([NameE((Name("+"))), NameE((Name("x"))),
                                      NameE((Name("y")))])}),
              NumE(2), NumE(3)]), "parseExpression works on Lambda exprs")




/* parseDefinition: concreteProgramPiece => definition
input: a concrete program piece input starting with SymbolC("define"), input
output: a definition formed from the contents of the cPP
*/

let parseDefinition: concreteProgramPiece => definition =
  input => switch(input) {
  | ListC([SymbolC("define"), SymbolC(s), cPP]) => switch(s) {
    | "define" => failwith("keyword")
    | "if" => failwith("keyword")
    | "cond" => failwith("keyword")
    | "and" => failwith("keyword")
    | "lambda" => failwith("keyword")
    | "let" => failwith("keyword")
    | "or" => failwith("keyword")
    | "empty" => failwith("keyword")
    | "true" => failwith("keyword")
    | "false" => failwith("keyword")
    | _ => (Name(s), parseExpression(cPP))
  }
  | _ => failwith("invalid input: parseDefinition expects cPPs of type ListC([SymbolC('define'), SymbolC(s), cPP])")
  }
  
checkExpectDefinition(parseDefinition(ListC([SymbolC("define"), SymbolC("x"), NumberC(5)])), (Name("x"), NumE(5)), 
"parseDefinition works")

checkExpectDefinition(parseDefinition(ListC([SymbolC("define"), SymbolC("y"), SymbolC("true")])), 
(Name("y"), BoolE(true)), "parseDefinition works")



/* parsePiece: concreteProgramPiece => abstractProgramPiece 
input: either a Definition or Expression form cPP, input
output: Definition or Expression respectively applied to parseDef or parseExp applied to either depending on data type */

let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), SymbolC(s), cPP]) => Definition(parseDefinition(input))
    | _ => Expression(parseExpression(input))
    };

checkExpectAbstractProgramPiece(parsePiece(ListC([SymbolC("define"), SymbolC("x"), NumberC(5)])), 
 Definition((Name("x"), NumE(5))), "parsePiece parses definitions")

checkExpectAbstractProgramPiece(parsePiece(aIfECPP), 
Expression((IfE({boolExpr:
                  ApplicationE([NameE((Name(">="))), NameE((Name("x"))),
                                NumE(3)]),
                 trueExpr: NumE(10), falseExpr: NumE(3)}))), "parsePiece parses ifs")


/*parse: concreteProgram => abstractProgram 
Input: a concrete program input
Output: the abstract program consisting of parsePiecing every element of input*/

let parse: concreteProgram => abstractProgram =
  input =>
    /* this will parse all of the pieces of this program,
     * giving us a list of pieces, our abstract syntax */
    List.map(parsePiece, input);

checkExpectAbstractProgram(parse([aIfECPP]),
[Expression((IfE({boolExpr:
                   ApplicationE([NameE((Name(">="))), NameE((Name("x"))),
                                 NumE(3)]),
                  trueExpr: NumE(10), falseExpr: NumE(3)})))], "parse parses ifs")



checkExpectAbstractProgram(parse([aLambdaECPP]), 
[Expression((LambdaE({nameList: [Name("x"), Name("y")],
                      lambdaBody:
                       ApplicationE([NameE((Name("+"))), NameE((Name("x"))),
                                     NameE((Name("y")))])})))], "parse parses LambdaEs")

checkExpectAbstractProgram(parse([aLambdaAppCPP]), 
[Expression((ApplicationE([LambdaE({nameList: [Name("x"), Name("y")],
                                    lambdaBody:
                                     ApplicationE([NameE((Name("+"))),
                                                   NameE((Name("x"))),
                                                   NameE((Name("y")))])}),
                           NumE(2), NumE(3)])))], "parse parses LambdaApps")





/* Helper Fxn Block: */

/* lookup: (environment, name) => option(value)
input: an environment env and a name id
output: None if if the binding was not found or Some(value)
where value is the corresponding value
*/

/*
OI: ([("x", 10); ("y", 20)], "x" )
  RI: [("y", 20)] , "x"
  RO: None
 OO: Some(20)
 Look to see what name in the env matches the name and that's the value
*/


/*
OI: ([("x", 10); ("y", 20)], "z" )
  RI: [("y", 20)] , "z"
  RO: None
 OO: None
 Look return 00
*/


let rec lookup: (environment, name) => option(value) =
(env, id) =>
switch(env) {
| [] => None
| [[]] => failwith("not an environment")
| [hd, ... tl] =>
let rec lookupHelper: (bindingList, name) => option(value) =
(bindList, id) => switch(bindList) {
| [] => lookup(tl, id)
| [hd, ... tl] => {
let (first, second) = hd
if (first == id) {Some(second)} else {lookupHelper(tl, id)}
}
}
lookupHelper(hd, id)
};



/* addBinding: (environment, binding) => environment
input: an environment env, and a binding bind
output: the environment with the binding added to it
*/
let addBinding: (environment, binding) => environment =
(env, bind) =>
switch(env) {
| [] => [[bind]]
| [[]] => failwith("not an environment")
| [hd, ... tl] => [[bind, ... hd], ... tl] /* or addBindingToList(hd, bind) */
};




/* peakyBinders: (list(name), list(value)) */
/* Recursion Diagrams:
OI:  ([Name("x"), Name("y"), Name("z")], [NumV(3), NumV(5), NumV(7)]
RI: ([Name("y"), Name("z")], [NumV(5), NumV(7)]
RO: [(Name("y"), NumV(5)), (Name("z"), NumV(7))]
Ideation Space: 
[(List.hd(lstForm), List.hd(lstAct)), ... peakyBinders(List.tl(lstForm), List.tl(lstAct))]
OO: [(Name("x"), NumV(3)), (Name("y"), NumV(5)), (Name("z"), NumV(7))]
*/
let rec peakyBinders: (list(name), list(value)) => list(binding) = (lstForm, lstAct) =>
  switch(lstForm, lstAct) {
    | ([], []) => []
    | ([hdF,...tlF], [hdA, ... tlA]) => [(List.hd(lstForm), List.hd(lstAct)), ... peakyBinders(List.tl(lstForm), List.tl(lstAct))]
    | (_, _) => failwith("error: lists must be of same length")
  };


  

/* extendEnv: (environment, bindingList) => environment
input: an environment env and a binding list bindList
output: the bindinglist consed onto the beginning of the environment
*/
 
let extendEnv: (environment, bindingList) => environment = 
    (envr, bindList) => [bindList, ... envr];

    
    
/* fullyFalse: list(value) => bool
input: a list containing values of type BoolV(bool)
output: a bool
*/


/*
OI: [(BoolV(false))]
 RI: [(BoolV(false))]
 RO true
OO: true
return the RO


OI: [(BoolV(false)), (BoolV(true))]
 RI: [(BoolV(true))]
 RO false
OO: false
return the RO
*/

let rec fullyFalse: list(value) => bool = alov => switch(alov) 
  { | [BoolV(false)] => true
    | [BoolV(false), ... tl] => fullyFalse(tl)
    | _ => false};

/* -------------------------------------------------------------------------------------   */

/* Initial Tle, Built-Ins Block */
/* plus: list(value) => value
input: a list of 2 values in form NumV(int) alov
output: the sum of the two ints inside the two NumV(int)*/

let plus: list(value) => value = 
alov => switch(alov) {
  | [NumV(i1), NumV(i2)] => NumV(i1 + i2)
  | _ => failwith("invalid input: plus expects two numbers")
};

checkExpect(plus([NumV(1), NumV(2)]), NumV(3), "plus works")
checkExpect(plus([NumV(8),NumV(-4)]), NumV(4), "plus works")


/* minus: list(value) => value
input: a list of 2 values in form NumV(int) alov
output: the int of the two ints inside the two NumV(int) subtracted from each other
*/

let minus: list(value) => value = 
alov => switch(alov) {
  | [NumV(i1), NumV(i2)] => NumV(i1 - i2)
  | _ => failwith("invalid input: plus expects two numbers")
};

/* mult: list(value) => value
input: a list of 2 values in form NumV(int) alov
output: the product of the two ints inside the two NumV(int)
*/

 let mult: list(value) => value = 
   alov => switch(alov) {
     | [NumV(i1), NumV(i2)] => NumV(i1*i2)
     | _ => failwith("invalid input: mult expects a list of two numbers")
   };
  
/* div: list(value) => value
input: a list of 2 values in form NumV(int) alov
output: the result of the two ints inside the two NumV(int) divide by each other
*/


   let div: list(value) => value = 
   alov => switch(alov) {
     | [NumV(i1), NumV(0)] => failwith("error: cannot divide by zero")
     | [NumV(i1), NumV(i2)] => NumV(i1/i2)
     | _ => failwith("invalid input: div expects a list of two numbers")
   };
   
  
/* remainder: list(value) => value
input: a list of 2 values in form NumV(int) alov
output: the remainder produced when dividing the two ints inside of NumV(int)
*/

    let remainder: list(value) => value = 
   alov => switch(alov) {
     | [NumV(i1), NumV(0)] => failwith("error: cannot divide by zero")
     | [NumV(i1), NumV(i2)] => NumV(i1 mod i2)
     | _ => failwith("invalid input: remainder expects a list of two numbers")
   };


/* numEqualP: list(value) => value
input: a list of 2 values in form NumV(int)
output: a boolV(true) if the two input values are equal and BoolV(false) if not
*/

  let numEqualP: list(value) => value =
    alov => switch(alov) {
     | [NumV(i1), NumV(i2)] => if (i1 == i2) {BoolV(true)} else {BoolV(false)}
     | _ => failwith("invalid input: numEqualP expects a list of two numbers")
    };
    
/* greaterThanP: list(value) => value
input: a list of 2 values in form NumV(int) alov
output: a boolV(true) if the first input is greater than the second and BoolV(false) if not
*/

    let greaterThanP: list(value) => value =
  alov => switch(alov) {
    | [NumV(i1), NumV(i2)] => if (i1 > i2) {BoolV(true)}
                    else {BoolV(false)}
    | _ => failwith("invalid input: greaterThan expects a list of two numbers")
  };

/* lessThanP: list(value) => value
input: a list of 2 values in form NumV(int) alov
output: a boolV(true) if the first input is less than the second and BoolV(false) if not
*/

  let lessThanP: list(value) => value =
  alov => switch(alov) {
    | [NumV(i1), NumV(i2)] => if (i1 < i2) {BoolV(true)}
                    else {BoolV(false)}
    | _=> failwith("invalid input: lessThan expects a list of two numbers")
  };

/* greaterEqThanP: list(value) => value
input: a list of 2 values in form NumV(int) alov
output: a boolV(true) if the first input is greater than or equal to the second and BoolV(false) if not
*/


  
   let greaterEqThanP: list(value) => value =
  alov => switch(alov) {
    | [NumV(i1), NumV(i2)] => if (i1 >= i2) {BoolV(true)}
                    else {BoolV(false)}
    | _ => failwith("invalid input: greaterThan expects a list of two numbers")
  };


/* lessEqThanP: list(value) => value
input: a list of 2 values in form NumV(int) alov
output: a boolV(true) if the first input is less than or equal to the second and BoolV(false) if not
*/

  let lessEqThanP: list(value) => value =
  alov => switch(alov) {
    | [NumV(i1), NumV(i2)] => if (i1 <= i2) {BoolV(true)}
                    else {BoolV(false)}
    | _=> failwith("invalid input: lessThan expects a list of two numbers")
  };
    
/* equalP: list(value) => value
input: a list of 2 values
output: a boolV(true) if the first input is equal to the second and BoolV(false) if not
*/

    let equalP: list(value) => value = 
    alov => switch(alov) {
      | [v1, v2] => if (v1 == v2) {BoolV(true)} else {BoolV(false)}

      | _ => failwith("invalid input: equalP expects a list of two values")
    };
  

/* numberP: list(value) => value
input: a list of 1 value alov
output: a boolV(true) if the first input is a numV and BoolV(false) if not
*/

    let numberP: list(value) => value =
    alov => switch(alov) {
      | [NumV(i)] => BoolV(true)
      | _ => BoolV(false)
    };

/* zeroP: list(value) => value
input: a list of 1 value alov
output: a boolV(true) if the first input is NumV(0) and BoolV(false) if not
*/

    
    let zeroP: list(value) => value =
    alov => switch(alov) {
      | [NumV(0)] => BoolV(true)
      | [NumV(_)] => BoolV(false)
      | _ => failwith("invalid input: zeroP expects a list of a number")
    };

/* cons: list(value) => value
input: a list of values alov
output: a value in the form of listV() where all the data from the
original list of values is added in the same order
*/
    
    let cons: list(value) => value = alov => switch(alov) {
    | [v, ListV(lst)] => ListV([v, ... lst])
    | _ => failwith("invalid input: cons takes in a two-element list containing, respectively, a value and a ListV(values) of the same data type")};
    

/* first: list(value) => value
input: a list of values alov
output: the first value in the list of values
*/

  let first: list(value) => value = alov => switch(alov) {
    | [ListV([])] => failwith("invalid input: empty list has no first element")
    | [ListV([hd, ... tl])] => ListV([hd])
    | [hd, ... _tl] => hd
    | _ => failwith("invalid input: not a list of vals")
  };


/* rest: list(value) => list(value)
input: a list of values alov
output: the list of values in the original list but the first one is removed
*/

  let rest: list(value) => value = alov => switch(alov) {
    | [ListV([])] => failwith("invalid input: empty list has no rest")
    | [ListV([_hd, ... tl])] => ListV(tl)
    | [_hd, ... tl] => ListV(tl)
    | _ => failwith("invalid input: not a list of vals")
  };
  

/* emptyP: list(value) => value
input: a list of values, alov
output: returns boolV(true) if the list is empty and false otherwise
*/

let emptyP: list(value) => value =
alov => switch(alov) {
  | [ListV([])] => BoolV(true)
  | [_] => BoolV(false)
  | _ => failwith("invalid input: emptyP expects a list of values")
};


/* consP: list(value) => value
input: a list of values, alov
output: returns boolV(false) if the list is empty and false otherwise
*/

let consP: list(value) => value =
alov => switch(alov) {
  | [ListV([])] => BoolV(false)
  | [_] => BoolV(true)
  | _ => failwith("invalid input: emptyP expects a list")
};


/* not: list(value) => value
input: a list of a boolean value 
output: returns BoolV(true) if input is BoolV(false), and vice versa 
*/

let not: list(value) => value = 
alov => switch(alov) {
  | [BoolV(false)] => BoolV(true)
  | [BoolV(true)] => BoolV(false)
  | _ => failwith("invalid input: not reverses one bool at a time")
};

/* zeroP: list(value) => value
input: a list of one value alov
output: returns boolV(true) if the list is NumV(0) and false otherwise
*/

let zeroP: list(value) => value =
  alov => switch(alov) {
    | [NumV(0)] => BoolV(true)
    | _ => BoolV(false)
    };
    

/*(Name("cons?"), BuiltinV({printedRep:"<builtin-proc-cons", bProc: cons})) */
let initialTle: environment = [[
  (Name("+"), BuiltinV({printedRep: "<builtin-proc+", bProc: plus,})),
  (Name("-"), BuiltinV({printedRep: "<builtin-proc-", bProc: minus,})),
  (Name("*"), BuiltinV({printedRep: "<builtin-proc-*", bProc: mult})),
  (Name("/"), BuiltinV({printedRep: "<builtin-proc-/", bProc: div})),
  (Name("empty?"), BuiltinV({printedRep: "<builtin-proc-emptyP", bProc: emptyP,})),
  (Name("cons?"), BuiltinV({printedRep: "<builtin-proc-consP", bProc: consP,})),
  (Name(">"), BuiltinV({printedRep: "<builtin-proc->", bProc: greaterThanP})),
  (Name("<"), BuiltinV({printedRep: "<builtin-proc-<", bProc: lessThanP})),
  (Name(">="), BuiltinV({printedRep: "<builtin-proc->=", bProc: greaterEqThanP})),
  (Name("<="), BuiltinV({printedRep: "<builtin-proc-<=", bProc: lessEqThanP})),
  (Name("remainder"), BuiltinV({printedRep: "<builtin-proc-rem", bProc: remainder})),
  (Name("cons"), BuiltinV({printedRep:"<builtin-proc-cons", bProc: cons})),
  (Name("rest"), BuiltinV({printedRep:"<builtin-proc-rest", bProc: rest})),
  (Name("number?"), BuiltinV({printedRep:"<builtin-proc-num?", bProc: numberP})),
  (Name("not"), BuiltinV({printedRep:"<builtin-proc-not?", bProc: not})),
  (Name("zero?"), BuiltinV({printedRep:"<builtin-proc-zero?", bProc: zeroP}))
  ]];

let env = [];


/* -------------------------------------------------------------------------------------   */

/* eval: (environment, environment, expression) => value 
Input: a top level environment tle, a local environment env, and an expression expr 
Output: a value resulting from evaluating the expr in the joint environment*/


let rec eval: (environment, environment, expression) => value =
(tle, env, expr) => switch(expr) {
  | NumE(i) => NumV(i)
  | BoolE(bl) => BoolV(bl)
  | NameE(Name(s)) => switch(lookup(List.append(tle, env), Name(s))) {
    | Some(bindVal) => bindVal
    | None => failwith("error: name not found")
    }
  | EmptyE => ListV([])
  | IfE({boolExpr: bExpr, trueExpr: tExpr, falseExpr: fExpr}) => switch(eval(tle, env, bExpr)) {
    | BoolV(true) => eval(tle, env, tExpr)
    | BoolV(false) => eval(tle, env, fExpr)
    | _ => failwith("invalid input: boolExpr must evaluate to a boolean")

  }
  | OrE(expr1, expr2) => switch(eval(tle, env, expr1)) {
    | BoolV(true) => BoolV(true)
    | BoolV(false) => switch(eval(tle, env, expr2)) {
        | BoolV(true) => BoolV(true)
        | BoolV(false) => BoolV(false)
        | _ => failwith("invalid input: or exprs must evaluate to a boolean")}
      | _ => failwith("invalid input: or exprs must evaluate to a boolean")
    
   }
  | AndE(expr1, expr2) => switch(eval(tle, env, expr1)) {
    | BoolV(true) => switch(eval(tle, env,expr2)){
      | BoolV(true) => BoolV(true)
      | BoolV(false) => BoolV(false)
      | _ => failwith("invalid input: and exprs must evaluate to a boolean")
    }
    | BoolV(false) => BoolV(false) 
    | _ => failwith("invalid input: exprs must evaluate to a boolean")
  }
  | CondE([hd, ... tl]) => switch(eval(tle, env, hd.conditionExpr)) {
    | BoolV(true) => eval(tle, env, hd.resultExpr);
    | BoolV(false) => 
      let rec listcondDatatoListExpr: list(condData) => list(expression) = input => switch(input) 
        { | [hd] => [hd.conditionExpr]
        | [hd, ... tl] => [hd.conditionExpr, ... listcondDatatoListExpr(tl)]
        | _ => failwith("domain") } switch(fullyFalse((List.map(exprPiece => eval(tle,env, exprPiece),
         listcondDatatoListExpr(tl))))) {
      | true => failwith("error: must have at least one true statement in a cond expression")
      | false=> eval(tle, env, CondE(tl))
    }
   | _ => failwith("invalid input: condE's conditionExprs must eval to BoolV")
    }
  | LambdaE({nameList: listId, lambdaBody: expr}) => ClosureV{
    cNameList: listId,
    cExpr: expr,
    cEnv: env,
  }
  | LetE({letPairs: lstletpair, letBody: expr}) => let letEvalHelper: letPair => binding = input =>
    switch(input) {
      | {pairName: id, pairExpr: expr1 } => (id, eval(tle, env, expr1))
    }
    eval(tle, extendEnv(env, List.map(letEvalHelper, lstletpair)), expr)
     | ApplicationE(lst) => switch(List.map(exprPiece => eval(tle, env, exprPiece), lst))
     { | [ClosureV(closDataRec), ... tl] => eval(tle, extendEnv(env, peakyBinders(closDataRec.cNameList, tl)), closDataRec.cExpr)
      | [BuiltinV(bltinRec),...tl] => bltinRec.bProc(tl) 
       | _ => failwith("invalid input: first element of ApplicationE(lst) must eval to either ClosureV(...) or BuiltinV(...)") }
 | _ => failwith("invalid input")
    
      };

checkExpect(eval(initialTle, env, NumE(3)), NumV(3), "eval works on nums")

let anIfE = IfE({boolExpr:
                   ApplicationE([NameE((Name(">="))), NumE(9),
                                 NumE(3)]),
                  trueExpr: NumE(10), falseExpr: NumE(3)})

checkExpect(eval(initialTle, env, anIfE), NumV(10), "eval works on IfEs")

let aAndE = AndE(ApplicationE([NameE(Name(">")), NumE(6), NumE(3)]), ApplicationE([NameE(Name(">")), NumE(2), NumE(3)]))
checkExpect(eval(initialTle, env, aAndE), BoolV(false), "eval works on ands")

let aCondE = CondE([{conditionExpr:
         ApplicationE([NameE((Name(">"))), NumE(4), NumE(3)]),
        resultExpr:
         ApplicationE([NameE((Name("+"))), NumE(6), NumE(3)])}])
checkExpect(eval(initialTle, env, aCondE), NumV(9), "eval works on conds")


let aLetE = LetE({letPairs: [{pairName: Name("x"), pairExpr: NumE(13)}], 
letBody: ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(13)])})

checkExpect(eval(initialTle, env, aLetE), NumV(26), "eval works on lets")


let aLambdaE = LambdaE(
{nameList:[Name("x"), Name("y")],
lambdaBody: ApplicationE([NameE(Name("+")), NameE(Name("x")), NameE(Name("y"))])
})

checkExpect(eval(initialTle, env, aLambdaE), 
ClosureV({cNameList: [Name("x"), Name("y")],
          cExpr:
           ApplicationE([NameE((Name("+"))), NameE((Name("x"))),
                         NameE((Name("y")))]),
          cEnv: []}), "eval works on closures")


let aLambdaCondAppE = ApplicationE([LambdaE({nameList: [Name("x")], lambdaBody: 
CondE([{conditionExpr: ApplicationE([NameE(Name(">")), NameE(Name("x")), NumE(3)]), 
resultExpr: NumE(5)}]),}), NumE(33)])

checkExpect(eval(initialTle, env, aLambdaCondAppE), NumV(5), "eval works on lambdas AppEs defining cond procedures"
) 



/* addDefinition: (environment, (name, expression)) => environment 
Input: a top level environment tle and a definition (id, expr)
Output: the tle with binding (id, eval(tle, env, expr)) added to it */

let addDefinition: (environment, (name, expression)) => environment =
(tle, (id, expr)) => 
  switch(lookup(tle,id)) {
    | Some(_) => failwith("Error: name already found in environment")
    | None => addBinding(tle, (id, eval(tle, env, expr)))
  };
  
let unusedEnv = [[(Name("x"),NumV(1))]];
checkExpect(addDefinition(unusedEnv, (Name("y"), NumE(3))), [[(Name("y"), NumV(3)), (Name("x"), NumV(1))]], 
"addDefinition adds to environments")
 
checkError(() => addDefinition(unusedEnv, (Name("x"), NumE(2))), "Error: name already found in environment")  


/* process: abstractProgram => list(value) 
Input: an abstract program pieces
Output: A list of values resulting from evaluating the components of the abstract program */
let process: abstractProgram => list(value) =
  pieces => {
    let rec processHelper: (environment, abstractProgram) => list(value) =
      (tle, pieces) =>
        switch (pieces) {
        | [] => []
        | [Definition(d), ...tl] => processHelper(addDefinition(tle, d),  tl)
        | [Expression(e), ...tl] => [eval(tle, [], e), ... processHelper(tle, tl)]
         
        };
    processHelper(initialTle, pieces);
  };


let easyCondEasyGo = "(define easy-cond-easy-go (lambda (x)
   (cond
     ((zero? x) 0)
     ((not (zero? x)) (+ x (easy-cond-easy-go (- x 1)))))))
     
     (easy-cond-easy-go 5)"


checkExpect(process([Expression(ApplicationE([NameE(Name("+")), NumE(6), NumE(7)]))]), [NumV(13)], "process works on basic addition")

checkExpect(process([Expression((IfE({boolExpr:
                   ApplicationE([NameE((Name(">="))), NumE(9),
                                 NumE(3)]),
                  trueExpr: NumE(10), falseExpr: NumE(3)})))]), [NumV(10)], "process works on ifs")
                  
checkExpect(process(parse([aLambdaAppCPP])), [NumV(5)], "process works on a lambda")

checkExpect(process(parse(readAll(easyCondEasyGo))), [NumV(15)], "process works on recursive procedures");





/* stringOfValue: value => string
Input: a value, aValue
Output: the value represented in an appropriate corresponding string form depending on the sort of value it is */
let rec stringOfValue: value => string =
  aValue => switch(aValue)
    { 
     | NumV(i) => string_of_int(i)
     | BoolV(b) => string_of_bool(b)
     | ClosureV(closDataRec) => "lambda expression"
     | BuiltinV(builtRec) => builtRec.printedRep
     | ListV(lst) => stringOfAList(lst, stringOfValue)

}

checkExpect(stringOfValue(ListV([NumV(5), NumV(10)])), "[5, 10]", "stringOfValue works on lists")
checkExpect(stringOfValue(eval(initialTle, env, aLambdaE)), "lambda expression", "stringOfValue works on eval creating a closure")
checkExpect(stringOfValue(BuiltinV({printedRep: "<builtin-proc-*", bProc: mult})), "<builtin-proc-*",
 "stringOfValue works on builtins")
 checkExpect(stringOfValue(eval(initialTle, env, anIfE)), "10", "stringOfValue works on the results of an eval on if")


/* TODO: write the header comment parts required by the Design Recipe */
let rackette: rawProgram => list(string) =
  program => List.map(stringOfValue, process(parse(readAll(program))));


checkExpect(rackette(easyCondEasyGo), ["15"], "rackette works on recursive procedures")

/* Apologies for missing check-expects, had to submit the program on time and was not able to fit them all in... */
 
