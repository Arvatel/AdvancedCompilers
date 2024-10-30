import type {
  DeclFun,
  Expr,
  Pattern,
  PatternVar,
  PatternVariant,
  Program,
  RecordFieldType,
  Type,
  TypeBool,
  TypeBottom,
  TypeFun,
  TypeList,
  TypeNat,
  TypeRecord,
  TypeRef,
  TypeSum,
  TypeTuple,
  TypeUnit,
  TypeVariant,
} from './ast';

enum Errors {
  // unexpected type specified for a parameter of an anonymous function
  UNEXPECTED_TYPE_FOR_PARAMETER = 'ERROR_UNEXPECTED_TYPE_FOR_PARAMETER',

  // type of an expression does not match an expected type (known from larger context)
  UNEXPECTED_TYPE_FOR_EXPRESSION = 'ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION',

  // unexpected anonymous function where an expression of a non-function type is expected
  UNEXPECTED_LAMBDA = 'ERROR_UNEXPECTED_LAMBDA',

  // unexpected expression where a function is expected
  NOT_A_FUNCTION = 'ERROR_NOT_A_FUNCTION',

  // undefined variable in a an expression
  UNDEFINED_VARIABLE = 'ERROR_UNDEFINED_VARIABLE',

  // a program is missing main function
  MISSING_MAIN = 'ERROR_MISSING_MAIN', //done

  // part 2
  // record is missing one or more of the expected fields
  MISSING_RECORD_FIELDS = 'ERROR_MISSING_RECORD_FIELDS',
  // record has one or more unexpected fields
  UNEXPECTED_RECORD_FIELDS = 'ERROR_UNEXPECTED_RECORD_FIELDS',
  // unexpected record where an expression of a non-record type is expected
  UNEXPECTED_RECORD = 'ERROR_UNEXPECTED_RECORD',
  // unexpected expression where a record is expected
  NOT_A_RECORD = 'ERROR_NOT_A_RECORD',
  // access to a field that is not present in the record
  UNEXPECTED_FIELD_ACCESS = 'ERROR_UNEXPECTED_FIELD_ACCESS',
  // unexpected tuple/pair where an expression of a non-tuple type is expected
  UNEXPECTED_TUPLE = 'ERROR_UNEXPECTED_TUPLE',
  // unexpected expression where a tuple/pair is expected
  NOT_A_TUPLE = 'ERROR_NOT_A_TUPLE',
  ERROR_TUPLE_INDEX_OUT_OF_BOUNDS = 'ERROR_TUPLE_INDEX_OUT_OF_BOUNDS',


  // when it is impossible to typecheck an expression of a sum type because the other half of the type is unknown
  AMBIGUOUS_SUM_TYPE = 'ERROR_AMBIGUOUS_SUM_TYPE',
  // when it is impossible to typecheck an expression of a list type because the type of its elements is unknown
  AMBIGUOUS_LIST_TYPE = 'ERROR_AMBIGUOUS_LIST_TYPE',
  // when match-expression does not have any patterns
  ILLEGAL_EMPTY_MATCHING = 'ERROR_ILLEGAL_EMPTY_MATCHING',
  // when match-expression does not have all necessary patterns (inl and inr for sum types and at least the empty list pattern and cons-pattern for lists)
  NONEXHAUSTIVE_MATCH_PATTERNS = 'ERROR_NONEXHAUSTIVE_MATCH_PATTERNS',
  // when an expression of a non-list type appears as the argument to one of the builtin list functions (List::head, List::tail, or List::isempty)
  NOT_A_LIST = 'ERROR_NOT_A_LIST',
  // when a list (List or ConsList) is encountered instead of an expression of expected non-list type
  UNEXPECTED_LIST = 'ERROR_UNEXPECTED_LIST',
  // when an injection into a sum type (inl or inr) is encountered instead of an expression of expected non-sum type
  UNEXPECTED_INJECTION = 'ERROR_UNEXPECTED_INJECTION',
  // when a pattern in a match-expression does not correspond to the type of matched expression
  UNEXPECTED_PATTERN_FOR_TYPE = 'ERROR_UNEXPECTED_PATTERN_FOR_TYPE',

  // part 4
  // when type of a variant (Variant) cannot be inferred (needs a type ascription)
  AMBIGUOUS_VARIANT_TYPE = 'ERROR_AMBIGUOUS_VARIANT_TYPE',
  // when a variant (Variant) is encountered where an expression of a non-variant type is expected
  UNEXPECTED_VARIANT = 'ERROR_UNEXPECTED_VARIANT',
  // when a variant (Variant) contains a label that does not match any of the labels in the expected variant type
  UNEXPECTED_VARIANT_LABEL = 'ERROR_UNEXPECTED_VARIANT_LABEL',

  // part 5
  // if an exception mechanism (Throw, TryCatch) is used without a globally declared exception type
  EXCEPTION_TYPE_NOT_DECLARED = 'ERROR_EXCEPTION_TYPE_NOT_DECLARED',
  // if the type of throw-expression cannot be inferred (not the type of of the exception, but the type of throw-expression itself!)
  AMBIGUOUS_THROW_TYPE = 'ERROR_AMBIGUOUS_THROW_TYPE',
  // if a bare memory address is found without an expected type for it
  AMBIGUOUS_REFERENCE_TYPE = 'ERROR_AMBIGUOUS_REFERENCE_TYPE',
  // if a panic expression is found without an expected type for it
  AMBIGUOUS_PANIC_TYPE = 'ERROR_AMBIGUOUS_PANIC_TYPE',
  // when to assign to or dereference an expression that does not have a reference type
  NOT_A_REFERENCE = 'ERROR_NOT_A_REFERENCE',
  // if a memory address literal is found when not expecting a reference
  UNEXPECTED_MEMORY_ADDRESS = 'ERROR_UNEXPECTED_MEMORY_ADDRESS',

  // part 6
  // when during subtype checking, a an actual (part of a) type does not match with an expected (part of a) type
  UNEXPECTED_SUBTYPE = 'ERROR_UNEXPECTED_SUBTYPE',
}

interface Context {
  symbolTable: Map<String, Type>;
  exceptionType: Type | null;
  hasMain: Boolean;
}

function createBaseContext(): Context {
  return {
    symbolTable: new Map(),
    exceptionType: null,
    hasMain: false,
  };
}

function copyContextWithSymbol(
  context: Context,
  name: String,
  type: Type
): Context {
  const newSymbols = new Map(context.symbolTable);
  newSymbols.set(name, type);
  return {
    ...context,
    symbolTable: newSymbols,
  };
}

const TYPE_NAT: TypeNat = { type: 'TypeNat' };
const TYPE_BOOL: TypeBool = { type: 'TypeBool' };
const TYPE_UNIT: TypeUnit = { type: 'TypeUnit' };
const TYPE_BOT: TypeBottom = { type: 'TypeBottom' };
const TYPE_LIST = (t: Type): TypeList => ({ type: "TypeList", elementType: t });
const TYPE_FUN = (paramType: Type, returnType: Type): TypeFun => ({
  type: "TypeFun",
  parametersTypes: [paramType],
  returnType: returnType,
});
const TYPE_REF = (reffered: Type): TypeRef => ({
  type: "TypeRef",
  referredType: reffered,
});
const TYPE_SUM = (left: Type, right: Type): TypeSum => ({
  type: "TypeSum",
  left,
  right,
});


// main part - part which is runs from index.ts
export function typecheckProgram(ast: Program) {
  
  const contx = createBaseContext();
  
  for (const decl of ast.declarations) {
    
    // handle different types of declarations
    switch (decl.type) {
      case 'DeclFun':
        contx.symbolTable.set(decl.name, {
          type: 'TypeFun',
          parametersTypes: [decl.parameters[0].paramType],
          returnType: decl.returnType!,
        });
        typecheckFunctionDecl(decl, contx);
        break;

      case 'DeclExceptionType': {
        contx.exceptionType = decl.exceptionType;
      }
      default:
        throw new Error('Unknown declaration type');
    }
  }

  if (contx.hasMain == false) {
    throw new Error(Errors.MISSING_MAIN);
  }
  console.log('Everything typechecks!');
}


function typecheckFunctionDecl(decl: DeclFun, contx: Context ) {
  const { name, parameters, returnValue, returnType } = decl;

  if (name === 'main') {
    contx.hasMain = true;
  }

  const param = parameters[0];
  const newContext = copyContextWithSymbol(contx, param.name, param.paramType);
  const type = typecheckExpression(returnValue, null, newContext);

  isTypesMatch(returnType!, type);
}

function typecheckExpression(
  expr: Expr,
  expectedType: Type | null,
  context: Context
): Type {
  
  console.log('Checking expression', expr); 
    
  switch (expr.type) {
    case 'Var': {
      console.log('var');

      if (context.symbolTable.has(expr.name) == false)
        throw new Error(Errors.UNDEFINED_VARIABLE);
      return context.symbolTable.get(expr.name)!;
    }
    case 'ConstBool': {
      console.log('bool');
      return TYPE_BOOL;
    }
    case 'ConstInt': {
      console.log('ConstInt');
      return TYPE_NAT;
    }
    case 'Succ': {
      console.log('succ');

      const type = typecheckExpression(expr.expr, TYPE_NAT, context);
      isTypesMatch(TYPE_NAT, type);
      return TYPE_NAT;
    }

    case 'NatIsZero': {
      console.log('NatIsZero');
      const type = typecheckExpression(expr.expr, TYPE_NAT, context);
      isTypesMatch(type, TYPE_NAT);
      return TYPE_BOOL;
    }

    case 'If': {
      console.log('if');

      const { condition, thenExpr, elseExpr } = expr;
      const conditionType = typecheckExpression(condition, TYPE_BOOL, context);
      isTypesMatch(conditionType, TYPE_BOOL);

      const thenType = typecheckExpression(thenExpr, expectedType, context);
      const elseType = typecheckExpression(elseExpr, expectedType, context);
      isTypesMatch(thenType, elseType);

      // console.log('end if');
      return thenType;
    }

    // recursion
    case 'NatRec': {
      console.log('start of recursion (NatRec)');

      const { n, initial, step } = expr;
      // n has to be Nat by default - as it is N of steps
      const nType = typecheckExpression(n, TYPE_NAT, context);

      isTypesMatch(nType, TYPE_NAT);

      const initialType = typecheckExpression (initial, null, context);
      const stepType = typecheckExpression (step, null, context);
      const tempStep = (stepType as TypeFun).returnType;
      const expectedST : TypeFun = {
        type: 'TypeFun',
        parametersTypes : [initialType],
        returnType: initialType,
      }

      isTypesMatch(expectedST, tempStep);
      return initialType;
    }

    // from labs
    case 'Abstraction': {
      console.log("Abstraction");
      if (expectedType && expectedType.type !== 'TypeFun') {
        throw new Error(Errors.UNEXPECTED_LAMBDA);
      }
      const { parameters, returnValue } = expr;
      const param = parameters[0];
      const paramExpected = expectedType?.parametersTypes[0];
      if (paramExpected) {
        try {
          isTypesMatch(paramExpected, param.paramType);
        } catch {
          throw new Error(Errors.UNEXPECTED_TYPE_FOR_PARAMETER);
        }
      }
      const newContext = copyContextWithSymbol(
        context,
        param.name,
        param.paramType
      );
      const bodyType = typecheckExpression(
        returnValue,
        expectedType?.returnType ?? null,
        newContext
      );
      return {
        type: 'TypeFun',
        parametersTypes: [param.paramType],
        returnType: bodyType,
      };
    }

    // from labs
    case 'Application': {
      console.log("Application");
      const { function: func, arguments: args } = expr;
      const type = typecheckExpression(func, null, context);
      if (type.type !== 'TypeFun') {
        throw new Error(Errors.NOT_A_FUNCTION);
      }
      const argType = typecheckExpression(args[0], type.parametersTypes[0], context);
      isTypesMatch(type.parametersTypes[0], argType);
      if (expectedType) {
        isTypesMatch(type.returnType, expectedType);
      }
      return (type as TypeFun).returnType;
    }

    case 'Unit': {
      console.log('Unit');
      return TYPE_UNIT;
    }

    // tuple doesn't mean there is only 3 elements - any number of elements actyally
    case 'Tuple': {
      console.log('tuple')
      if (expectedType && expectedType.type !== 'TypeTuple') {
        throw new Error(Errors.UNEXPECTED_TUPLE);
      }

      const array = new Array();
      for (var i = 0; i < expr.exprs.length; i++){
        const t = typecheckExpression(expr.exprs[i], expectedType?.types[i] ?? null, context);
        console.log('tuple added el');
        array.push(t);
      }
      const tuple: TypeTuple = {
        type: 'TypeTuple',
        types: array,
      };
      return tuple;
    }

    case 'DotTuple': {
      console.log('dotTuple');

      const t = typecheckExpression(expr.expr, null, context);
      if (t.type !== 'TypeTuple'){
        throw new Error(Errors.NOT_A_TUPLE);
      }
      else{
        if (expr.index > t.types.length && expr.index > 0) {
          throw new Error(Errors.ERROR_TUPLE_INDEX_OUT_OF_BOUNDS);
        }
        // returns the type of the specified element from the tuple (-1 cause counting starts with 0)
        return t.types[expr.index - 1];
      }
    }

    // we are checking record creation
    case 'Record': {
      console.log('record');

      if (expectedType && expectedType.type !== 'TypeRecord') {
        throw new Error(Errors.UNEXPECTED_RECORD);
      }

      const record = Array();
      if (expr.type == 'Record'){
        for (const item of expr.bindings){
          if (item.type == 'Binding'){
            const t = typecheckExpression(item.expr, null, context);
            const rec: RecordFieldType = {
              type: 'RecordFieldType',
              label: item.name,
              fieldType: t,
            };
            record.push(rec);
          }
        }
        return {
          type: 'TypeRecord',
          fieldTypes: record,
        }
      }
    }
    // we are checking how we work with dot record itself
    case 'DotRecord': {
      console.log('dotRecord');

      const type = typecheckExpression(expr.expr, null, context);

      if (type.type !== 'TypeRecord'){
        throw new Error(Errors.NOT_A_RECORD);
      }

      // trying to find variable with the same identifier name - if not - error
      const t = type.fieldTypes.find((field) => field.label === expr.label);
      if (t == null)
        throw new Error(Errors.UNEXPECTED_FIELD_ACCESS);
      
      return t.fieldType;
    }

    // you may assume that all patterns are just variables
    case 'Let': {
      console.log('Let');
      expr.patternBindings.forEach(({pattern, rhs}) => {
        const {name} = pattern as PatternVar;
        const t = typecheckExpression(rhs, null, context);

        context = copyContextWithSymbol(context, name, t);
      });
    return typecheckExpression(expr.body, expectedType, context);
    }

    case 'TypeAscription': {
      console.log('TypeAscription');
      const t = typecheckExpression(expr.expr, expr.ascribedType, context);
      isTypesMatch(expr.ascribedType, t);
      return expr.ascribedType;
    }

    case 'Inl': {
      console.log('inl');
      if (expectedType && expectedType.type !== 'TypeSum') {
        throw new Error(Errors.UNEXPECTED_INJECTION);
      }
      const act = typecheckExpression(expr.expr, expectedType?.left ?? null, context);

      if (expectedType){
        isTypesMatch(expectedType.left, act);
        return expectedType;
      }
      else
        return TYPE_SUM(act, TYPE_BOT);
    }

    case 'Inr': {
      console.log('inr');
      if (expectedType && expectedType.type !== 'TypeSum') {
        throw new Error(Errors.UNEXPECTED_INJECTION);
      }
      const act = typecheckExpression(expr.expr, expectedType?.right ?? null, context);

      if (expectedType){
        isTypesMatch(expectedType.right, act);
        return expectedType;
      }
      else
        return TYPE_SUM(TYPE_BOT, act);
    }

    // from lab
    case 'Match': {
      console.log('match');
      const { cases, expr: expression } = expr;
      const exprType = typecheckExpression(expression, null, context);
      if (cases.length === 0) {
        throw new Error(Errors.ILLEGAL_EMPTY_MATCHING);
      }
      let caseBodyExpectedType: Type | null = expectedType;
      for (const matchCase of cases) {
        const extendedCtx = checkPattern(matchCase.pattern, exprType, context);
        const inferredType = typecheckExpression(
          matchCase.expr,
          expectedType,
          extendedCtx
        );
        if (caseBodyExpectedType != null) {
          isTypesMatch(caseBodyExpectedType, inferredType);
        } else {
          caseBodyExpectedType = inferredType;
        }
      }
      if (
        !isExhaustive(
          exprType,
          cases.map((case_) => case_.pattern)
        )
      ) {
        throw new Error(Errors.NONEXHAUSTIVE_MATCH_PATTERNS);
      }
      return caseBodyExpectedType!;
    }

    case 'List': {
      console.log('List');
      console.log('Checking expression', expr.exprs); 
      console.log('Expression type', expr.type);
      console.log('Expected type', expectedType); 

      if (expectedType && expectedType.type != 'TypeList') {
        throw new Error(Errors.UNEXPECTED_LIST);
      }
      const {exprs} = expr;
      let elementExpectedType: Type | null = expectedType;
      console.log('List, expect', expectedType);
      
      for (const it of exprs) {
        
        const inferredType = typecheckExpression(it, expectedType?.elementType ?? null, context);
        
        console.log('List, for - ele', elementExpectedType);
        console.log('List, for - inf', inferredType);


        if (elementExpectedType){
          console.log('List, checking if', inferredType);
          // console.log('List, checking if', elementExpectedType);
          isTypesMatch(elementExpectedType, inferredType);
          
          if (elementExpectedType.type != inferredType.type && inferredType.type != "TypeAuto"){
            throw new Error(Errors.UNEXPECTED_TYPE_FOR_EXPRESSION);
          }
          else{
            console.log('List, else', inferredType);
            elementExpectedType = inferredType;
          }
        }
        else
          elementExpectedType = inferredType;
      }

      return TYPE_LIST(elementExpectedType ?? TYPE_BOT);
    }


    case 'Cons': {
      console.log('cons');
      if (expectedType && expectedType.type !== 'TypeList') {
        throw new Error(Errors.UNEXPECTED_LIST);
      }
      const {head, tail} = expr;
      
      const headT = typecheckExpression(head, expectedType?.elementType ?? null, context);
      const tailT = typecheckExpression(tail, expectedType, context);

      if (tailT.type !== 'TypeList') 
        throw new Error(Errors.NOT_A_LIST);

      isTypesMatch(headT, tailT.elementType);
      return tailT;
    }

    case 'ListHead': {
      console.log('ListHead');
      const t = typecheckExpression(expr.expr, null, context);

      console.log('ListHead t -', t);
      if (t.type !== 'TypeList')
        throw new Error(Errors.NOT_A_LIST);

      // t = expectedType;

      console.log('ListHead elem -', t.elementType);
      return t.elementType;
    }

    case 'ListTail': {
      console.log('ListTail');
      const t = typecheckExpression(expr.expr, null, context);

      if (t.type !== 'TypeList')
        throw new Error(Errors.NOT_A_LIST);

      return t;
    }
    case 'ListIsEmpty': {
      console.log('ListIsEmpty');
      const t = typecheckExpression(expr.expr, null, context);

      if (t.type !== 'TypeList')
        throw new Error(Errors.NOT_A_LIST);

      return TYPE_BOOL;
    }
    case 'Variant': {
      console.log('Variant');
      const { label, expr: value } = expr;
      let fieldExpectedType: Type | null = null;

      if (expectedType) {
        if (expectedType.type !== 'TypeVariant')
          throw new Error(Errors.UNEXPECTED_VARIANT);
        
        const field = expectedType.fieldTypes.find((field) => field.label === label);

        if (field == undefined)
          throw new Error(Errors.UNEXPECTED_VARIANT_LABEL);
    
        fieldExpectedType = field.fieldType!;
      }

      const fieldType = typecheckExpression(value!, fieldExpectedType, context);

      if (fieldExpectedType)
        isTypesMatch(fieldExpectedType, fieldType);
      
      return (expectedType ?? {type: 'TypeVariant',fieldTypes: [{ type: 'VariantFieldType', label, fieldType }],});
    }

    // it's like a way to make recursion without recursion
    case 'Fix': {
      console.log('Fix');
      const expected = expectedType && TYPE_FUN(expectedType, expectedType);
      const actualType = typecheckExpression(expr.expr, expected, context);
      
      if (actualType.type !== 'TypeFun') 
        throw new Error(Errors.NOT_A_FUNCTION);
      
      isTypesMatch(actualType.parametersTypes[0], actualType.returnType);
      
      return actualType.returnType;
    }

    case 'ConstMemory': {
      console.log('ConstMemory');
      if (!expectedType) {
        throw new Error(Errors.AMBIGUOUS_REFERENCE_TYPE);
      }
      if (expectedType.type !== 'TypeRef') {
        throw new Error(Errors.UNEXPECTED_MEMORY_ADDRESS);
      }
      return expectedType;
    }

    case 'Sequence': {
      console.log("Sequence");
      
      const { expr1, expr2 } = expr;
      
      const t1 = typecheckExpression(expr1, TYPE_UNIT, context);
      isTypesMatch(TYPE_UNIT, t1);

      return typecheckExpression(expr2, expectedType, context);
    }

    case 'Reference': {
      console.log('Reference');
      const {expr: val} = expr;

      if (expectedType && expectedType.type === 'TypeRef') {
        expectedType = expectedType.referredType;
      }
      const t = typecheckExpression(val, expectedType, context);
      return TYPE_REF(t);
    }

    case 'Dereference': {
      console.log('Dereference');
      const refType = typecheckExpression(expr.expr, expectedType && TYPE_REF(expectedType), context);

      if (refType.type !== 'TypeRef')
        throw new Error(Errors.NOT_A_REFERENCE);

      return refType.referredType;
    }
    
    case 'Assignment': {
      console.log('Assignment');
      const {lhs, rhs} = expr;
      const lhsType = typecheckExpression(lhs, null, context);
      
      if (lhsType.type !== 'TypeRef')
        throw new Error(Errors.NOT_A_REFERENCE);
      
      const rhsType = typecheckExpression(rhs, lhsType.referredType, context);
      isTypesMatch(rhsType, lhsType.referredType);

      return TYPE_UNIT;
    }
    case 'Panic': {
      console.log('Panic');
      if (!expectedType)
        return TYPE_BOT;
      
      return expectedType;
    }

    case 'Throw': {
      console.log('Throw');
      if (context.exceptionType == null)
        throw new Error(Errors.EXCEPTION_TYPE_NOT_DECLARED);

      if (!expectedType)
        return TYPE_BOT;

      const t = typecheckExpression(expr.expr, context.exceptionType, context);
      isTypesMatch(context.exceptionType, t);
      return expectedType;
    }

    case 'TryWith': {
      console.log('TryWith');
      const { tryExpr, fallbackExpr } = expr;

      const tt = typecheckExpression(tryExpr, expectedType, context);
      const ft = typecheckExpression(fallbackExpr, expectedType, context);

      isTypesMatch(tt, ft);
      return tt;
    }

    case 'TryCatch': {
      console.log('TryCatch');
      if (context.exceptionType == null) {
        throw new Error(Errors.EXCEPTION_TYPE_NOT_DECLARED);
      }
      const { tryExpr, fallbackExpr, pattern } = expr;

      const tryType = typecheckExpression(tryExpr, expectedType, context);
      const fallbackCtx = checkPattern(pattern, context.exceptionType, context);
      const fallbackType = typecheckExpression(fallbackExpr, expectedType, fallbackCtx);
      
      isTypesMatch(tryType, fallbackType);
      
      return tryType;
    }

    case 'TypeCast': {
      console.log('TypeCast');
      const { expr: expression, castType } = expr;
      const _ = typecheckExpression(expression, null, context);
      return castType;
    }
    default:
      throw new Error('Unknown expression type');
  }
}

//  It performs pattern checking on a specific pattern and throws an error if the provided pattern is incompatible with the given type
// from lab

function checkPattern(pattern: Pattern, type: Type, context: Context): Context {
  switch (pattern.type) {
    case 'PatternVar': {
      return copyContextWithSymbol(context, pattern.name, type);
    }
    case 'PatternInl': {
      if (type.type !== 'TypeSum') {
        throw new Error(Errors.UNEXPECTED_PATTERN_FOR_TYPE);
      }
      return checkPattern(pattern.pattern, type.left, context);
    }
    case 'PatternInr': {
      if (type.type !== 'TypeSum') {
        throw new Error(Errors.UNEXPECTED_PATTERN_FOR_TYPE);
      }
      return checkPattern(pattern.pattern, type.right, context);
    }
    case 'PatternVariant': {
      if (type.type !== 'TypeVariant') {
        throw new Error(Errors.UNEXPECTED_PATTERN_FOR_TYPE);
      }
      const { label, pattern: innerPattern } = pattern;
      const { fieldTypes } = type;
      const field = fieldTypes.find((field) => field.label === label);
      if (field == undefined) {
        throw new Error(Errors.UNEXPECTED_PATTERN_FOR_TYPE);
      }
      
      return checkPattern(innerPattern!, field.fieldType!, context);
    }
    default:
      throw new Error('Unimplemented');
  }
}

// checks whether a set of provided patterns can cover all possible cases of a given type
function isExhaustive(type: Type, patterns: Pattern[]): boolean {
  const types = patterns.map((pattern) => pattern.type);
  if (types.some((type) => type === 'PatternVar')) return true;
  switch (type.type) {
    case 'TypeSum': {
      return types.includes('PatternInl') && types.includes('PatternInr');
    }
    case 'TypeVariant': {
      const { fieldTypes } = type;
      const usedPatternLabels = (patterns as PatternVariant[]).map(
        (pattern) => pattern.label
      );
      for (const { label } of fieldTypes) {
        if (!usedPatternLabels.includes(label)) {
          return false;
        }
      }
      return true;
    }
    default:
      return false;
  }
}


// checks if types are matching to one another
function isTypesMatch(expected: Type, actual: Type) {
  // Usually represents the most general type in a type system
  // It can be thought of as a supertype that encompasses all possible types  
  if (expected.type == 'TypeTop') {
    return true;
  }

  // indicates that there are no valid values that can match this type
  if (actual.type == 'TypeBottom') {
    return true;
  }
  console.log('Matching types - expected:', expected, 'and actual: ', actual);

  if (expected.type === actual.type) {
    
    // checks for complex types
    switch (expected.type) {
      case 'TypeFun': {
        isTypesMatch((actual as TypeFun).parametersTypes[0], expected.parametersTypes[0]);
        isTypesMatch(expected.returnType, (actual as TypeFun).returnType);
        return true;
      }
      case 'TypeTuple': {
        // check only for pairs
        const [expected1, expected2] = expected.types;
        const [actual1, actual2] = (actual as TypeTuple).types;
        isTypesMatch(expected1, actual1);
        isTypesMatch(expected2, actual2);

        // TODO - think about it - how to do multiple params
        // for (var i = 0; i < expected.exprs.length; i++){
        //   isTypesMatch((expected as TypeTuple).types[i], (actual as TypeTuple).types[i]);
        // };

        return true;
      }
      case 'TypeRecord': {
        const actualFieldTypes = (actual as TypeRecord).fieldTypes;
        
        expected.fieldTypes.forEach(({label, fieldType}) => {
          const a = actualFieldTypes.find((item) => item.label === label);
          
          // if the field was not found
          if (a == null){
            throw new Error(Errors.MISSING_RECORD_FIELDS);
          }
          isTypesMatch(fieldType, a.fieldType)
        })
        return true;
      }

      case 'TypeList': {
        isTypesMatch(expected.elementType, (actual as TypeList).elementType);
        return true;
      }

      case 'TypeVariant': {
        const actualFields = (actual as TypeVariant).fieldTypes;

        for (const { label, fieldType } of actualFields) {
          
          const expectedField = expected.fieldTypes.find((f) => f.label === label);

          if (expectedField == null)
            throw new Error(Errors.UNEXPECTED_VARIANT_LABEL);
          
          isTypesMatch(expectedField.fieldType!, fieldType!);
        }
        return true;
      }

      case 'TypeRef': {

        isTypesMatch(expected.referredType, (actual as TypeRef).referredType);
        isTypesMatch((actual as TypeRef).referredType, expected.referredType);

        return true;
      }

      case 'TypeSum': {
        isTypesMatch(expected.left, (actual as TypeSum).left);
        isTypesMatch(expected.right, (actual as TypeSum).right);
      }

      default:
        return true;
    }
  } else if (expected.type != actual.type) {
    if (expected.type == "TypeAuto" || actual.type == "TypeAuto") {
      return;
    }
    if (expected.type == "TypeBool" || "TypeNat" || "TypeFun" || "TypeRecord" || 
      actual.type == "TypeBool" || "TypeNat" || "TypeFun" || "TypeRecord") {
      throw new Error(Errors.UNEXPECTED_TYPE_FOR_EXPRESSION);
    }
  }
  throw new Error(Errors.UNEXPECTED_SUBTYPE);
}


