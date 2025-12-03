// semantic.js
// Analizador semántico basado en AST de parser.js

// ======== Definición de tipos internos ========
// Usamos objetos JS para representar tipos:
//  - Básico: { kind: "basic", name: "integer" | "real" | "boolean" | "char" | "string" }
//  - Array:  { kind: "array", base: Type, low: any, high: any }

const BASIC_TYPE_NAMES = ["integer", "real", "boolean", "char", "string"];

function makeBasic(name) {
  return { kind: "basic", name: name.toLowerCase() };
}
function makeArray(base, lowExpr, highExpr) {
  return { kind: "array", base, low: lowExpr, high: highExpr };
}

function isBasic(t, name) {
  return t && t.kind === "basic" && t.name === name.toLowerCase();
}
function isNumeric(t) {
  return t && t.kind === "basic" && (t.name === "integer" || t.name === "real");
}
function isInteger(t) {
  return isBasic(t, "integer");
}
function isBoolean(t) {
  return isBasic(t, "boolean");
}
function isChar(t) {
  return isBasic(t, "char");
}
function isStringType(t) {
  return isBasic(t, "string");
}

function sameType(t1, t2) {
  if (!t1 || !t2) return false;
  if (t1.kind !== t2.kind) return false;
  if (t1.kind === "basic") return t1.name === t2.name;
  if (t1.kind === "array") {
    // muy simple: mismo tipo base
    return sameType(t1.base, t2.base);
  }
  return false;
}

// ======== Scopes y tabla de símbolos ========

function createContext() {
  const ctx = {
    errors: [],
    // alias de tipos: nombreTipo -> Type
    types: new Map(),
    // pila de scopes: cada scope es Map<nombre, symbol>
    scopes: [new Map()],
  };

  // semilla: tipos básicos
  for (const n of BASIC_TYPE_NAMES) {
    ctx.types.set(n, makeBasic(n));
  }

  // semilla: procedimientos integrados (write, writeln, etc.)
  const g = ctx.scopes[0];
  const builtins = ["write", "writeln", "read", "readln"];
  for (const b of builtins) {
    g.set(b.toLowerCase(), { kind: "proc", name: b.toLowerCase(), builtin: true });
  }

  return ctx;
}

function pushScope(ctx) {
  ctx.scopes.push(new Map());
}
function popScope(ctx) {
  ctx.scopes.pop();
}
function currentScope(ctx) {
  return ctx.scopes[ctx.scopes.length - 1];
}
function defineSymbol(ctx, name, sym, node) {
  const scope = currentScope(ctx);
  const key = name.toLowerCase();
  if (scope.has(key)) {
    reportError(ctx, node, `Identificador "${name}" ya fue declarado en este ámbito.`, name);
  } else {
    scope.set(key, sym);
  }
}
function lookupSymbol(ctx, name) {
  const key = name.toLowerCase();
  for (let i = ctx.scopes.length - 1; i >= 0; i--) {
    const scope = ctx.scopes[i];
    if (scope.has(key)) return scope.get(key);
  }
  return null;
}

function defineTypeAlias(ctx, name, typeNode) {
  const key = name.toLowerCase();
  if (ctx.types.has(key)) {
    // re-definición de tipo
    // (en Pascal algunos compiladores lo permiten, aquí lo marcamos)
    // No usamos node aquí, pero podrías pasarlo si quieres marcar línea.
  }
  ctx.types.set(key, typeNode);
}

function resolveTypeName(ctx, name, nodeForErr) {
  const key = name.toLowerCase();
  if (ctx.types.has(key)) return ctx.types.get(key);
  if (BASIC_TYPE_NAMES.includes(key)) {
    return makeBasic(key);
  }
  reportError(ctx, nodeForErr, `Tipo "${name}" no definido.`, name);
  return null;
}

// ======== Manejo de errores ========

function reportError(ctx, node, msg, lexemeOpt) {
  const line = node && node.line != null ? node.line : 0;
  const column = node && node.column != null ? node.column : 0;
  const lexeme = lexemeOpt != null ? lexemeOpt : (node && node.lexeme ? node.lexeme : "");
  ctx.errors.push({ line, column, msg, lexeme });
}

// ======== Resolución de tipos de declaraciones ========

function resolveTypeNode(ctx, typeNode) {
  if (!typeNode) return null;

  if (typeNode.kind === "NamedType") {
    return resolveTypeName(ctx, typeNode.name, typeNode);
  }

  if (typeNode.kind === "ArrayType") {
    // array[low..high] of base
    const baseType = resolveTypeNode(ctx, typeNode.base);
    if (!baseType) return null;
    return makeArray(baseType, typeNode.low, typeNode.high);
  }

  return null;
}

// ======== Tipo de expresiones ========

function inferExprType(ctx, expr) {
  if (!expr) return null;

  switch (expr.kind) {
    case "Literal": {
      const h = expr.typeHint && expr.typeHint.toLowerCase();
      if (BASIC_TYPE_NAMES.includes(h)) return makeBasic(h);
      return null;
    }

    case "Var": {
      const sym = lookupSymbol(ctx, expr.name);
      if (!sym || sym.kind !== "var") {
        reportError(ctx, expr, `Identificador "${expr.name}" usado pero no declarado.`, expr.name);
        return null;
      }
      let t = sym.type;
      // índices de array
      if (expr.indexes && expr.indexes.length) {
        for (const idxExpr of expr.indexes) {
          const idxType = inferExprType(ctx, idxExpr);
          if (!isInteger(idxType)) {
            reportError(
              ctx,
              idxExpr,
              `El índice de un arreglo debe ser integer.`,
              expr.name
            );
          }
          if (!t || t.kind !== "array") {
            reportError(
              ctx,
              expr,
              `"${expr.name}" no es un arreglo pero se usa con índices.`,
              expr.name
            );
            return null;
          }
          t = t.base;
        }
      }
      // campos de record (no implementados)
      if (expr.fields && expr.fields.length) {
        reportError(
          ctx,
          expr,
          `Acceso a campos de record no soportado en este analizador.`,
          expr.name
        );
        return null;
      }
      return t;
    }

    case "Unary": {
      const innerType = inferExprType(ctx, expr.expr);
      const op = String(expr.op).toLowerCase();
      if (op === "not") {
        if (!isBoolean(innerType)) {
          reportError(ctx, expr, `El operador 'not' requiere un operando boolean.`, "not");
        }
        return makeBasic("boolean");
      }
      // +x, -x: numérico
      if (!isNumeric(innerType)) {
        reportError(
          ctx,
          expr,
          `El operador '${expr.op}' requiere operando numérico.`,
          String(expr.op)
        );
      }
      // si es real en algún momento, usamos real
      return innerType && isNumeric(innerType)
        ? (isBasic(innerType, "real") ? makeBasic("real") : makeBasic("integer"))
        : null;
    }

    case "Binary": {
      const op = String(expr.op).toLowerCase();
      const leftType = inferExprType(ctx, expr.left);
      const rightType = inferExprType(ctx, expr.right);

      // relacionales -> boolean
      if (["=", "<>", "<", ">", "<=", ">="].includes(op)) {
        if (!leftType || !rightType) return makeBasic("boolean");
        // Podríamos ser más estrictos (ej. comparar string vs integer), por ahora chequeo simple:
        if (!sameType(leftType, rightType)) {
          reportError(
            ctx,
            expr,
            `Comparación entre tipos incompatibles.`,
            String(expr.op)
          );
        }
        return makeBasic("boolean");
      }

      // booleanos: and, or
      if (op === "and" || op === "or") {
        if (!isBoolean(leftType) || !isBoolean(rightType)) {
          reportError(
            ctx,
            expr,
            `Los operadores lógicos 'and'/'or' requieren operandos boolean.`,
            op
          );
        }
        return makeBasic("boolean");
      }

      // aritméticos: +, -, *, /, div, mod
      if (["+", "-", "*", "/", "div", "mod"].includes(op)) {
        if (!isNumeric(leftType) || !isNumeric(rightType)) {
          reportError(
            ctx,
            expr,
            `Operador aritmético '${op}' requiere operandos numéricos.`,
            op
          );
          return null;
        }

        // "/" -> real siempre
        if (op === "/") return makeBasic("real");

        // "div" y "mod" requieren enteros
        if (op === "div" || op === "mod") {
          if (!isInteger(leftType) || !isInteger(rightType)) {
            reportError(
              ctx,
              expr,
              `'${op}' requiere operandos integer.`,
              op
            );
          }
          return makeBasic("integer");
        }

        // +, -, * : si alguno es real, resultado real; si ambos integer, integer
        if (isBasic(leftType, "real") || isBasic(rightType, "real")) {
          return makeBasic("real");
        }
        return makeBasic("integer");
      }

      return null;
    }

    case "CallExpr": {
      // llamada usada en expresión (función)
      const sym = lookupSymbol(ctx, expr.name);
      if (!sym) {
        reportError(
          ctx,
          expr,
          `Función "${expr.name}" no declarada.`,
          expr.name
        );
        return null;
      }
      if (sym.kind !== "func") {
        // procedimiento usado como función
        reportError(
          ctx,
          expr,
          `"${expr.name}" no es una función (no retorna valor).`,
          expr.name
        );
        return null;
      }
      // aquí podrías validar número/tipo de parámetros (por ahora no lo hacemos)
      return sym.returnType || null;
    }

    default:
      return null;
  }
}

// ======== Chequeo de statements ========

function checkStatement(ctx, stmt) {
  if (!stmt) return;

  switch (stmt.kind) {
    case "Assign": {
      // target debe ser Var
      const target = stmt.target;
      const tTarget = inferExprType(ctx, target);
      const tExpr = inferExprType(ctx, stmt.expr);

      if (!tTarget || !tExpr) return;
      if (!sameType(tTarget, tExpr)) {
        const nameLex = target && target.name ? target.name : ":=";
        reportError(
          ctx,
          stmt,
          `Asignación incompatible: tipos distintos entre variable y expresión.`,
          nameLex
        );
      }
      return;
    }

    case "If": {
      const tCond = inferExprType(ctx, stmt.cond);
      if (!isBoolean(tCond)) {
        reportError(
          ctx,
          stmt.cond || stmt,
          `La condición de 'if' debe ser boolean.`,
          "if"
        );
      }
      checkStatement(ctx, stmt.thenBranch);
      if (stmt.elseBranch) checkStatement(ctx, stmt.elseBranch);
      return;
    }

    case "While": {
      const tCond = inferExprType(ctx, stmt.cond);
      if (!isBoolean(tCond)) {
        reportError(
          ctx,
          stmt.cond || stmt,
          `La condición de 'while' debe ser boolean.`,
          "while"
        );
      }
      checkStatement(ctx, stmt.body);
      return;
    }

    case "For": {
      const sym = lookupSymbol(ctx, stmt.varName);
      if (!sym || sym.kind !== "var") {
        reportError(
          ctx,
          stmt,
          `Variable de control "${stmt.varName}" no declarada.`,
          stmt.varName
        );
      } else if (!isInteger(sym.type)) {
        reportError(
          ctx,
          stmt,
          `La variable de control de 'for' debe ser integer.`,
          stmt.varName
        );
      }

      const tFrom = inferExprType(ctx, stmt.from);
      const tTo = inferExprType(ctx, stmt.to);
      if (!isNumeric(tFrom) || !isNumeric(tTo)) {
        reportError(
          ctx,
          stmt,
          `Los límites de 'for' deben ser expresiones numéricas.`,
          "for"
        );
      }

      checkStatement(ctx, stmt.body);
      return;
    }

    case "Call": {
      const sym = lookupSymbol(ctx, stmt.name);
      if (!sym) {
        reportError(
          ctx,
          stmt,
          `Procedimiento/función "${stmt.name}" no declarado.`,
          stmt.name
        );
        return;
      }
      if (sym.kind !== "proc" && sym.kind !== "func") {
        reportError(
          ctx,
          stmt,
          `"${stmt.name}" no es un procedimiento o función.`,
          stmt.name
        );
      }
      // aquí podrías validar número y tipos de parámetros
      return;
    }

    case "Compound": {
      // bloque begin..end anidado: nuevo scope
      pushScope(ctx);
      for (const s of stmt.stmts || []) {
        checkStatement(ctx, s);
      }
      popScope(ctx);
      return;
    }

    default:
      return;
  }
}

// ======== Chequeo de declaraciones ========

function processDeclarations(ctx, decls) {
  for (const d of decls || []) {
    if (d.kind === "TypeDecl") {
      const tResolved = resolveTypeNode(ctx, d.type);
      if (tResolved) {
        defineTypeAlias(ctx, d.name, tResolved);
      }
    } else if (d.kind === "VarDecl") {
      const tResolved = resolveTypeNode(ctx, d.type);
      if (!tResolved) continue;
      for (const name of d.names) {
        defineSymbol(ctx, name, { kind: "var", name, type: tResolved }, d);
      }
    } else {
      // ProcDecl/FuncDecl si los agregas después
    }
  }
}

// ======== Chequeo de bloques ========

function checkBlock(ctx, block) {
  if (!block || block.kind !== "Block") return;

  // Declara tipos y variables en el scope actual
  processDeclarations(ctx, block.decls);

  // Sentencias
  for (const s of block.stmts || []) {
    checkStatement(ctx, s);
  }
}

// ======== Entrada principal ========

function analizarSemanticaAST(astProgram) {
  const ctx = createContext();

  if (!astProgram || astProgram.kind !== "Program") {
    // si el parser falló, devolvemos lo que se pueda
    return ctx.errors;
  }

  // Scope global (ya existe uno en ctx.scopes)
  // agregamos posibles cosas globales extra si quieres

  checkBlock(ctx, astProgram.block);

  return ctx.errors;
}

// Exportar en navegador
if (typeof window !== "undefined") {
  window.analizarSemanticaAST = analizarSemanticaAST;
}
