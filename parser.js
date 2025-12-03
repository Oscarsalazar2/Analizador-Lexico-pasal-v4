// parser.js

// ======== Tipos de nodos AST ========

// Usaremos objetos plain JS con un campo "kind" para distinguir tipos de nodo.

// Programa completo
// { kind: "Program", name, block }
 
// Bloque
// { kind: "Block", decls: [...], stmts: [...] }

// Declaraciones:
// { kind: "TypeDecl", name, type }
// { kind: "VarDecl", names: [...], type }
// (más adelante podrías agregar ConstDecl, ProcDecl, FuncDecl, etc.)

// Statements:
// { kind: "Assign", target, expr }
// { kind: "If", cond, thenBranch, elseBranch }
// { kind: "While", cond, body }
// { kind: "For", varName, from, to, direction, body }
// { kind: "Call", name, args }
// { kind: "Compound", stmts: [...] } // begin..end interno

// Expresiones:
// { kind: "Var", name, indexes: [...], fields: [...] }
// { kind: "Literal", value, typeHint } // typeHint: "integer","real","string",...
// { kind: "Binary", op, left, right }
// { kind: "Unary", op, expr }


// ======== Parser ========

class Parser {
  constructor(tokens) {
    this.tokens = tokens.filter(t => t.type !== "COMENTARIO");
    this.pos = 0;
  }

  current() {
    return this.tokens[this.pos] || null;
  }

  matchLex(lex) {
    const t = this.current();
    if (t && String(t.lexeme).toLowerCase() === lex.toLowerCase()) {
      this.pos++;
      return t;
    }
    return null;
  }

  matchType(type) {
    const t = this.current();
    if (t && t.type === type) {
      this.pos++;
      return t;
    }
    return null;
  }

  expectLex(lex, msg) {
    const t = this.current();
    if (!t || String(t.lexeme).toLowerCase() !== lex.toLowerCase()) {
      throw this.errorHere(msg || `Se esperaba '${lex}'`);
    }
    this.pos++;
    return t;
  }

  expectType(type, msg) {
    const t = this.current();
    if (!t || t.type !== type) {
      throw this.errorHere(msg || `Se esperaba token de tipo ${type}`);
    }
    this.pos++;
    return t;
  }

  errorHere(msg) {
    const t = this.current() || { line: 0, column: 0, lexeme: "EOF" };
    const err = new Error(msg);
    err.line = t.line;
    err.column = t.column;
    err.lexeme = t.lexeme;
    return err;
  }

  // ====== Entrada principal ======

  parseProgram() {
    // program Nombre ; Block .
    this.matchErrors = []; // para acumular errores en lugar de tirar todo

    try {
      const progTok = this.expectLex("program", "Se esperaba 'program' al inicio.");
      const nameTok = this.expectType("IDENTIFICADOR", "Se esperaba nombre del programa.");
      this.expectLex(";", "Se esperaba ';' después del encabezado del programa.");

      const block = this.parseBlock();

      this.expectLex(".", "Se esperaba '.' al final del programa.");

      return {
        kind: "Program",
        name: nameTok.lexeme,
        block,
      };
    } catch (e) {
      if (e && e.line != null) {
        this.matchErrors.push({
          line: e.line,
          column: e.column,
          lexeme: e.lexeme,
          msg: e.message,
        });
      } else {
        this.matchErrors.push({
          line: 0,
          column: 0,
          lexeme: "",
          msg: String(e),
        });
      }
      return null;
    }
  }

  parseBlock() {
    const decls = [];

    // Secciones opcionales type / var / const
    let loop = true;
    while (loop) {
      const t = this.current();
      if (!t || t.type !== "PALABRA_RESERVADAS") break;
      const lex = String(t.lexeme).toLowerCase();

      if (lex === "type") {
        this.pos++;
        this.parseTypeSection(decls);
      } else if (lex === "var") {
        this.pos++;
        this.parseVarSection(decls);
      } else if (lex === "const") {
        // TODO: podrías implementar sección const
        this.pos++;
        this.skipConstSection(); 
      } else {
        loop = false;
      }
    }

    // begin ... end
    this.expectLex("begin", "Se esperaba 'begin' para iniciar el bloque de sentencias.");
    const stmts = this.parseStatementList();
    this.expectLex("end", "Se esperaba 'end' para cerrar el bloque.");

    return {
      kind: "Block",
      decls,
      stmts,
    };
  }

  // ====== Sección TYPE ======

  parseTypeSection(decls) {
    // type
    //   MiTipo = integer;
    //   Otro = MiTipo;
    //   ...
    while (true) {
      const id = this.matchType("IDENTIFICADOR");
      if (!id) break;
      this.expectLex("=", "Se esperaba '=' en la declaración de tipo.");
      const type = this.parseSimpleType();
      this.expectLex(";", "Se esperaba ';' al final de la declaración de tipo.");

      decls.push({
        kind: "TypeDecl",
        name: id.lexeme,
        type, // este 'type' es un descriptor (ver parseSimpleType)
      });
    }
  }

  parseSimpleType() {
    // Por ahora: tipo básico o alias simple o array
    const t = this.current();
    if (!t) throw this.errorHere("Se esperaba tipo.");

    // array[1..10] of integer
    if (t.type === "PALABRA_RESERVADAS" && String(t.lexeme).toLowerCase() === "array") {
      this.pos++;
      this.expectLex("[", "Se esperaba '[' en tipo array.");
      const low = this.parseExpression(); // aquí podrías restringir a enteros
      this.expectLex("..", "Se esperaba '..' en tipo array.");
      const high = this.parseExpression();
      this.expectLex("]", "Se esperaba ']' en tipo array.");
      this.expectLex("of", "Se esperaba 'of' en tipo array.");
      const base = this.parseSimpleType();
      return {
        kind: "ArrayType",
        low,
        high,
        base,
      };
    }

    // tipo básico o alias
    if (t.type === "PALABRA_RESERVADAS" || t.type === "IDENTIFICADOR") {
      this.pos++;
      return {
        kind: "NamedType",
        name: t.lexeme,
      };
    }

    throw this.errorHere("Tipo no válido.");
  }

  // ====== Sección VAR ======

  parseVarSection(decls) {
    // var
    //   a, b: integer;
    //   x: MiTipo;
    //   arr: array[1..10] of integer;
    while (true) {
      const firstId = this.matchType("IDENTIFICADOR");
      if (!firstId) break;

      const names = [firstId.lexeme];
      // , id , id ...
      while (this.matchLex(",")) {
        const nextId = this.expectType("IDENTIFICADOR", "Se esperaba identificador después de ','.");
        names.push(nextId.lexeme);
      }

      this.expectLex(":", "Se esperaba ':' en declaración var.");
      const type = this.parseSimpleType();
      this.expectLex(";", "Se esperaba ';' al final de declaración var.");

      decls.push({
        kind: "VarDecl",
        names,
        type,
      });
    }
  }

  skipConstSection() {
    // implementación simplificada: leer hasta que ya no vea IDENTIFICADOR =
    while (true) {
      const id = this.matchType("IDENTIFICADOR");
      if (!id) break;
      if (!this.matchLex("=")) {
        // no parece declaración const formal
        break;
      }
      // saltar expresión hasta ';'
      while (this.current() && String(this.current().lexeme) !== ";") {
        this.pos++;
      }
      this.matchLex(";");
    }
  }

  // ====== Sentencias ======

  parseStatementList() {
    const stmts = [];
    while (true) {
      const t = this.current();
      if (!t) break;
      const lex = String(t.lexeme).toLowerCase();
      if (lex === "end") break; // fin de bloque
      const stmt = this.parseStatement();
      if (stmt) stmts.push(stmt);
      // ; opcional entre statements
      this.matchLex(";");
    }
    return stmts;
  }

  parseStatement() {
    const t = this.current();
    if (!t) return null;
    const lex = String(t.lexeme).toLowerCase();

    if (t.type === "IDENTIFICADOR") {
      // puede ser asignación o llamada
      return this.parseAssignOrCall();
    }
    if (lex === "if") return this.parseIf();
    if (lex === "while") return this.parseWhile();
    if (lex === "for") return this.parseFor();
    if (lex === "begin") {
      // bloque anidado
      this.pos++;
      const inner = this.parseStatementList();
      this.expectLex("end", "Se esperaba 'end' para cerrar bloque interno.");
      return { kind: "Compound", stmts: inner };
    }
    // sentencia vacía
    return null;
  }

  parseAssignOrCall() {
    // IDENT ...
    const idTok = this.expectType("IDENTIFICADOR", "Se esperaba identificador.");

    // mirar siguiente
    if (this.matchLex(":=")) {
      // asignación
      const target = this.buildVarFromId(idTok);
      const expr = this.parseExpression();
      return {
        kind: "Assign",
        target,
        expr,
      };
    } else if (this.matchLex("(")) {
      // llamada a procedimiento/función
      const args = [];
      if (!this.matchLex(")")) {
        do {
          args.push(this.parseExpression());
        } while (this.matchLex(","));
        this.expectLex(")", "Se esperaba ')' al cerrar llamada.");
      }
      return {
        kind: "Call",
        name: idTok.lexeme,
        args,
      };
    } else {
      // puede ser variable sola (no la usamos), la ignoramos o marcamos error.
      return {
        kind: "Var",
        name: idTok.lexeme,
        indexes: [],
        fields: [],
      };
    }
  }

  buildVarFromId(idTok) {
    // soportar: a, a[expr], a[expr1][expr2], a.campo...
    const name = idTok.lexeme;
    const indexes = [];
    const fields = [];

    while (true) {
      if (this.matchLex("[")) {
        const e = this.parseExpression();
        indexes.push(e);
        this.expectLex("]", "Se esperaba ']' en índice de array.");
      } else if (this.matchLex(".")) {
        const f = this.expectType("IDENTIFICADOR", "Se esperaba nombre de campo después de '.'.");
        fields.push(f.lexeme);
      } else {
        break;
      }
    }

    return {
      kind: "Var",
      name,
      indexes,
      fields,
    };
  }

  // ====== IF, WHILE, FOR ======

  parseIf() {
    this.expectLex("if");
    const cond = this.parseExpression();
    this.expectLex("then", "Se esperaba 'then' en if.");
    const thenBranch = this.parseStatement();
    let elseBranch = null;
    if (this.matchLex("else")) {
      elseBranch = this.parseStatement();
    }
    return { kind: "If", cond, thenBranch, elseBranch };
  }

  parseWhile() {
    this.expectLex("while");
    const cond = this.parseExpression();
    this.expectLex("do", "Se esperaba 'do' en while.");
    const body = this.parseStatement();
    return { kind: "While", cond, body };
  }

  parseFor() {
    this.expectLex("for");
    const id = this.expectType("IDENTIFICADOR", "Se esperaba variable de control en for.");
    this.expectLex(":=", "Se esperaba ':=' en for.");
    const from = this.parseExpression();
    let direction = "to";
    if (this.matchLex("to")) {
      direction = "to";
    } else if (this.matchLex("downto")) {
      direction = "downto";
    } else {
      throw this.errorHere("Se esperaba 'to' o 'downto' en for.");
    }
    const to = this.parseExpression();
    this.expectLex("do", "Se esperaba 'do' en for.");
    const body = this.parseStatement();
    return {
      kind: "For",
      varName: id.lexeme,
      from,
      to,
      direction,
      body,
    };
  }

  // ====== EXPRESIONES (precedencia) ======

  parseExpression() {
    return this.parseRelational();
  }

  parseRelational() {
    let left = this.parseAdditive();
    while (true) {
      const t = this.current();
      if (!t) break;
      const lex = String(t.lexeme);
      if (lex === "=" || lex === "<" || lex === ">" || lex === "<=" || lex === ">=" || lex === "<>") {
        this.pos++;
        const right = this.parseAdditive();
        left = { kind: "Binary", op: lex, left, right };
      } else {
        break;
      }
    }
    return left;
  }

  parseAdditive() {
    let left = this.parseTerm();
    while (true) {
      const t = this.current();
      if (!t) break;
      const lex = String(t.lexeme);
      if (lex === "+" || lex === "-" || String(lex).toLowerCase() === "or") {
        this.pos++;
        const right = this.parseTerm();
        left = { kind: "Binary", op: lex.toLowerCase(), left, right };
      } else {
        break;
      }
    }
    return left;
  }

  parseTerm() {
    let left = this.parseFactor();
    while (true) {
      const t = this.current();
      if (!t) break;
      const lex = String(t.lexeme);
      if (lex === "*" || lex === "/" || String(lex).toLowerCase() === "div" || String(lex).toLowerCase() === "mod" || String(lex).toLowerCase() === "and") {
        this.pos++;
        const right = this.parseFactor();
        left = { kind: "Binary", op: lex.toLowerCase(), left, right };
      } else {
        break;
      }
    }
    return left;
  }

  parseFactor() {
    const t = this.current();
    if (!t) throw this.errorHere("Expresión incompleta.");

    // unarios +, -, not
    if (t.lexeme === "+" || t.lexeme === "-" || String(t.lexeme).toLowerCase() === "not") {
      this.pos++;
      const expr = this.parseFactor();
      return { kind: "Unary", op: String(t.lexeme).toLowerCase(), expr };
    }

    // literales
    if (t.type === "NUMERO_ENTERO" || t.type === "NUMERO_HEX" || t.type === "NUMERO_BIN" || t.type === "NUMERO_OCT") {
      this.pos++;
      return { kind: "Literal", value: t.lexeme, typeHint: "integer" };
    }
    if (t.type === "NUMERO_REAL") {
      this.pos++;
      return { kind: "Literal", value: t.lexeme, typeHint: "real" };
    }
    if (t.type === "CADENA") {
      this.pos++;
      return { kind: "Literal", value: t.lexeme, typeHint: "string" };
    }
    if (t.type === "CHAR") {
      this.pos++;
      return { kind: "Literal", value: t.lexeme, typeHint: "char" };
    }
    if (t.type === "PALABRA_RESERVADAS") {
      const low = String(t.lexeme).toLowerCase();
      if (low === "true" || low === "false") {
        this.pos++;
        return { kind: "Literal", value: low, typeHint: "boolean" };
      }
    }

    // paréntesis
    if (t.lexeme === "(") {
      this.pos++;
      const expr = this.parseExpression();
      this.expectLex(")", "Se esperaba ')' en expresión.");
      return expr;
    }

    // identificador: var o llamada
    if (t.type === "IDENTIFICADOR") {
      // para expresiones usaremos variable/función llamada
      const idTok = this.expectType("IDENTIFICADOR");
      // ¿llamada?
      if (this.matchLex("(")) {
        const args = [];
        if (!this.matchLex(")")) {
          do {
            args.push(this.parseExpression());
          } while (this.matchLex(","));
          this.expectLex(")", "Se esperaba ')' al cerrar llamada.");
        }
        return {
          kind: "CallExpr",
          name: idTok.lexeme,
          args,
        };
      }
      // variable (con índices / campos)
      return this.buildVarFromId(idTok);
    }

    throw this.errorHere("Factor inválido en expresión.");
  }
}

// Exportar para uso en main.js
if (typeof window !== "undefined") {
  window.ParserPascal = Parser;
}
