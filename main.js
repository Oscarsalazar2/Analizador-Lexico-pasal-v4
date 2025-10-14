document.addEventListener("DOMContentLoaded", () => {
  const $ = (id) => {
    const el = document.getElementById(id);
    if (!el) console.warn("Falta el elemento con id:", id);
    return el;
  };
  const escapeHtml = (str) =>
    String(str)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;");

  // ========= CodeMirror =========
  const cmTextarea = document.getElementById("codigo");
  const editor = CodeMirror.fromTextArea(cmTextarea, {
    mode: "text/x-pascal",
    lineNumbers: true,
    theme: "pascalLight",
    tabSize: 2,
    indentUnit: 2,
    lineWrapping: true,
  });

  let cmMarks = [];
  let cmLineClasses = [];
  function clearEditorMarks() {
    cmMarks.forEach((m) => m.clear());
    cmMarks = [];
    cmLineClasses.forEach(({ line, where, cls }) =>
      editor.removeLineClass(line, where, cls)
    );
    cmLineClasses = [];
  }
  function markErrorsInEditor(tokens, allErrs) {
    clearEditorMarks();
    // Marcas por tokens de error (rango exacto)
    for (const t of tokens) {
      if (
        t.type === "ERROR" &&
        typeof t.lexeme === "string" &&
        t.lexeme !== "<prevalidación>" &&
        t.lexeme !== "<postvalidación>"
      ) {
        const line0 = t.line - 1;
        const ch0 = t.column - 1;
        const ch1 = ch0 + String(t.lexeme).length;
        const mark = editor.markText(
          { line: line0, ch: ch0 },
          { line: line0, ch: ch1 },
          { className: "cm-lex-error" }
        );
        cmMarks.push(mark);
      }
    }
    // Marcas por errores de validación (línea completa)
    for (const e of allErrs) {
      const line0 = e.line - 1;
      editor.addLineClass(line0, "wrap", "cm-pre-error");
      cmLineClasses.push({ line: line0, where: "wrap", cls: "cm-pre-error" });
    }
  }

  // ========= Palabras/Librerías =========
  const palabrasreservadas = new Set([
    "program",
    "si",
    "otrocaso",
    "const",
    "type",
    "var",
    "array",
    "of",
    "record",
    "function",
    "procedure",
    "begin",
    "end",
    "if",
    "then",
    "else",
    "while",
    "do",
    "for",
    "to",
    "downto",
    "repeat",
    "until",
    "case",
    "with",
    "goto",
    "and",
    "or",
    "not",
    "div",
    "mod",
    "true",
    "false",
    "integer",
    "real",
    "boolean",
    "char",
    "string",
    "write",
    "writeln",
    "read",
    "readln",
    "uses",
    "input",
    "output",
    "xor",
    "shl",
    "shr",
    "in",
  ]);
  const libreria = new Set([
    "system",
    "sysutils",
    "strutils",
    "math",
    "dateutils",
    "types",
    "variants",
    "typinfo",
    "crt",
    "wincrt",
    "video",
    "keyboard",
    "mouse",
    "printers",
    "dos",
    "fileutil",
    "baseunix",
    "unix",
    "windows",
    "process",
    "shell",
    "inifiles",
    "graph",
    "wingraph",
    "dialogs",
    "forms",
    "controls",
    "graphics",
    "extctrls",
    "menus",
    "classes",
    "contnrs",
    "rtlconsts",
    "syncobjs",
    "sockets",
    "ssockets",
    "fphttpclient",
    "fpftpclient",
    "fpjson",
    "jsonparser",
    "xmlread",
    "netdb",
    "db",
    "sqldb",
    "sqlite3conn",
    "mysql57conn",
    "pqconnection",
    "odbcconn",
    "threads",
    "mtprocs",
    "mathconsts",
    "comobj",
    "registry",
    "filectrl",
  ]);

  // ========= Códigos de token =========
  const TOKEN_CODES = {
    PALABRA_RESERVADAS: 0,
    LIBRERIA: 1,
    IDENTIFICADOR: 2,
    NUMERO: 3,
    CADENA: 4,
    COMENTARIO: 5,
    DIRECTIVA: 6,
    ASIGNACION: 7,
    CONDICIONES: 8,
    OPERADOR: 11,
    SIMBOLOS_ESPECIALES: 12,
    ERROR: 99,
    NUMERO_ENTERO: 3,
    NUMERO_REAL: 3,
    IGUAL: 8,
    MENOR: 8,
    MAYOR: 8,
    MENOR_IGUAL: 8,
    MAYOR_IGUAL: 8,
    DESIGUALDAD: 8,
    SUMA: 11,
    RESTA: 11,
    MULTIPLICACION: 11,
    DIVISION: 11,
    CARET: 11,
    ARROBA: 11,
    PUNTO_COMA: 12,
    DOS_PUNTOS: 12,
    COMA: 12,
    PUNTO: 12,
    PARENTESIS_ABIERTO: 12,
    PARENTESIS_CERRADO: 12,
    CORCHETE_ABIERTO: 12,
    CORCHETE_CERRADO: 12,
    PUNTOPUNTO: 12,
    CHARCODE: 13,
    CHAR: 4,
    NUMERO_HEX: 3,
    NUMERO_BIN: 3,
    NUMERO_OCT: 3,
  };
  const TOKEN_CODES_TABLA = {
    PALABRA_RESERVADAS: 0,
    LIBRERIA: 1,
    IDENTIFICADOR: 2,
    NUMERO: 3,
    CADENA: 4,
    COMENTARIO: 5,
    DIRECTIVA: 6,
    ASIGNACION: 7,
    CONDICIONES: 8,
    OPERADOR: 11,
    SIMBOLOS_ESPECIALES: 12,
    CHARCODE: 13,
    ERROR: 99,
  };

  // ========= Prevalidación (rápida) =========
  function prevalidar(codigo) {
    const errores = [];
    let state = "normal";
    let line = 1,
      col = 1;
    let strStart = { line: 1, col: 1 };
    let braceStart = { line: 1, col: 1 };
    let parenStart = { line: 1, col: 1 };

    for (let i = 0; i < codigo.length; i++) {
      const ch = codigo[i];
      const next = codigo[i + 1];

      if (state === "normal") {
        if (ch === "'") {
          state = "string";
          strStart = { line, col };
        } else if (ch === "{") {
          state = "braceComment";
          braceStart = { line, col };
        } else if (ch === "(" && next === "*") {
          state = "parenComment";
          parenStart = { line, col };
          i++;
          col++;
        } else if (ch === "/" && next === "/") {
          state = "lineComment";
          i++;
          col++;
        }
      } else if (state === "string") {
        if (ch === "'") {
          if (next === "'") {
            i++;
            col++;
          } else {
            state = "normal";
          }
        } else if (ch === "\n" || ch === "\r") {
          errores.push({
            line: strStart.line,
            column: strStart.col,
            msg: "Cadena no puede ser multilínea o está sin cerrar.",
          });
          state = "normal";
        }
      } else if (state === "lineComment") {
        if (ch === "\n") state = "normal";
      } else if (state === "braceComment") {
        if (ch === "}") state = "normal";
      } else if (state === "parenComment") {
        if (ch === "*" && next === ")") {
          state = "normal";
          i++;
          col++;
        }
      }

      if (ch === "\n") {
        line++;
        col = 1;
      } else {
        col++;
      }
    }

    if (state === "string")
      errores.push({
        line: strStart.line,
        column: strStart.col,
        msg: "Cadena sin cerrar (').",
      });
    if (state === "braceComment")
      errores.push({
        line: braceStart.line,
        column: braceStart.col,
        msg: "Comentario { } sin cerrar.",
      });
    if (state === "parenComment")
      errores.push({
        line: parenStart.line,
        column: parenStart.col,
        msg: "Comentario (* *) sin cerrar.",
      });

    // Enmascara strings, comentarios y TAMBIÉN CHARCODE (#$xx, #nn)
    const maskRegex =
      /'(?:''|[^'])*'|\(\*[\s\S]*?\*\)|\{[\s\S]*?\}|\/\/.*$|#\$[0-9A-Fa-f]+|#\d+/gm;
    const vis = codigo.replace(maskRegex, (m) => m.replace(/[^\r\n]/g, " "));

    const patronIdentNum = /\b\d+(?!\.\d)(?:[A-DF-Za-df-z_][A-Za-z0-9_]*)\b/g;
    const patronMultiPunto = /\b\d+\.\d+\.\d+(?:[eE][+-]?\d+)?\b/g;
    const patronExpIncompleto = /\b\d+(?:\.\d+)?[eE](?![+-]?\d)/g;
    const patronIdentConGuion = /\b[A-Za-z_][A-Za-z0-9_]*-[A-Za-z0-9_]+\b/g;

    function agregarErrores(patron, msg) {
      patron.lastIndex = 0;
      let m;
      while ((m = patron.exec(vis)) !== null) {
        const idx = m.index;
        const before = vis.slice(0, idx);
        const l = (before.match(/\n/g) || []).length + 1;
        const lastCr = before.lastIndexOf("\n");
        const c = lastCr === -1 ? idx + 1 : idx - lastCr;
        errores.push({ line: l, column: c, msg });
      }
    }
    agregarErrores(
      patronIdentNum,
      "Identificador no puede iniciar con número."
    );
    agregarErrores(patronMultiPunto, "Número con múltiples puntos decimales.");
    agregarErrores(patronExpIncompleto, "Notación científica incompleta.");
    agregarErrores(
      patronIdentConGuion,
      "Guion '-' no permitido en identificadores; usa '_' en su lugar."
    );

    return errores;
  }

  // ========= Tokenizador =========
  function tokenizarBase(codigo) {
    const tokens = [];
    const tokenRE = new RegExp(
      [
        "(\\s+)", // 1 . espacios
        "(\\{\\$[^}]*\\})", // 2 . DIRECTIVA {$...}
        "(\\(\\*[\\s\\S]*?\\*\\))", // 3 . comentario (* *)
        "(\\{[\\s\\S]*?\\})", // 4 . comentario { }
        "(//[^\\n]*)", // 5 . comentario //
        "('(?:''|[^'])*')", // 6 . cadena/char
        "(\\d+[A-Za-z_][A-Za-z0-9_]*)", // 7 . ident inicia con dígito => ERROR
        "(\\d+\\.\\d+\\.\\d+(?:[eE][+-]?\\d+)?)", // 8 . múltiples puntos => ERROR
        "(\\d+(?:\\.\\d+)?[eE](?![+-]?\\d))", // 9 . exp incompleta => ERROR
        "(#\\d+|#\\$[0-9A-Fa-f]+)", // 10. CHARCODE
        "(\\.\\.)", // 11. ..
        "(:=|<=|>=|<>)", // 12. dobles válidos
        "(\\+=|\\-=|\\*=|/=|\\+\\+|--|==|&&|\\|\\|)", // 13. compuestos NO-Pascal => ERROR
        "([+\\-*/=<>^@])", // 14. operadores simples
        "([;:.,()\\[\\]])", // 15. delimitadores
        "([A-Za-z_][A-Za-z0-9_]*)", // 16. id / palabra reservada / librería
        "((?:\\d+\\.\\d+|\\d+\\.(?!\\.)|\\d+)(?:[eE][+-]?\\d+)?)", // 17. número
        "(\\$[0-9A-Fa-f]+)", // 18. HEX ($FF)
        "(%[01]+)", // 19. BIN (%1010)
        "(&[0-7]+)", // 20. OCT (&77)
        "(.)", // 21. otro => ERROR
      ].join("|"),
      "g"
    );

    let m;
    while ((m = tokenRE.exec(codigo)) !== null) {
      const idx = m.index;
      const before = codigo.slice(0, idx);
      const line = (before.match(/\n/g) || []).length + 1;
      const lastCr = before.lastIndexOf("\n");
      const column = lastCr === -1 ? idx + 1 : idx - lastCr;

      if (m[1]) continue; // espacios

      if (m[2]) {
        // DIRECTIVA
        const type = "DIRECTIVA";
        tokens.push({
          type,
          lexeme: m[2],
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }
      if (m[3] || m[4] || m[5]) {
        // COMENTARIO
        const type = "COMENTARIO";
        const lex = m[3] || m[4] || m[5];
        tokens.push({
          type,
          lexeme: lex,
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }
      if (m[6]) {
        // CADENA o CHAR
        const raw = m[6];
        const inner = raw.slice(1, -1).replace(/''/g, "'");
        const type = inner.length === 1 ? "CHAR" : "CADENA";
        tokens.push({
          type,
          lexeme: inner,
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }

      // ERRORES específicos
      if (m[7]) {
        tokens.push({
          type: "ERROR",
          lexeme: m[7],
          line,
          column,
          code: TOKEN_CODES.ERROR,
          msg: "Identificador no puede iniciar con número.",
        });
        continue;
      }
      if (m[8]) {
        tokens.push({
          type: "ERROR",
          lexeme: m[8],
          line,
          column,
          code: TOKEN_CODES.ERROR,
          msg: "Número con múltiples puntos decimales.",
        });
        continue;
      }
      if (m[9]) {
        tokens.push({
          type: "ERROR",
          lexeme: m[9],
          line,
          column,
          code: TOKEN_CODES.ERROR,
          msg: "Notación científica incompleta.",
        });
        continue;
      }

      if (m[10]) {
        // CHARCODE
        const type = "CHARCODE";
        tokens.push({
          type,
          lexeme: m[10],
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }
      if (m[11]) {
        // ..
        const type = "PUNTOPUNTO";
        tokens.push({
          type,
          lexeme: m[11],
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }
      if (m[12]) {
        // := <= >= <>
        const map = {
          ":=": "ASIGNACION",
          "<=": "MENOR_IGUAL",
          ">=": "MAYOR_IGUAL",
          "<>": "DESIGUALDAD",
        };
        const type = map[m[12]] || "ERROR";
        tokens.push({
          type,
          lexeme: m[12],
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }
      if (m[13]) {
        // compuestos inválidos
        tokens.push({
          type: "ERROR",
          lexeme: m[13],
          line,
          column,
          code: TOKEN_CODES.ERROR,
          msg: "Operador no válido en Pascal.",
        });
        continue;
      }

      if (m[14]) {
        // operadores simples
        const map = {
          "+": "SUMA",
          "-": "RESTA",
          "*": "MULTIPLICACION",
          "/": "DIVISION",
          "=": "IGUAL",
          "<": "MENOR",
          ">": "MAYOR",
          "^": "CARET",
          "@": "ARROBA",
        };
        const type = map[m[14]] || "ERROR";
        tokens.push({
          type,
          lexeme: m[14],
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }
      if (m[15]) {
        // delimitadores
        const map = {
          ";": "PUNTO_COMA",
          ":": "DOS_PUNTOS",
          ",": "COMA",
          ".": "PUNTO",
          "(": "PARENTESIS_ABIERTO",
          ")": "PARENTESIS_CERRADO",
          "[": "CORCHETE_ABIERTO",
          "]": "CORCHETE_CERRADO",
        };
        const type = map[m[15]] || "ERROR";
        tokens.push({
          type,
          lexeme: m[15],
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }
      if (m[16]) {
        // id / palabra reservada / librería
        const lexLower = m[16].toLowerCase();
        let type;
        if (palabrasreservadas.has(lexLower)) type = "PALABRA_RESERVADAS";
        else if (libreria.has(lexLower)) type = "LIBRERIA";
        else type = "IDENTIFICADOR";
        tokens.push({
          type,
          lexeme: m[16],
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }
      if (m[17]) {
        // número (int/real)
        let lex = m[17];
        const esReal = /[.eE]/.test(lex);
        if (!esReal && /^0\d+$/.test(lex)) {
          tokens.push({
            type: "ERROR",
            lexeme: lex,
            line,
            column,
            code: TOKEN_CODES.ERROR,
            msg: "Entero con cero inicial no permitido (p.ej., '08').",
          });
          continue;
        }
        if (esReal && /\d+\.(?!\.)$/.test(lex)) lex += "0"; // 3. -> 3.0
        const type = esReal ? "NUMERO_REAL" : "NUMERO_ENTERO";
        tokens.push({
          type,
          lexeme: lex,
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }
      if (m[18]) {
        tokens.push({
          type: "NUMERO_HEX",
          lexeme: m[18],
          line,
          column,
          code: TOKEN_CODES.NUMERO_HEX ?? 3,
        });
        continue;
      }
      if (m[19]) {
        tokens.push({
          type: "NUMERO_BIN",
          lexeme: m[19],
          line,
          column,
          code: TOKEN_CODES.NUMERO_BIN ?? 3,
        });
        continue;
      }
      if (m[20]) {
        tokens.push({
          type: "NUMERO_OCT",
          lexeme: m[20],
          line,
          column,
          code: TOKEN_CODES.NUMERO_OCT ?? 3,
        });
        continue;
      }

      if (m[21]) {
        tokens.push({
          type: "ERROR",
          lexeme: m[21],
          line,
          column,
          code: TOKEN_CODES.ERROR,
          msg: "Carácter no reconocido por el lenguaje.",
        });
        continue;
      }
    }
    return tokens;
  }

  // ========= Parche: aceptar .45 como real =========
  function lineColToIndex(src, line, col) {
    let l = 1,
      c = 1;
    for (let i = 0; i < src.length; i++) {
      if (l === line && c === col) return i;
      const ch = src[i];
      if (ch === "\n") {
        l++;
        c = 1;
      } else {
        c++;
      }
    }
    return src.length;
  }
  function patchLeadingDotNumbers(src) {
    const re = /(^|[^.\d])\.(\d+(?:[eE][+-]?\d+)?)/g;
    let code = "",
      map = [],
      iOrig = 0;
    function pushChunk(str) {
      code += str;
      for (let k = 0; k < str.length; k++) map.push(iOrig + k);
    }

    let m,
      lastIndex = 0;
    while ((m = re.exec(src)) !== null) {
      const idx = m.index;
      const before = src.slice(lastIndex, idx);
      pushChunk(before);
      iOrig += before.length;

      const prev = m[1],
        numberPart = m[2];
      const replacement = prev + "0." + numberPart;
      code += replacement;

      for (let k = 0; k < prev.length; k++) map.push(iOrig + k);
      map.push(-1); // '0' insertado
      map.push(iOrig + prev.length); // '.'
      for (let k = 0; k < numberPart.length; k++)
        map.push(iOrig + prev.length + 1 + k);

      iOrig += prev.length + 1 + numberPart.length;
      lastIndex = idx + prev.length + 1 + numberPart.length;
    }
    const tail = src.slice(lastIndex);
    pushChunk(tail);
    iOrig += tail.length;
    return { codePatched: code, patchMap: map };
  }
  function tokenizarPatched(codigoOriginal) {
    const { codePatched, patchMap } = patchLeadingDotNumbers(codigoOriginal);
    const tokens = tokenizarBase(codePatched);

    for (const t of tokens) {
      if (
        !t ||
        (t.type !== "NUMERO_REAL" &&
          t.type !== "NUMERO" &&
          t.type !== "NUMERO_ENTERO")
      )
        continue;
      const idxPatched = lineColToIndex(codePatched, t.line, t.column);

      let idxP0 = idxPatched;
      if (patchMap[idxP0] === -1 && codePatched[idxP0] === "0") {
        const idxPatchedDot = idxP0 + 1;
        const iOrigDot = patchMap[idxPatchedDot];
        if (
          iOrigDot != null &&
          iOrigDot >= 0 &&
          codigoOriginal[iOrigDot] === "."
        ) {
          t.column = Math.max(1, t.column - 1);
          if (typeof t.lexeme === "string" && t.lexeme.startsWith("0."))
            t.lexeme = "." + t.lexeme.slice(2);
          if (t.type === "NUMERO" || t.type === "NUMERO_ENTERO") {
            t.type = "NUMERO_REAL";
            t.code = TOKEN_CODES.NUMERO_REAL ?? 3;
          }
        }
      } else {
        const iOrig = patchMap[idxP0];
        if (iOrig != null && iOrig >= 0 && codigoOriginal[iOrig] === ".") {
          if (typeof t.lexeme === "string" && t.lexeme.startsWith("0."))
            t.lexeme = "." + t.lexeme.slice(2);
          if (t.type === "NUMERO" || t.type === "NUMERO_ENTERO") {
            t.type = "NUMERO_REAL";
            t.code = TOKEN_CODES.NUMERO_REAL ?? 3;
          }
        }
      }
    }
    return tokens;
  }

  // ========= Utilidades =========
  function indexToLineCol(str, idx) {
    let line = 1,
      col = 1;
    for (let i = 0; i < idx; i++) {
      if (str[i] === "\n") {
        line++;
        col = 1;
      } else col++;
    }
    return { line, column: col };
  }

  // ========= Escaneo crudo de paréntesis/corchetes =========
  function scanDelimsRaw(src) {
    const errs = [];
    let i = 0,
      line = 1,
      col = 1;
    const stack = [];
    const pushOpen = (ch) => stack.push({ ch, line, col });
    const pushErr = (lexeme, msg) =>
      errs.push({ line, column: col, lexeme, msg });

    while (i < src.length) {
      const ch = src[i],
        next = src[i + 1];

      // Strings
      if (ch === "'") {
        i++;
        col++;
        while (i < src.length) {
          const c = src[i],
            n = src[i + 1];
          if (c === "'" && n === "'") {
            i += 2;
            col += 2;
            continue;
          }
          if (c === "'") {
            i++;
            col++;
            break;
          }
          if (c === "\n") {
            break;
          }
          i++;
          col++;
        }
        continue;
      }
      // // comentario de línea
      if (ch === "/" && next === "/") {
        i += 2;
        col += 2;
        while (i < src.length && src[i] !== "\n") {
          i++;
          col++;
        }
        continue;
      }
      // { ... } incluye {$...}
      if (ch === "{") {
        i++;
        col++;
        while (i < src.length && src[i] !== "}") {
          if (src[i] === "\n") {
            i++;
            line++;
            col = 1;
          } else {
            i++;
            col++;
          }
        }
        if (i < src.length) {
          i++;
          col++;
        }
        continue;
      }
      // (* ... *)
      if (ch === "(" && next === "*") {
        i += 2;
        col += 2;
        while (i < src.length) {
          if (src[i] === "*" && src[i + 1] === ")") {
            i += 2;
            col += 2;
            break;
          }
          if (src[i] === "\n") {
            i++;
            line++;
            col = 1;
          } else {
            i++;
            col++;
          }
        }
        continue;
      }

      // Delimitadores
      if (ch === "(" || ch === "[") {
        pushOpen(ch);
        i++;
        col++;
        continue;
      }
      if (ch === ")" || ch === "]") {
        if (!stack.length) {
          pushErr(
            ch,
            ch === ")"
              ? "Paréntesis ')' sin apertura."
              : "Corchete ']' sin apertura."
          );
          i++;
          col++;
          continue;
        }
        const top = stack[stack.length - 1];
        const ok =
          (top.ch === "(" && ch === ")") || (top.ch === "[" && ch === "]");
        if (ok) stack.pop();
        else
          pushErr(
            ch,
            ch === ")"
              ? "Paréntesis ')' sin apertura."
              : "Corchete ']' sin apertura."
          );
        i++;
        col++;
        continue;
      }

      // Avance
      if (ch === "\n") {
        i++;
        line++;
        col = 1;
      } else {
        i++;
        col++;
      }
    }

    // Aberturas sin cerrar
    for (const u of stack) {
      errs.push({
        line: u.line,
        column: u.col,
        lexeme: u.ch,
        msg:
          u.ch === "("
            ? "Paréntesis '(' sin cerrar."
            : "Corchete '[' sin cerrar.",
      });
    }
    return errs;
  }

  // ========= Comentarios avanzados =========
  function validateCommentsAdvanced(src) {
    const errs = [];
    let state = "normal";
    let i = 0;
    const pos = (idx) => indexToLineCol(src, idx);

    while (i < src.length) {
      const ch = src[i],
        next = src[i + 1];

      if (state === "normal") {
        if (ch === "'") {
          i++;
          while (i < src.length) {
            if (src[i] === "'" && src[i + 1] === "'") {
              i += 2;
              continue;
            }
            if (src[i] === "'") {
              i++;
              break;
            }
            if (src[i] === "\n" || src[i] === "\r") break;
            i++;
          }
          continue;
        }
        if (ch === "{") {
          state = "brace";
          i++;
          continue;
        }
        if (ch === "(" && next === "*") {
          state = "paren";
          i += 2;
          continue;
        }
        if (ch === "/" && next === "/") {
          while (i < src.length && src[i] !== "\n") i++;
          continue;
        }

        if (ch === "}") {
          const p = pos(i);
          errs.push({
            line: p.line,
            column: p.column,
            msg: "Cierre '}' sin apertura.",
            lexeme: "}",
          });
        }
        if (ch === "*" && next === ")") {
          const p = pos(i);
          errs.push({
            line: p.line,
            column: p.column,
            msg: "Cierre '*)' sin apertura.",
            lexeme: "*)",
          });
        }
        i++;
      } else if (state === "brace") {
        if (ch === "{") {
          const p = pos(i);
          errs.push({
            line: p.line,
            column: p.column,
            msg: "Comentario { } anidado no permitido.",
            lexeme: "{",
          });
        }
        if (ch === "}") state = "normal";
        i++;
      } else if (state === "paren") {
        if (ch === "(" && next === "*") {
          const p = pos(i);
          errs.push({
            line: p.line,
            column: p.column,
            msg: "Comentario (* *) anidado no permitido.",
            lexeme: "(*",
          });
        }
        if (ch === "*" && next === ")") {
          state = "normal";
          i += 2;
          continue;
        }
        i++;
      }
    }
    return errs;
  }

  // ========= Post-validación (string-based) =========
  const ERR_CODE = TOKEN_CODES.ERROR || 99;
  const maskRegexAll = /'(?:''|[^'])*'|\(\*[\s\S]*?\*\)|\{[\s\S]*?\}|\/\/.*$/gm;

  function postValidar(src) {
    const errs = [];
    const vis = src.replace(maskRegexAll, (m) => m.replace(/[^\r\n]/g, " "));

    // Operadores prohibidos
    const reBadOps = /\+\+|--|\*\*|==|&&|\|\||:=:/g;
    // ...
    const reTripleDots = /\.\.\.(?!\.)/g;
    // Charcode dec/hex
    const reCharDec = /#(\d+)/g;
    const reCharHex = /#\$([0-9A-Fa-f]+)/g;
    // Strings (para detectar barras invertidas dentro)
    const reString = /'((?:''|[^'])*)'/g;
    // Identificadores MUY largos
    const reTooLongId = /\b[A-Za-z_][A-Za-z0-9_]{63,}\b/g;
    // Números con _
    const reNumUnders = /\b\d[\d_]*_\d[\d_]*\b/g;

    const scan = (re, text, fn) => {
      re.lastIndex = 0;
      let m;
      while ((m = re.exec(text)) !== null) {
        if (re.lastIndex === m.index) re.lastIndex++;
        fn(m);
      }
    };
    const pushAt = (idx, len, msg, lx = null) => {
      const p = indexToLineCol(src, idx);
      errs.push({
        line: p.line,
        column: p.column,
        msg,
        lexeme: lx ?? src.slice(idx, idx + len),
      });
    };

    scan(reBadOps, vis, (m) =>
      pushAt(m.index, m[0].length, "Operador no válido en Pascal.", m[0])
    );
    scan(reTripleDots, vis, (m) =>
      pushAt(m.index, 3, "Secuencia '...' no válida. Use '..' para rangos.")
    );

    scan(reCharDec, vis, (m) => {
      const val = parseInt(m[1], 10);
      if (!(val >= 0 && val <= 255))
        pushAt(
          m.index,
          m[0].length,
          "CHARCODE decimal fuera de rango (0..255)."
        );
    });
    scan(reCharHex, vis, (m) => {
      const hex = m[1];
      const val = parseInt(hex, 16);
      if (!/^[0-9A-Fa-f]+$/.test(hex))
        pushAt(m.index, m[0].length, "CHARCODE hexadecimal inválido.");
      else if (!(val >= 0 && val <= 255))
        pushAt(
          m.index,
          m[0].length,
          "CHARCODE hexadecimal fuera de rango (0..FF)."
        );
    });

    scan(reString, src, (sm) => {
      const inner = sm[1];
      const base = sm.index + 1;
      const bs = /\\[A-Za-z\\'"]/.exec(inner);
      if (bs)
        pushAt(
          base + bs.index,
          bs[0].length,
          "Secuencia con '\\' no válida en Pascal. Usa CHARCODE (#10) o comillas dobles ''.",
          bs[0]
        );
    });

    scan(reTooLongId, vis, (m) =>
      pushAt(
        m.index,
        m[0].length,
        `Identificador demasiado largo (${m[0].length}). Límite sugerido: 63.`,
        m[0]
      )
    );
    scan(reNumUnders, vis, (m) =>
      pushAt(
        m.index,
        m[0].length,
        "Separadores '_' en números no soportados por este analizador.",
        m[0]
      )
    );

    // Delimitadores y comentarios avanzados
    errs.push(...scanDelimsRaw(src));
    errs.push(...validateCommentsAdvanced(src));

    return errs;
  }

  // ========= Post-validación con tokens (punto aislado 1 . 10) =========
  function postValidarConTokens(tokens) {
    const errs = [];
    const isNum = (t) =>
      t &&
      (t.type === "NUMERO_ENTERO" ||
        t.type === "NUMERO_REAL" ||
        t.type === "NUMERO_HEX" ||
        t.type === "NUMERO_BIN" ||
        t.type === "NUMERO_OCT");

    for (let i = 1; i < tokens.length - 1; i++) {
      const prev = tokens[i - 1],
        cur = tokens[i],
        next = tokens[i + 1];
      if (cur.type === "PUNTO" && isNum(prev) && isNum(next)) {
        errs.push({
          line: cur.line,
          column: cur.column,
          lexeme: ".",
          msg: "Punto aislado entre números. Usa '..' para rangos o '1.0' para reales.",
        });
      }
    }
    return errs;
  }

  // ========= Dedupe y orden =========
  function dedupeErrObjs(arr) {
    const seen = new Set(),
      out = [];
    for (const e of arr) {
      const key = `${e.line}|${e.column}|${e.msg}`;
      if (seen.has(key)) continue;
      seen.add(key);
      out.push(e);
    }
    return out;
  }
  function dedupeErrorTokens(tokens) {
    const seen = new Set(),
      out = [];
    for (const t of tokens) {
      if (t.type === "ERROR") {
        const key = `${t.line}|${t.column}|${t.msg || t.lexeme}`;
        if (seen.has(key)) continue;
        seen.add(key);
      }
      out.push(t);
    }
    return out;
  }
  function sortTokensForDisplay(tokens) {
    return [...tokens].sort((a, b) => {
      const ae = a.type === "ERROR" ? 0 : 1;
      const be = b.type === "ERROR" ? 0 : 1;
      if (ae !== be) return ae - be;
      if (a.line !== b.line) return a.line - b.line;
      if (a.column !== b.column) return a.column - b.column;
      return 0;
    });
  }

  // ========= Render =========
  function renderTokens(tokens) {
    const tbody = $("tokensBody");
    if (!tbody) return;
    if (!tokens.length) {
      tbody.innerHTML =
        '<tr><td colspan="6">Sin tokens (entrada vacía o sólo espacios).</td></tr>';
      return;
    }
    tbody.innerHTML = tokens
      .map((t, i) => {
        const lex = escapeHtml(String(t.lexeme));
        const msg = t.msg ? escapeHtml(String(t.msg)) : "";
        const rowClass = t.type === "ERROR" ? "error-row" : "";
        return `<tr class="${rowClass}" data-line="${t.line}" data-col="${
          t.column
        }" data-len="${String(t.lexeme ?? "").length}">
        <td>${i + 1}</td>
        <td>${t.line}:${t.column}</td>
        <td>${t.type}</td>
        <td><pre style="margin:0;background:transparent;border:0;padding:0">${lex}</pre></td>
        <td>${t.code}</td>
        <td class="error-msg">${msg}</td>
      </tr>`;
      })
      .join("");
  }
  function renderErrores(errs) {
    const box = $("erroresBox");
    if (!box) return;
    box.innerHTML = errs
      .map(
        (e) =>
          `<div class="err">L${e.line}:C${e.column} — ${escapeHtml(
            e.msg
          )}</div>`
      )
      .join("");
  }

  // ========= Análisis unificado =========
  let lastTokens = [];
  function analyzeAll() {
    const codigo = editor.getValue();

    // 1) Prevalidación
    const errsPre = prevalidar(codigo);
    // 2) Tokenización (.45 patch)
    const tokensMain = tokenizarPatched(codigo);
    // 3) Post-validación (texto crudo)
    const errsPost = postValidar(codigo);
    // 4) Post-validación con tokens (punto aislado)
    const errsTok = postValidarConTokens(tokensMain);

    // 5) Convertir errores a tokens y unir
    const ERR_CODE_LOCAL = TOKEN_CODES.ERROR || 99;
    const tokens = [...tokensMain];
    if (errsPre.length) {
      tokens.unshift(
        ...errsPre.map((e) => ({
          type: "ERROR",
          lexeme: "<prevalidación>",
          line: e.line,
          column: e.column,
          code: ERR_CODE_LOCAL,
          msg: e.msg,
        }))
      );
    }
    if (errsPost.length || errsTok.length) {
      tokens.push(
        ...errsPost.concat(errsTok).map((e) => ({
          type: "ERROR",
          lexeme: e.lexeme ?? "<postvalidación>",
          line: e.line,
          column: e.column,
          code: ERR_CODE_LOCAL,
          msg: e.msg,
        }))
      );
    }

    // 6) Dedupe
    const errsAll = errsPre.concat(errsPost, errsTok);
    const errsMerged = dedupeErrObjs(errsAll);
    const tokensDedup = dedupeErrorTokens(tokens);

    // 7) Ordenar tokens para mostrar (errores arriba)
    const tokensDisplay = sortTokensForDisplay(tokensDedup);

    // 8) Render + marcas
    renderTokens(tokensDisplay);
    renderErrores(errsMerged);
    markErrorsInEditor(tokensDedup, errsMerged);
    lastTokens = tokensDedup;
  }

  // ========= Listeners =========
  $("btnAnalizar")?.addEventListener("click", analyzeAll);

  // Click en fila: resaltar línea si es error
  (() => {
    const tbody = document.getElementById("tokensBody");
    if (!tbody) return;
    let lastLine = null;
    tbody.addEventListener("click", (ev) => {
      const tr = ev.target.closest("tr");
      if (!tr) return;
      const isError = tr.classList.contains("error-row");
      if (!isError) return;
      const line = parseInt(tr.getAttribute("data-line"), 10);
      if (!line) return;

      const line0 = line - 1;
      if (lastLine !== null)
        editor.removeLineClass(lastLine, "background", "cm-error-line");
      editor.addLineClass(line0, "background", "cm-error-line");
      lastLine = line0;

      editor.setCursor({ line: line0, ch: 0 });
      editor.scrollIntoView({ line: line0, ch: 0 }, 100);
    });
  })();

  $("btnCargarEjemplo")?.addEventListener("click", () => {
    const sample = `program TorturaLexPascal; { Debe terminar con punto al final del archivo }
{$mode objfpc}{$H+} // DIRECTIVAS
uses SysUtils, Math, Forms; // PALABRAS RESERVADAS + LIBRERÍAS OK
var
  i, j: integer; // OK
  r: real;       // OK
  s: string;     // OK
  c: char;       // OK
  p: ^integer;   // Puntero
  q: ^char;      // Puntero
begin
  i := 0;        // OK
  i := 08;       // ERROR: entero con cero inicial
  i := 123;      // OK
  r := 3.14;     // OK
  r := 3.;       // OK (normaliza a 3.0)
  r := .45e-1;   // OK por el parche
  r := 1.2e3;    // OK
  r := 3.4e;     // ERROR: exponente incompleto
  r := 1.2.3;    // ERROR: múltiples puntos
  s := 'A'#10'B'; // OK CHARCODE
  s := #$0A;     // OK: hex dentro de rango
  s := #$1FF;    // ERROR: hex fuera de rango
  s := #999;     // ERROR: dec fuera de rango
  s := 'Línea con barra \\n'; // ERROR: backslash no válido

  // Operadores y delimitadores
  i := i + j;            // OK
  i := i - j * 2 div 1;  // OK
  if i = j then i := j;  // OK
  i ++;                  // ERROR
  i += 1;                // ERROR
  if i == j then i := j; // ERROR
  i := 1..10;            // OK rango
  i := 1 . 10;           // ERROR: punto aislado

  // Paréntesis/corchetes desbalanceados:
  i := (1 + (2 * 3);     // '(' sin cerrar
  j := [1, 2, 3;         // '[' sin cerrar
  i := 5) + 1;           // ')' sin apertura
  j := 10];              // ']' sin apertura
end.`;
    editor.setValue(sample);
    $("erroresBox") && ($("erroresBox").innerHTML = "");
    $("tokensBody") &&
      ($("tokensBody").innerHTML =
        '<tr><td colspan="6">Presiona <strong>Analizar</strong> para generar tokens.</td></tr>');
    clearEditorMarks();
  });

  $("btnLimpiar")?.addEventListener("click", () => {
    editor.setValue("");
    $("erroresBox") && ($("erroresBox").innerHTML = "");
    $("tokensBody") &&
      ($("tokensBody").innerHTML =
        '<tr><td colspan="6">Presiona <strong>Analizar</strong> para generar tokens.</td></tr>');
    clearEditorMarks();
  });

  // Cargar archivo .pas
  $("btnCargasPas")?.addEventListener("click", (ev) => {
    ev.preventDefault();
    const input = document.createElement("input");
    input.type = "file";
    input.accept = ".pas,.txt";
    input.addEventListener("change", (e) => {
      const f = e.target.files?.[0];
      if (!f) return;
      const reader = new FileReader();
      reader.onload = (ev) => {
        const txt = String(ev.target.result ?? "");
        editor.setValue(txt);
        $("erroresBox") && ($("erroresBox").innerHTML = "");
        $("tokensBody") &&
          ($("tokensBody").innerHTML =
            '<tr><td colspan="6">Presiona <strong>Analizar</strong> para generar tokens.</td></tr>');
        clearEditorMarks();
        editor.focus();
        editor.setCursor({ line: 0, ch: 0 });
      };
      reader.readAsText(f);
    });
    input.click();
  });

  // Descargar .lex
  $("btnDescargar")?.addEventListener("click", () => {
    if (!lastTokens.length) analyzeAll();
    const tokens = lastTokens.length
      ? lastTokens
      : tokenizarPatched(editor.getValue());
    let content = "LINEA:COL\tTIPO\tLEXEMA\tNUMERO\tMENSAJE\n";
    for (const t of tokens) {
      const msg = t.msg ? t.msg : "";
      content += `${t.line}:${t.column}\t${t.type}\t${t.lexeme}\t${t.code}\t${msg}\n`;
    }
    const blob = new Blob([content], { type: "text/plain;charset=utf-8" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = "resultado.lex";
    a.click();
    URL.revokeObjectURL(url);
  });

  // Modal: lista de códigos
  $("btnListaCodigos")?.addEventListener("click", () => {
    const tbody = document.querySelector("#tablaCodigos tbody");
    if (!tbody) return;
    tbody.innerHTML = "";
    for (const [token, numero] of Object.entries(TOKEN_CODES_TABLA)) {
      tbody.innerHTML += `<tr><td>${numero}</td><td>${token}</td></tr>`;
    }
    if ($("modalCodigos")) $("modalCodigos").style.display = "block";
  });
  $("cerrarModal")?.addEventListener("click", () => {
    if ($("modalCodigos")) $("modalCodigos").style.display = "none";
  });
  window.addEventListener("click", (event) => {
    const modal = $("modalCodigos");
    if (event.target === modal) modal.style.display = "none";
  });

  // Estilos mínimos para CodeMirror (marcas)
  const style = document.createElement("style");
  style.textContent = `
    .cm-pre-error { background: rgba(255, 80, 80, .07); }
    .cm-lex-error { background: rgba(255, 0, 0, .15); border-bottom: 1px dotted #c00; }
    .cm-error-line { background: rgba(255, 0, 0, .25); }
  `;
  document.head.appendChild(style);

  clearEditorMarks();
});
