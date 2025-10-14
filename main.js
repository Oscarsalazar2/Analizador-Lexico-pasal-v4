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

  const palabrasreservadas = new Set([
    "program",
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

  const TOKEN_CODES = {
    PALABRA_RESERVADAS: 0,
    LIBRERIA: 1,
    IDENTIFICADOR: 2,
    NUMERO: 3,
    CADENA: 4,
    COMENTARIO: 5,
    ASIGNACION: 7,
    CONDICIONES: 8, // (<, >, <=, >=, =, <>)
    OPERADOR: 11, // (+, -, *, /)
    SIMBOLOS_ESPECIALES: 12, // (; : , . ( ) [ ] ..)
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

    PUNTO_COMA: 12,
    DOS_PUNTOS: 12,
    COMA: 12,
    PUNTO: 12,
    PARENTESIS_ABIERTO: 12,
    PARENTESIS_CERRADO: 12,
    CORCHETE_ABIERTO: 12,
    CORCHETE_CERRADO: 12,
    PUNTOPUNTO: 12,
  };

  const TOKEN_CODES_TABLA = {
    PALABRA_RESERVADAS: 0,
    LIBRERIA: 1,
    IDENTIFICADOR: 2,
    NUMERO: 3,
    CADENA: 4,
    COMENTARIO: 5,
    ASIGNACION: 7,
    CONDICIONES: 8,
    OPERADOR: 11,
    SIMBOLOS_ESPECIALES: 12,
    ERROR: 99,
  };

  // ========= Prevalidación (errores estructurales) =========
  function prevalidar(codigo) {
    const errores = [];

    // a) String sin cerrar / multilínea
    let enStr = false,
      lineaIni = 1,
      colIni = 1;
    let linea = 1,
      col = 1;
    for (let i = 0; i < codigo.length; i++) {
      const ch = codigo[i];
      if (!enStr && ch === "'") {
        enStr = true;
        lineaIni = linea;
        colIni = col;
      } else if (enStr && ch === "'") {
        if (codigo[i + 1] === "'") {
          i++;
          col++;
          continue;
        } // '' escapa
        enStr = false;
      } else if (enStr && (ch === "\n" || ch === "\r")) {
        // <-- también detecta CR (Windows)
        errores.push({
          line: lineaIni,
          column: colIni,
          msg: "Cadena no puede ser multilínea o está sin cerrar.",
        });
        enStr = false;
      }
      if (ch === "\n") {
        linea++;
        col = 1;
      } else {
        col++;
      }
    }
    if (enStr)
      errores.push({
        line: lineaIni,
        column: colIni,
        msg: "Cadena sin cerrar (').",
      });

    // b) Comentarios sin cerrar (no anidados) 
    function buscarSinCerrar(abrir, cerrar, nombre) {
      let idx = 0;
      while (true) {
        const a = codigo.indexOf(abrir, idx);
        if (a === -1) break;
        const c = codigo.indexOf(cerrar, a + abrir.length);
        if (c === -1) {
          const before = codigo.slice(0, a);
          const line = (before.match(/\n/g) || []).length + 1;
          const lastCr = before.lastIndexOf("\n");
          const column = lastCr === -1 ? a + 1 : a - lastCr;
          errores.push({
            line,
            column,
            msg: `Comentario ${nombre} sin cerrar.`,
          });
          break; // reporta el primero
        }
        idx = c + cerrar.length;
      }
    }
    buscarSinCerrar("(*", "*)", "(* *)");
    buscarSinCerrar("{", "}", "{ }");

    return errores;
  }

  function tokenizar(codigo) {
    const tokens = [];
    const tokenRE = new RegExp(
      [
        "(\\s+)", // 1: espacios
        "(\\(\\*[\\s\\S]*?\\*\\))", // 2: comentario (* *)
        "(\\{[\\s\\S]*?\\})", // 3: comentario { }
        "(//[^\\n]*)", // 4: comentario //
        "('(?:''|[^'])*')", // 5: cadena '...'
        "(\\.\\.)", // 6: ..
        "(:=|<=|>=|<>)", // 7: operadores dobles
        "([+\\-*/=<>])", // 8: operadores simples
        "([;:.,()\\[\\]])", // 9: delimitadores
        "([A-Za-z_][A-Za-z0-9_]*)", // 10: identificador
        "((?:\\d+\\.\\d+|\\d+\\.(?!\\.)|\\d+)(?:[eE][+-]?\\d+)?)", // 11: número
        "(.)", // 12: cualquier otro -> ERROR
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

      // comentarios
      if (m[2] || m[3] || m[4]) {
        const type = "COMENTARIO";
        const lex = m[2] || m[3] || m[4];
        tokens.push({
          type,
          lexeme: lex,
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }

      // cadenas
      if (m[5]) {
        const raw = m[5];
        const inner = raw.slice(1, -1).replace(/''/g, "'");
        const type = "CADENA";
        tokens.push({
          type,
          lexeme: inner,
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }

      // ..
      if (m[6]) {
        const type = "PUNTOPUNTO";
        tokens.push({
          type,
          lexeme: m[6],
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }

      // := <= >= <>
      if (m[7]) {
        const map = {
          ":=": "ASIGNACION",
          "<=": "MENOR_IGUAL",
          ">=": "MAYOR_IGUAL",
          "<>": "DESIGUALDAD",
        };
        const type = map[m[7]] || "ERROR";
        tokens.push({
          type,
          lexeme: m[7],
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }

      // + - * / = < >
      if (m[8]) {
        const map = {
          "+": "SUMA",
          "-": "RESTA",
          "*": "MULTIPLICACION",
          "/": "DIVISION",
          "=": "IGUAL",
          "<": "MENOR",
          ">": "MAYOR",
        };
        const type = map[m[8]] || "ERROR";
        tokens.push({
          type,
          lexeme: m[8],
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }

      // ; : , . ( ) [ ]
      if (m[9]) {
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
        const type = map[m[9]] || "ERROR";
        tokens.push({
          type,
          lexeme: m[9],
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }

      // identificadores / palabras reservadas / librerías
      if (m[10]) {
        const lexLower = m[10].toLowerCase();
        let type;
        if (palabrasreservadas.has(lexLower)) {
          type = "PALABRA_RESERVADAS";
        } else if (libreria.has(lexLower)) {
          type = "LIBRERIA";
        } else {
          type = "IDENTIFICADOR";
        }
        tokens.push({
          type,
          lexeme: m[10],
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
        });
        continue;
      }

      // números
      if (m[11]) {
        let lex = m[11];
        const esReal = /[.\eE]/.test(lex);
        if (esReal && /\d+\.(?!\.)$/.test(lex)) lex += "0"; // normaliza "3." -> "3.0"
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

      // error léxico
      if (m[12]) {
        const type = "ERROR";
        tokens.push({
          type,
          lexeme: m[12],
          line,
          column,
          code: TOKEN_CODES[type] ?? -1,
          msg: "Carácter no reconocido por el lenguaje.",
        });
        continue;
      }
    }
    return tokens;
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
        return `<tr>
        <td>${i + 1}</td>
        <td>${t.line}:${t.column}</td>
        <td>${t.type}</td>
        <td><pre style="margin:0;background:transparent;border:0;padding:0">${lex}</pre></td>
        <td>${t.code}</td>
        <td>${msg}</td>
      </tr>`;
      })
      .join("");
  }

  function renderErrores(errs) {
    const box = $("erroresBox");
    if (!box) return;
    if (!errs.length) {
      box.innerHTML = "";
      return;
    }
    box.innerHTML = errs
      .map(
        (e) =>
          `<div class="err">L${e.line}:C${e.column} — ${escapeHtml(
            e.msg
          )}</div>`
      )
      .join("");
  }

  $("btnAnalizar")?.addEventListener("click", () => {
    const codigo = $("codigo")?.value ?? "";

    const errs = prevalidar(codigo);
    renderErrores(errs);

    const tokens = tokenizar(codigo);
    if (errs.length) {
      for (const e of errs) {
        tokens.unshift({
          type: "ERROR",
          lexeme: "<prevalidación>",
          line: e.line,
          column: e.column,
          code: TOKEN_CODES.ERROR,
          msg: e.msg,
        });
      }
    }

    renderTokens(tokens);
  });

  $("btnCargarEjemplo")?.addEventListener("click", () => {
    const sample = `program Ejemplo;
uses SysUtils, Math, Forms; // librerías con mayúsculas (se deben reconocer)
var
  s: string;
  a, b: integer;
  r: real;
begin
  s := 'can''t';
  // comentario de línea
  { comentario en llaves }
  (* comentario de bloque multilinea *)
  if s <> '' then
    writeln(s);
  a := 3;      // entero
  r := 3.;     // real -> normaliza a 3.0
  r := 3.14e-2;// real científico
  b := 10;
  r := 1..10;  // rango
end.`;
    if ($("codigo")) $("codigo").value = sample;
    if ($("erroresBox")) $("erroresBox").innerHTML = "";
    if ($("tokensBody"))
      $("tokensBody").innerHTML =
        '<tr><td colspan="6">Presiona <strong>Analizar</strong> para generar tokens.</td></tr>';
  });

  $("btnLimpiar")?.addEventListener("click", () => {
    if ($("codigo")) $("codigo").value = "";
    if ($("erroresBox")) $("erroresBox").innerHTML = "";
    if ($("tokensBody"))
      $("tokensBody").innerHTML =
        '<tr><td colspan="6">Presiona <strong>Analizar</strong> para generar tokens.</td></tr>';
  });

  // Cargar archivo .pas
  $("btnCargasPas")?.addEventListener("click", () => $("fileInput")?.click());
  $("fileInput")?.addEventListener("change", (e) => {
    const f = e.target.files?.[0];
    if (!f) return;
    const reader = new FileReader();
    reader.onload = (ev) => {
      if ($("codigo")) $("codigo").value = ev.target.result;
      if ($("erroresBox")) $("erroresBox").innerHTML = "";
      if ($("tokensBody"))
        $("tokensBody").innerHTML =
          '<tr><td colspan="6">Presiona <strong>Analizar</strong> para generar tokens.</td></tr>';
    };
    reader.readAsText(f);
  });

  // Descargar .lex
  $("btnDescargar")?.addEventListener("click", () => {
    const codigo = $("codigo")?.value ?? "";
    const tokens = tokenizar(codigo);
    const errs = prevalidar(codigo);
    if (errs.length) {
      for (const e of errs) {
        tokens.unshift({
          type: "ERROR",
          lexeme: "<prevalidación>",
          line: e.line,
          column: e.column,
          code: TOKEN_CODES.ERROR,
          msg: e.msg,
        });
      }
    }
    if (!tokens.length) {
      alert("Primero analiza el código o asegúrate de que no esté vacío");
      return;
    }
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
});
