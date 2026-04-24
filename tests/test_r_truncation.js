/**
 * R-file truncation lint.
 *
 * This session hit three R files that were silently truncated mid-file by the
 * editing sandbox (utils.R, name_normalization.R, accreditation_scrapers.R).
 * Each one produced downstream breakage that took real debugging to trace
 * back to the truncation. The canonical fingerprint was always the same:
 *   - no trailing newline at EOF, AND/OR
 *   - unbalanced `{`, `(`, or `[`, AND/OR
 *   - EOF inside a string literal, AND/OR
 *   - last non-whitespace character is not a legal expression terminator
 *
 * This lint is a 5-second CI gate that would have caught all three. It
 * runs over every .R file under scripts/ and tests/ and fails on any of
 * the above conditions.
 *
 * Implementation notes:
 *   - The tokenizer is deliberately minimal — we only need to know when we
 *     are inside a string, a comment, or a backtick-quoted identifier so
 *     that braces inside those are not counted. R-specific constructs not
 *     relevant to bracket balance (operators, literals, function defs) are
 *     treated as code.
 *   - Acceptable terminators are `}` (end of block), `)` (end of call),
 *     `]` / `]]` (end of subset), or any R identifier character
 *     `[A-Za-z0-9_.]`. An identifier-terminated file is valid because R
 *     scripts often end with a final call like `main()` that returns a
 *     value. Operator endings like `,`, `+`, `|>`, `<-` are always
 *     truncations.
 */

const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..');
const SEARCH_DIRS = ['scripts', 'tests'];

function walk(dir) {
  const results = [];
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) results.push(...walk(full));
    else if (entry.isFile() && full.endsWith('.R')) results.push(full);
  }
  return results;
}

/**
 * Scan R source and return a diagnostics object. Pure function of input text;
 * no I/O so the same logic is trivially unit-testable.
 */
function scanRSource(src) {
  const issues = [];
  if (src.length === 0) {
    issues.push('file is empty');
    return { issues };
  }

  // 1) Trailing newline.
  if (!src.endsWith('\n')) {
    issues.push('missing trailing newline at EOF');
  }

  // 2) Bracket balance + terminal state.
  let state = 'code'; // code | comment | str_d | str_s | backtick
  let curly = 0;
  let paren = 0;
  let square = 0;
  let lastNonWsChar = '';
  let lastNonWsLine = 0;
  let line = 1;

  for (let i = 0; i < src.length; i += 1) {
    const c = src[i];
    if (c === '\n') line += 1;
    if (c !== ' ' && c !== '\t' && c !== '\n' && c !== '\r') {
      lastNonWsChar = c;
      lastNonWsLine = line;
    }

    if (state === 'code') {
      if (c === '#') state = 'comment';
      else if (c === '"') state = 'str_d';
      else if (c === "'") state = 'str_s';
      else if (c === '`') state = 'backtick';
      else if (c === '{') curly += 1;
      else if (c === '}') curly -= 1;
      else if (c === '(') paren += 1;
      else if (c === ')') paren -= 1;
      else if (c === '[') square += 1;
      else if (c === ']') square -= 1;
    } else if (state === 'comment') {
      if (c === '\n') state = 'code';
    } else if (state === 'str_d') {
      if (c === '\\' && i + 1 < src.length) {
        i += 1; // skip escaped next char
      } else if (c === '"') {
        state = 'code';
      }
    } else if (state === 'str_s') {
      if (c === '\\' && i + 1 < src.length) {
        i += 1;
      } else if (c === "'") {
        state = 'code';
      }
    } else if (state === 'backtick') {
      if (c === '`') state = 'code';
    }
  }

  if (state === 'str_d' || state === 'str_s') {
    issues.push('EOF inside unterminated string literal');
  } else if (state === 'backtick') {
    issues.push('EOF inside unterminated backtick identifier');
  }
  if (curly !== 0) issues.push(`brace balance is ${curly} (expected 0)`);
  if (paren !== 0) issues.push(`paren balance is ${paren} (expected 0)`);
  if (square !== 0) issues.push(`bracket balance is ${square} (expected 0)`);

  // 3) Terminal character must be a valid expression terminator. An R file
  //    typically ends with `}` (block close), `)` (call close), `]` (subset),
  //    or an identifier character `[A-Za-z0-9_.]`. Anything else — a bare
  //    comma, a trailing pipe `|`, an operator — means the last expression
  //    was cut off mid-token.
  if (lastNonWsChar && !/^[})\]A-Za-z0-9_.]$/.test(lastNonWsChar)) {
    issues.push(
      `last non-whitespace char is "${lastNonWsChar}" at line ${lastNonWsLine} ` +
      '(expected `}`, `)`, `]`, or identifier char)'
    );
  }

  return { issues };
}

function main() {
  let files = [];
  for (const dir of SEARCH_DIRS) {
    const absDir = path.join(ROOT, dir);
    if (fs.existsSync(absDir)) files = files.concat(walk(absDir));
  }
  files.sort();

  const failures = [];
  for (const file of files) {
    const src = fs.readFileSync(file, 'utf8');
    const { issues } = scanRSource(src);
    if (issues.length > 0) {
      failures.push({ file: path.relative(ROOT, file), issues });
    }
  }

  if (failures.length === 0) {
    console.log(`test_r_truncation: OK (${files.length} R files scanned)`);
    process.exit(0);
  }

  console.error(`test_r_truncation: ${failures.length} file(s) look truncated:`);
  for (const { file, issues } of failures) {
    for (const issue of issues) {
      console.error(`  ${file}: ${issue}`);
    }
  }
  process.exit(1);
}

// Allow unit-test-style import via `node -e "require('./tests/test_r_truncation').scanRSource(...)"`.
if (require.main === module) {
  main();
} else {
  module.exports = { scanRSource };
}
