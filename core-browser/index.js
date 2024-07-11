let editor = ace.edit("editor");
editor.setTheme("ace/theme/dracula");
editor.session.setMode("ace/mode/javascript");

let results = ace.edit("results");
results.setTheme("ace/theme/dracula");
results.setReadOnly(true);
results.setShowPrintMargin(false);
results.renderer.setShowGutter(false);
results.setHighlightActiveLine(false);
results.renderer.$cursorLayer.element.style.display = "none"

import init, { run_code } from "./smtc/symstatic_browser.js"
await init();
console.log(run_code("println(\"foo\"", ""));
