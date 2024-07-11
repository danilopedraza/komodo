let editor = ace.edit("editor");
editor.setTheme("ace/theme/dracula");
editor.session.setMode("ace/mode/javascript");

let results = ace.edit("results");
results.setTheme("ace/theme/dracula");
results.session.setMode("ace/mode/javascript");
results.setReadOnly(true);
results.setShowPrintMargin(false);
results.renderer.setShowGutter(false);
results.setHighlightActiveLine(false);
