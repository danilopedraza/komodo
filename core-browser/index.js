const editor = ace.edit("editor");
editor.setOptions({
  theme: "ace/theme/dracula",
  mode: "ace/mode/javascript",
  tabSize: 4,
  useSoftTabs: true,
});

const results = ace.edit("results");
results.setOptions({
  theme: "ace/theme/dracula",
  readOnly: true,
  showPrintMargin: false,
  showGutter: false,
  highlightActiveLine: false,
});
results.renderer.$cursorLayer.element.style.display = "none";

const inputBox = ace.edit("input");
inputBox.setOptions({
  theme: "ace/theme/dracula",
  showPrintMargin: false,
  placeholder: "Enter your standard input here",
});

import init, { run_code } from "./komodo/komodo_browser.js"
await init();

let run = () => {
  const stdin = inputBox.getValue();
  const source = editor.getValue();
  let res = run_code(source, stdin);
  results.setValue(res);
  results.clearSelection();
};

document.addEventListener("keydown", (e) => {
  if (e.key === "Enter" && e.ctrlKey) run();
});

document
  .getElementById("exec-button")
  .addEventListener("click", run);
