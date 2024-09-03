import init, { run_code } from "./komodo/komodo_browser.js"

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

const stateMessage = document.getElementById("state-message");

let setStateMessage = ({ msg, color }) => {
  stateMessage.style.color = color;
  stateMessage.innerText = msg;
};

function getErrorDiagnostic(msg) {
  switch (msg) {
    case "unreachable executed": return "Your program reached an unimplemented part of the interpreter";
    default: return "It probably ran out of memory";
  }
}

let run = () => {
  const stdin = inputBox.getValue();
  const source = editor.getValue();
  try {
    setStateMessage({
      msg: "The interpreter is executing your program...",
      color: "gray",
    });
    let res = run_code(source, stdin);
    results.setValue(res);
    results.clearSelection();
    setStateMessage({
      msg: "The interpreter finished its execution.",
      color: "gray",
    });
  } catch (e) {
    setStateMessage({
      msg: `The interpreter failed to execute your program. ${getErrorDiagnostic(e.message)}.`,
      color: "red",
    });
    results.setValue("");
    results.clearSelection();
  } finally {
    init();
  }
};

document.addEventListener("keydown", (e) => {
  if (e.key === "Enter" && e.ctrlKey) run();
});

document
  .getElementById("exec-button")
  .addEventListener("click", run);

init().then(() =>
  setStateMessage({
    msg: "Execute your code with the \"Run\" button or Ctrl + Enter.",
    color: "gray",
  })
);
