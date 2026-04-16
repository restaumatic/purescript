#!/usr/bin/env node
// Analyze HasField entailment time by label, type, and module
const fs = require("fs");
const data = JSON.parse(fs.readFileSync(process.argv[2] || "purs.eventlog.json", "utf8"));
const traces = data.traces;

let currentModule = null;
let moduleStack = [];
let hasFieldStack = [];

let byModule = {};
let byLabel = {};
let byType = {};       // second arg (the field type)
let byModuleLabel = {};

for (const t of traces) {
  if (!t.desc) continue;

  // Module markers: "ModuleName start" / "ModuleName end"
  let m = /^([\w.]+) (start|end)$/.exec(t.desc);
  if (m) {
    if (m[2] === "start") {
      moduleStack.push(m[1]);
      currentModule = m[1];
    } else {
      moduleStack.pop();
      currentModule = moduleStack.length > 0 ? moduleStack[moduleStack.length - 1] : null;
    }
    continue;
  }

  // HasField start: "tc-entails Data.Record.HasField <args> start"
  m = /^tc-entails Data\.Record\.HasField (.+) start$/.exec(t.desc);
  if (m) {
    hasFieldStack.push({ module: currentModule, args: m[1], startTx: t.tx });
    continue;
  }

  // HasField end
  if (t.desc === "tc-entails Data.Record.HasField end") {
    if (hasFieldStack.length > 0) {
      const entry = hasFieldStack.pop();
      const dur = t.tx - entry.startTx;
      const mod = entry.module || "unknown";
      const args = entry.args;

      // Parse: 'labelName' TypeName RowKind
      // or: _ _ Record  (unknown)
      const parts = args.split(/\s+/);
      let label = "_";
      let fieldType = "_";
      if (parts[0].startsWith("'")) {
        // label is 'xxx' — may contain spaces if it's a multi-word symbol, but usually single
        label = parts[0].replace(/'/g, "");
        fieldType = parts.length > 1 ? parts[1] : "_";
      }

      if (!byModule[mod]) byModule[mod] = { time: 0, count: 0 };
      byModule[mod].time += dur;
      byModule[mod].count++;

      if (!byLabel[label]) byLabel[label] = { time: 0, count: 0 };
      byLabel[label].time += dur;
      byLabel[label].count++;

      if (!byType[fieldType]) byType[fieldType] = { time: 0, count: 0 };
      byType[fieldType].time += dur;
      byType[fieldType].count++;

      const key = mod + " | " + label;
      if (!byModuleLabel[key]) byModuleLabel[key] = { time: 0, count: 0, module: mod, label };
      byModuleLabel[key].time += dur;
      byModuleLabel[key].count++;
    }
  }
}

const totalTime = Object.values(byLabel).reduce((s, v) => s + v.time, 0);

console.log("=== HasField inclusive time by LABEL (top 40) ===\n");
const sortedLabels = Object.entries(byLabel).sort((a, b) => b[1].time - a[1].time);
for (const [label, val] of sortedLabels.slice(0, 40)) {
  const pct = (val.time / totalTime * 100).toFixed(1);
  console.log(
    ("'" + label + "'").padEnd(45),
    (val.time * 1000).toFixed(0).toString().padStart(8) + "ms",
    pct.padStart(6) + "%",
    val.count.toString().padStart(7) + " calls"
  );
}

console.log("\n=== HasField inclusive time by FIELD TYPE (top 20) ===\n");
const sortedTypes = Object.entries(byType).sort((a, b) => b[1].time - a[1].time);
for (const [type, val] of sortedTypes.slice(0, 20)) {
  const pct = (val.time / totalTime * 100).toFixed(1);
  console.log(
    type.padEnd(45),
    (val.time * 1000).toFixed(0).toString().padStart(8) + "ms",
    pct.padStart(6) + "%",
    val.count.toString().padStart(7) + " calls"
  );
}

console.log("\n=== HasField inclusive time by MODULE (top 20) ===\n");
const sortedModules = Object.entries(byModule).sort((a, b) => b[1].time - a[1].time);
for (const [mod, val] of sortedModules.slice(0, 20)) {
  const pct = (val.time / totalTime * 100).toFixed(1);
  console.log(
    mod.padEnd(55),
    (val.time * 1000).toFixed(0).toString().padStart(8) + "ms",
    pct.padStart(6) + "%",
    val.count.toString().padStart(7) + " calls"
  );
}

// TranslationKey analysis
console.log("\n=== TranslationKey analysis ===\n");
let tkTime = 0, tkCount = 0;
for (const [type, val] of Object.entries(byType)) {
  if (/TranslationKey/i.test(type)) {
    tkTime += val.time;
    tkCount += val.count;
    console.log("Type:", type, (val.time * 1000).toFixed(0) + "ms", val.count, "calls");
  }
}
console.log("\nTotal TranslationKey HasField time:", (tkTime * 1000).toFixed(0) + "ms",
  "(" + (tkTime / totalTime * 100).toFixed(1) + "% of HasField)");
console.log("Total TranslationKey HasField calls:", tkCount);

// Also check labels that look like translation keys
console.log("\n=== Labels matching translation-like patterns ===\n");
let tlLabelTime = 0, tlLabelCount = 0;
for (const [label, val] of sortedLabels) {
  // Translation keys are often camelCase identifiers
  // Let's just look for the top labels in TranslationKey-typed calls
}

// Top module+label combos
console.log("\n=== Top module+label combos (top 30) ===\n");
const sortedML = Object.entries(byModuleLabel).sort((a, b) => b[1].time - a[1].time);
for (const [key, val] of sortedML.slice(0, 30)) {
  console.log(
    key.padEnd(75),
    (val.time * 1000).toFixed(0).toString().padStart(8) + "ms",
    val.count.toString().padStart(7) + " calls"
  );
}

console.log("\n--- Total ---");
console.log("Total HasField time (inclusive):", (totalTime * 1000).toFixed(0) + "ms");
console.log("Total HasField calls:", Object.values(byLabel).reduce((s, v) => s + v.count, 0));
