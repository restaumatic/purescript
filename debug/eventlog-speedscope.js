// Convert eventlog2html JSON to Chrome trace format for flamegraph visualization.
//
// Usage:
//   purs +RTS -l-agu -N1 -RTS compile $(spago sources)
//   eventlog2html --json purs.eventlog
//   node debug/eventlog-speedscope.js purs.eventlog.json > profile.json
//   # Open profile.json in https://www.speedscope.app/ or chrome://tracing
//
// The output contains two levels of nesting:
//   - Module-level spans (from traceMarkerIO in Make.hs): "ModuleName start/end"
//   - Declaration-level spans (from traceMarker in TypeChecker.hs): "tc ModuleName kind:name start/end"
//
// Use -N1 when profiling to get clean single-threaded nesting.

var fs = require("fs");

var inputFile = process.argv[2];
if (!inputFile) {
    console.error("Usage: node eventlog-speedscope.js <purs.eventlog.json> [--top N] [--cap CAP]");
    console.error("");
    console.error("Options:");
    console.error("  --top N    Also print top N slowest declarations to stderr (default: 50)");
    console.error("  --cap CAP  Filter to a specific GHC capability (thread). Default: all.");
    process.exit(1);
}

var topN = 50;
var filterCap = null;
for (var i = 3; i < process.argv.length; i++) {
    if (process.argv[i] === "--top" && process.argv[i+1]) {
        topN = parseInt(process.argv[i+1], 10);
        i++;
    } else if (process.argv[i] === "--cap" && process.argv[i+1]) {
        filterCap = parseInt(process.argv[i+1], 10);
        i++;
    }
}

var eventlog = JSON.parse(fs.readFileSync(inputFile, "utf-8"));

// Module-level: "ModuleName start" / "ModuleName end"
var moduleRe = /^([\w.]+) (start|end)$/;
// Declaration-level: "tc ModuleName kind:name start" / "tc ModuleName kind:name end"
var declRe = /^tc ([\w.]+) ([\w:+]+) (start|end)$/;
// Phase-level: "tc-phase ModuleName bindName phase start" / "... end"
var phaseRe = /^tc-phase ([\w.]+) ([\w+]+) (infer|solve) (start|end)$/;
// Entailment: "tc-entails ClassName start" / "tc-entails ClassName end"
var entailsRe = /^tc-entails ([\w.]+) (start|end)$/;

// Sort by timestamp
eventlog.traces.sort(function(a, b) { return a.tx - b.tx; });

// Filter by capability if requested
var traces = eventlog.traces;
if (filterCap !== null) {
    traces = traces.filter(function(t) { return t.cap === filterCap; });
}

// Build Chrome trace events and collect declaration timings
// Chrome trace format: https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU
var chromeEvents = [];
var declTimings = [];
var openSpans = {};  // key -> startTime (seconds)

for (var trace of traces) {
    var m = moduleRe.exec(trace.desc);
    var d = declRe.exec(trace.desc);
    var p = phaseRe.exec(trace.desc);
    var e = entailsRe.exec(trace.desc);
    var tid = trace.cap !== undefined ? trace.cap : 0;

    if (d) {
        var spanName = d[1] + " " + d[2];
        var ev = d[3];
        if (ev === "start") {
            openSpans[spanName] = trace.tx;
            chromeEvents.push({
                name: d[2],       // e.g. "val:updateExternalMenuView"
                cat: "typecheck",
                ph: "B",
                ts: trace.tx * 1e6,
                pid: 1, tid: tid
            });
        } else if (ev === "end") {
            chromeEvents.push({
                name: d[2],
                cat: "typecheck",
                ph: "E",
                ts: trace.tx * 1e6,
                pid: 1, tid: tid
            });
            if (openSpans[spanName] !== undefined) {
                declTimings.push({
                    module: d[1],
                    label: d[2],
                    duration: trace.tx - openSpans[spanName]
                });
                delete openSpans[spanName];
            }
        }
    } else if (p) {
        // Phase within typesOf: infer / solve
        chromeEvents.push({
            name: p[3],           // "infer" or "solve"
            cat: "phase",
            ph: p[4] === "start" ? "B" : "E",
            ts: trace.tx * 1e6,
            pid: 1, tid: tid
        });
    } else if (e) {
        // Per-constraint entailment
        chromeEvents.push({
            name: e[1],           // e.g. "Data.Show.Show"
            cat: "entails",
            ph: e[2] === "start" ? "B" : "E",
            ts: trace.tx * 1e6,
            pid: 1, tid: tid
        });
    } else if (m) {
        chromeEvents.push({
            name: m[1],           // e.g. "Restaumatic.PR.MenuV2.Import"
            cat: "module",
            ph: m[2] === "start" ? "B" : "E",
            ts: trace.tx * 1e6,
            pid: 1, tid: tid
        });
    }
}

if (chromeEvents.length === 0) {
    console.error("No matching start/end spans found in " + inputFile);
    console.error("Make sure you ran: purs +RTS -l-agu -N1 -RTS compile ...");
    process.exit(1);
}

// Output Chrome trace JSON to stdout
// Using the object format with metadata
var output = {
    traceEvents: chromeEvents,
    displayTimeUnit: "ms",
    metadata: {
        source: "purs eventlog"
    }
};
fs.writeFileSync("/dev/stdout", JSON.stringify(output));

// Print declaration timing report to stderr
if (topN > 0 && declTimings.length > 0) {
    declTimings.sort(function(a, b) { return b.duration - a.duration; });

    var totalDeclTime = declTimings.reduce(function(sum, d) { return sum + d.duration; }, 0);

    console.error("");
    console.error("=== Per-declaration typecheck timing (top " + Math.min(topN, declTimings.length) + " of " + declTimings.length + ") ===");
    console.error("");

    var maxModLen = 0, maxLabelLen = 0;
    var shown = declTimings.slice(0, topN);
    for (var d of shown) {
        if (d.module.length > maxModLen) maxModLen = d.module.length;
        if (d.label.length > maxLabelLen) maxLabelLen = d.label.length;
    }

    for (var d of shown) {
        var ms = (d.duration * 1000).toFixed(1);
        var pct = (d.duration / totalDeclTime * 100).toFixed(1);
        console.error(
            d.module.padEnd(maxModLen) + "  " +
            d.label.padEnd(maxLabelLen) + "  " +
            ms.padStart(8) + "ms  " +
            pct.padStart(5) + "%"
        );
    }

    var topTime = shown.reduce(function(sum, d) { return sum + d.duration; }, 0);
    console.error("");
    console.error("Total declaration typecheck time: " + (totalDeclTime * 1000).toFixed(0) + "ms across " + declTimings.length + " declarations");
    console.error("Top " + shown.length + " account for " + (topTime / totalDeclTime * 100).toFixed(1) + "% of declaration time");
}
