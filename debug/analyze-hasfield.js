// Analyze HasField entailment performance from eventlog JSON
//
// Usage:
//   node debug/analyze-hasfield.js /path/to/purs.eventlog.json
//
// This script digs into the tc-entails events to understand:
// 1. Distribution of HasField resolution times
// 2. What sub-constraints are solved inside HasField
// 3. Time breakdown: instance matching vs sub-constraint solving
// 4. How much time is spent on newtypes vs plain Records
// 5. Whether there are repeated identical constraints

var fs = require("fs");

var inputFile = process.argv[2];
if (!inputFile) {
    console.error("Usage: node debug/analyze-hasfield.js <purs.eventlog.json>");
    process.exit(1);
}

var eventlog = JSON.parse(fs.readFileSync(inputFile, "utf-8"));
eventlog.traces.sort(function(a, b) { return a.tx - b.tx; });

// Parse all entailment events into a tree structure
var entailsStartRe = /^tc-entails ([\w.]+) (.+) start$/;
var entailsEndRe = /^tc-entails ([\w.]+) end$/;
var instanceRe = /^tc-entails-instance ([\w.]+) (.+)$/;

// Build a tree of entailment spans
var stack = [];
var rootSpans = [];

for (var trace of eventlog.traces) {
    var m;
    if ((m = entailsStartRe.exec(trace.desc))) {
        var span = {
            className: m[1],
            args: m[2].trim(),
            startTx: trace.tx,
            endTx: null,
            duration: null,
            instance: null,
            children: [],
            parent: stack.length > 0 ? stack[stack.length - 1] : null
        };
        if (span.parent) {
            span.parent.children.push(span);
        }
        stack.push(span);
    } else if ((m = entailsEndRe.exec(trace.desc))) {
        if (stack.length > 0 && stack[stack.length - 1].className === m[1]) {
            var span = stack.pop();
            span.endTx = trace.tx;
            span.duration = trace.tx - span.startTx;
            if (!span.parent) {
                rootSpans.push(span);
            }
        }
    } else if ((m = instanceRe.exec(trace.desc))) {
        if (stack.length > 0) {
            stack[stack.length - 1].instance = m[2];
        }
    }
}

// Now analyze HasField specifically
var hasFieldSpans = [];
function collectHasField(span) {
    if (span.className === "Data.Record.HasField") {
        hasFieldSpans.push(span);
    }
    // Don't recurse into children — we want top-level HasField invocations
}

for (var span of rootSpans) {
    collectHasField(span);
    // Also check direct children of non-HasField roots
}

// Actually, let's collect ALL HasField spans, noting depth
function collectAll(span, depth) {
    if (span.className === "Data.Record.HasField") {
        hasFieldSpans.push({ span: span, depth: depth });
    }
    for (var child of span.children) {
        collectAll(child, depth + 1);
    }
}

hasFieldSpans = [];
for (var span of rootSpans) {
    collectAll(span, 0);
}

console.error("=== HasField Analysis ===\n");
console.error("Total HasField entailment spans: " + hasFieldSpans.length);

// 1. Time distribution
var durations = hasFieldSpans.map(function(h) { return h.span.duration * 1000; }); // ms
durations.sort(function(a, b) { return a - b; });

var totalMs = durations.reduce(function(s, d) { return s + d; }, 0);
console.error("Total HasField time: " + totalMs.toFixed(0) + "ms (" + (totalMs/1000).toFixed(1) + "s)");
console.error("Mean: " + (totalMs / durations.length).toFixed(3) + "ms");
console.error("Median: " + durations[Math.floor(durations.length / 2)].toFixed(3) + "ms");
console.error("P90: " + durations[Math.floor(durations.length * 0.9)].toFixed(3) + "ms");
console.error("P99: " + durations[Math.floor(durations.length * 0.99)].toFixed(3) + "ms");
console.error("Max: " + durations[durations.length - 1].toFixed(3) + "ms");
console.error("Min: " + durations[0].toFixed(6) + "ms");

// Distribution buckets
var buckets = [0.01, 0.05, 0.1, 0.5, 1, 2, 5, 10, 50, 100, Infinity];
var bucketCounts = buckets.map(function() { return { count: 0, totalMs: 0 }; });
for (var d of durations) {
    for (var i = 0; i < buckets.length; i++) {
        if (d < buckets[i]) {
            bucketCounts[i].count++;
            bucketCounts[i].totalMs += d;
            break;
        }
    }
}
console.error("\nTime distribution:");
var prevBound = 0;
for (var i = 0; i < buckets.length; i++) {
    var label = prevBound + "-" + (buckets[i] === Infinity ? "∞" : buckets[i]) + "ms";
    console.error("  " + label.padEnd(15) + " " +
        String(bucketCounts[i].count).padStart(7) + " spans  " +
        bucketCounts[i].totalMs.toFixed(0).padStart(7) + "ms total  " +
        (bucketCounts[i].totalMs / totalMs * 100).toFixed(1).padStart(5) + "% of time");
    prevBound = buckets[i];
}

// 2. Depth analysis (how many are top-level vs sub-constraints)
var depthCounts = {};
for (var h of hasFieldSpans) {
    depthCounts[h.depth] = (depthCounts[h.depth] || 0) + 1;
}
console.error("\nDepth distribution (0 = top-level entailment call):");
for (var depth in depthCounts) {
    console.error("  depth " + depth + ": " + depthCounts[depth] + " spans");
}

// 3. Instance analysis — what instances are resolved?
var instanceCounts = {};
for (var h of hasFieldSpans) {
    var inst = h.span.instance || "(unknown)";
    if (!instanceCounts[inst]) instanceCounts[inst] = { count: 0, totalMs: 0 };
    instanceCounts[inst].count++;
    instanceCounts[inst].totalMs += h.span.duration * 1000;
}
var instEntries = Object.entries(instanceCounts).sort(function(a, b) { return b[1].totalMs - a[1].totalMs; });
console.error("\nInstance resolution (top 20):");
for (var entry of instEntries.slice(0, 20)) {
    console.error("  " + entry[0].padEnd(60) + " " +
        String(entry[1].count).padStart(6) + "x  " +
        entry[1].totalMs.toFixed(0).padStart(7) + "ms  " +
        (entry[1].totalMs / totalMs * 100).toFixed(1).padStart(5) + "%");
}

// 4. Sub-constraint analysis — what's inside each HasField?
var childClassCounts = {};
for (var h of hasFieldSpans) {
    for (var child of h.span.children) {
        var key = child.className;
        if (!childClassCounts[key]) childClassCounts[key] = { count: 0, totalMs: 0 };
        childClassCounts[key].count++;
        childClassCounts[key].totalMs += child.duration * 1000;
    }
}
console.error("\nSub-constraints inside HasField (immediate children):");
var childEntries = Object.entries(childClassCounts).sort(function(a, b) { return b[1].totalMs - a[1].totalMs; });
for (var entry of childEntries) {
    console.error("  " + entry[0].padEnd(50) + " " +
        String(entry[1].count).padStart(6) + "x  " +
        entry[1].totalMs.toFixed(0).padStart(7) + "ms  " +
        (entry[1].totalMs / totalMs * 100).toFixed(1).padStart(5) + "%");
}

// 5. "Self time" — HasField time NOT spent in child constraints
var selfTimes = hasFieldSpans.map(function(h) {
    var childTime = h.span.children.reduce(function(s, c) { return s + c.duration; }, 0);
    return (h.span.duration - childTime) * 1000; // ms
});
var totalSelfMs = selfTimes.reduce(function(s, d) { return s + d; }, 0);
console.error("\nSelf time (HasField overhead excluding sub-constraints):");
console.error("  Total self time: " + totalSelfMs.toFixed(0) + "ms (" + (totalSelfMs / totalMs * 100).toFixed(1) + "% of HasField time)");
console.error("  Mean self time:  " + (totalSelfMs / selfTimes.length).toFixed(3) + "ms");

// 6. Args analysis — what types are being accessed?
var argPatterns = {};
for (var h of hasFieldSpans) {
    var args = h.span.args;
    if (!argPatterns[args]) argPatterns[args] = { count: 0, totalMs: 0 };
    argPatterns[args].count++;
    argPatterns[args].totalMs += h.span.duration * 1000;
}
var argEntries = Object.entries(argPatterns).sort(function(a, b) { return b[1].totalMs - a[1].totalMs; });
console.error("\nTop 30 HasField argument patterns (by total time):");
for (var entry of argEntries.slice(0, 30)) {
    console.error("  " + entry[0].substring(0, 80).padEnd(80) + " " +
        String(entry[1].count).padStart(5) + "x  " +
        entry[1].totalMs.toFixed(0).padStart(6) + "ms  " +
        (entry[1].totalMs / totalMs * 100).toFixed(1).padStart(5) + "%");
}

// 7. Repeated constraints — how many unique vs repeated?
var uniqueArgs = Object.keys(argPatterns).length;
var totalArgs = hasFieldSpans.length;
var repeatedEntries = argEntries.filter(function(e) { return e[1].count > 1; });
var repeatedTotal = repeatedEntries.reduce(function(s, e) { return s + e[1].count; }, 0);
var repeatedTimeMs = repeatedEntries.reduce(function(s, e) { return s + e[1].totalMs; }, 0);
console.error("\nRepetition analysis:");
console.error("  Unique constraint patterns: " + uniqueArgs);
console.error("  Total resolutions: " + totalArgs);
console.error("  Repeated (>1x): " + repeatedEntries.length + " patterns, " + repeatedTotal + " resolutions, " + repeatedTimeMs.toFixed(0) + "ms");
console.error("  Savings if cached: ~" + (repeatedTimeMs - repeatedEntries.length * (totalSelfMs / totalArgs)).toFixed(0) + "ms");

// 8. Number of children per HasField span
var childCountDist = {};
for (var h of hasFieldSpans) {
    var nc = h.span.children.length;
    childCountDist[nc] = (childCountDist[nc] || 0) + 1;
}
console.error("\nChildren-per-HasField distribution:");
for (var nc of Object.keys(childCountDist).sort(function(a, b) { return a - b; })) {
    console.error("  " + nc + " children: " + childCountDist[nc] + " spans");
}

// 9. Record type analysis — what record types are being accessed?
// The third arg in HasField is the record type
// Pattern: "'fieldName' TypeCtor" or "'fieldName' TypeCtor {..}"
var recordTypes = {};
for (var h of hasFieldSpans) {
    var parts = h.span.args.split(" ");
    // First part is the field name (quoted), rest is the type
    var recordType = parts.length > 1 ? parts.slice(1).join(" ") : "(unknown)";
    if (!recordTypes[recordType]) recordTypes[recordType] = { count: 0, totalMs: 0 };
    recordTypes[recordType].count++;
    recordTypes[recordType].totalMs += h.span.duration * 1000;
}
var recordEntries = Object.entries(recordTypes).sort(function(a, b) { return b[1].totalMs - a[1].totalMs; });
console.error("\nTop 30 record types being accessed (by total time):");
for (var entry of recordEntries.slice(0, 30)) {
    console.error("  " + entry[0].substring(0, 60).padEnd(60) + " " +
        String(entry[1].count).padStart(5) + "x  " +
        entry[1].totalMs.toFixed(0).padStart(6) + "ms  " +
        (entry[1].totalMs / totalMs * 100).toFixed(1).padStart(5) + "%");
}
