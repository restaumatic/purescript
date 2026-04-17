// Debug compilation times of modules from eventlog profiling
//
// Build with stack:
//     stack build
// Run a command like this to generate purs.eventlog:
//     purs +RTS -l-agu -N1 -RTS compile $(spago sources)
// (Use -N1 for accurate per-declaration timings.)
// Process it with
//     eventlog2html --json purs.eventlog
//     node eventlog.js purs.eventlog.json
//
// This shows per-module timing and concurrency stats.
// For per-declaration flamegraphs, see eventlog-speedscope.js.
//
// See the GHC docs for descriptions of the RTS flags:
//   - https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#rts-options-for-heap-profiling
//   - https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#rts-eventlog
//   - https://downloads.haskell.org/ghc/latest/docs/users_guide/using-concurrent.html?highlight=threaded#rts-options-for-smp-parallelism
var mainFile = process.argv[2];
if (!mainFile) throw new Error("Provide a file name");

var name_length = 0;

function summarizeEventlog(filename) {
    var eventlog = JSON.parse(require("fs").readFileSync(filename, "utf-8"));
    // eventlog.heap
    //   c: Set(3) { 'Heap Size', 'Live Bytes', 'Blocks Size' }
    // eventlog.samples
    // eventlog.traces

    var traces = {};
    var minTx = Infinity;
    var maxTx = -Infinity;
    var maxMem = -Infinity;
    var total = 0;
    var con = [];
    var max_cons = [[]];
    var cursor = 0;

    // I guess some buffering makes it out of order?
    eventlog.traces.sort(({tx: tx1}, {tx: tx2}) => tx1 - tx2);

    for (let trace of eventlog.traces) {
        var m = /^([\w.]+) (start|end)$/.exec(trace.desc);
        if (!m) continue;
        var name = m[1];
        if (!(name in traces)) traces[name] = {};
        if (name.length > name_length) name_length = name.length;
        var ev = m[2];

        if (traces[name][ev]) {
            if (traces[name].time === 0) {
                console.log("Warn: start after end", name, traces[name].start, trace.tx);
            } else {
                console.log("Warn: duplicate event", trace.desc);
            }
            continue;
        }

        var time = trace.tx;
        if (time < minTx) minTx = time;
        if (time > maxTx) maxTx = time;

        while (cursor < eventlog.heap.length && eventlog.heap[cursor].x < trace.tx) {
            cursor++;
            if (eventlog.heap[cursor].c !== 'Heap Size') {
                cursor = eventlog.heap.length;
            }
        }
        if (ev === "start") {
            traces[name].cursor = cursor;
        }

        traces[name][ev] = time;
        if (ev === "end" && !("start" in traces[name])) {
            console.log("Warn: missing start for", name);
            traces[name].start = time;
            traces[name].time = 0;
            continue;
        }
        if ("start" in traces[name] && "end" in traces[name]) {
            traces[name].time = traces[name].end - traces[name].start;
            var mems = eventlog.heap.slice(traces[name].cursor, cursor).map(e => e.y);
            var mem_min = Math.min(...mems);
            var mem_max = Math.max(...mems);
            var maxMem = Math.max(maxMem, mem_max);
            Object.assign(traces[name], {mem_min,mem_max});
            total += traces[name].time;
        }

        if (ev === "start") con = con.concat([name]);
        if (ev === "end") {
            var l = con.length;
            con = con.filter(n => n !== name);
            if (con.length !== l - 1) {
                console.log(con, name);
            }
        }
        if (con.length >= max_cons[0].length) {
            if (con.length > max_cons[0].length)
                max_cons = [];
            max_cons.push(con);
        }
    }

    var timespan = maxTx - minTx;

    return { traces, total, minTx, maxTx, timespan, max_cons, maxMem };
}

var mainFiles = process.argv.slice(2);

if (mainFiles.length > 1) {
    for (let file of mainFiles) {
        console.log(file);
        var { traces, total, timespan, max_cons, maxMem } = summarizeEventlog(file);
        if (timespan === -Infinity && total === 0 && max_cons[0].length === 0) continue;
        var max_con_time = 0;
        var concurrencies = max_cons.map(max_con => {
            if (max_con.length !== max_cons[0].length)
                throw new Error("max_con length error");
            var modules = max_con.map(name => [name, traces[name]]);
            var start = Math.max(...modules.map(([name, {start}]) => start));
            var end = Math.min(...modules.map(([name, {end}]) => end));
            var time = end - start;
            max_con_time += time;
            return {
                modules,
                start,
                end,
                time,
            };
        });
        console.log("timespan                   ", timespan);
        console.log("ratio (avg concurrency?)   ", total/timespan);
        console.log("max concurrency            ", max_cons[0].length);
        console.log("time at max concurrency (%)", 100*max_con_time/timespan);
        console.log("peak heap size             ", space(maxMem));
    }
    process.exit(0);
}

var { traces, total, timespan, max_cons } = summarizeEventlog(mainFile);

var timings = [];
for (let name in traces) {
    let trace = traces[name];
    if (!("time" in trace)) {
        console.log("Warn: missing timing for", name, trace);
    } else if (trace.time > 0) {
        timings.push([name, trace.time]);
    }
}

timings.sort(([n1,t1,_1,m1], [n2,t2,_2,m2]) => t1 - t2);

timings.push(["stats", "-----"]);
timings.push(["total", total]);
timings.push(["timespan", timespan]);
timings.push(["ratio (avg concurrency?)", total/timespan]);
var max_con_time = 0;
var concurrencies = max_cons.map(max_con => {
    if (max_con.length !== max_cons[0].length)
        throw new Error("max_con length error");
    var modules = max_con.map(name => [name, traces[name]]);
    var start = Math.max(...modules.map(([name, {start}]) => start));
    var end = Math.min(...modules.map(([name, {end}]) => end));
    var time = end - start;
    max_con_time += time;
    return {
        modules,
        start,
        end,
        time,
    };
});
timings.push(["max concurrency", max_cons[0].length]);
timings.push(["time at max concurrency (s)", max_con_time]);
timings.push(["time at max concurrency (%)", 100*max_con_time/timespan]);

for (let [name, time] of timings) {
    // console.log(name.padEnd(name_length, " "), (""+time).substring(0, 5).padStart(5, " "));
    console.log(name.padEnd(name_length, " "), time);
}

// Per-declaration breakdown (from "tc Module kind:name start/end" markers)
var declRe = /^tc ([\w.]+) ([\w:+]+) (start|end)$/;
// Phase markers: "tc-phase Module bindName infer|solve start|end"
var phaseRe = /^tc-phase ([\w.]+) ([\w+]+) (infer|solve) (start|end)$/;
var declTraces = {};
var phaseTraces = {};
var rawEventlog = JSON.parse(require("fs").readFileSync(mainFile, "utf-8"));
for (let trace of rawEventlog.traces) {
    var d = declRe.exec(trace.desc);
    if (d) {
        var key = d[1] + " " + d[2];
        if (!(key in declTraces)) declTraces[key] = { module: d[1], label: d[2] };
        declTraces[key][d[3]] = trace.tx;
        continue;
    }
    var p = phaseRe.exec(trace.desc);
    if (p) {
        var pkey = p[1] + " " + p[2] + " " + p[3];
        if (!(pkey in phaseTraces)) phaseTraces[pkey] = { module: p[1], bind: p[2], phase: p[3] };
        phaseTraces[pkey][p[4]] = trace.tx;
    }
}

// Build phase timing lookup: "Module bind" -> { infer: ms, solve: ms }
var phaseLookup = {};
for (let key in phaseTraces) {
    let pt = phaseTraces[key];
    if ("start" in pt && "end" in pt) {
        var lkey = pt.module + " " + pt.bind;
        if (!(lkey in phaseLookup)) phaseLookup[lkey] = {};
        phaseLookup[lkey][pt.phase] = (pt.end - pt.start) * 1000;
    }
}

var declTimings = [];
for (let key in declTraces) {
    let dt = declTraces[key];
    if ("start" in dt && "end" in dt) {
        // Try to find matching phase data
        var bindName = dt.label.replace(/^(val|bind):/, "");
        var phases = phaseLookup[dt.module + " " + bindName] || {};
        declTimings.push([dt.module, dt.label, dt.end - dt.start, phases]);
    }
}

if (declTimings.length > 0) {
    declTimings.sort(([,,t1], [,,t2]) => t2 - t1);
    var totalDeclTime = declTimings.reduce((s, [,,t]) => s + t, 0);
    var maxModLen = Math.max(...declTimings.slice(0, 50).map(([m]) => m.length));
    var maxLabelLen = Math.max(...declTimings.slice(0, 50).map(([,l]) => l.length));

    console.log("");
    console.log("=== Per-declaration typecheck timing (top " + Math.min(50, declTimings.length) + " of " + declTimings.length + ") ===");
    console.log("");
    for (let [mod, label, time, phases] of declTimings.slice(0, 50)) {
        var ms = (time * 1000).toFixed(1);
        var pct = (time / totalDeclTime * 100).toFixed(1);
        var phaseStr = "";
        if (phases.infer !== undefined || phases.solve !== undefined) {
            var inferMs = (phases.infer || 0).toFixed(0);
            var solveMs = (phases.solve || 0).toFixed(0);
            phaseStr = "  [infer:" + inferMs + " solve:" + solveMs + "]";
        }
        console.log(
            mod.padEnd(maxModLen) + "  " +
            label.padEnd(maxLabelLen) + "  " +
            ms.padStart(8) + "ms  " +
            pct.padStart(5) + "%" +
            phaseStr
        );
    }
    var topTime = declTimings.slice(0, 50).reduce((s, [,,t]) => s + t, 0);
    console.log("");
    console.log("Total declaration time: " + (totalDeclTime * 1000).toFixed(0) + "ms across " + declTimings.length + " declarations");
    console.log("Top 50 account for " + (topTime / totalDeclTime * 100).toFixed(1) + "%");
}


function space(v) {
    if (!isFinite(v)) return "----";
    if (v === Infinity) return "+Inf";
    if (v === -Infinity) return "-Inf";
    if (v !== v) return " NaN";
    var sizes = [
        [1_000_000_000, "G"],
        [1_000_000,     "M"],
        [1_000,         "K"],
        [0,             ""],
    ]
    for (let [value, suffix] of sizes) {
        if (v < value) continue;
        if (!suffix) return (""+v).padStart(4, " ");
        var adj = v/value;
        var str = ""+adj;
        if (adj >= 100) return str.substring(0,3)+suffix;
        if (adj >= 10) return " "+str.substring(0,2)+suffix;
        return str.substring(0,3)+suffix;
    }
}
function signed(fmt, v) {
    if (!isFinite(v)) return " "+fmt(v);
    if (v < 0) return "-"+fmt(-v);
    return "+"+fmt(v);
}
