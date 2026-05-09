#!/usr/bin/env python3
"""
Aggregate compare/eq callers from a GHC .prof file's call tree.

Usage: analyze-prof.py <path-to-prof> [--levels N]

For each target SCC (compare Qualified, etc.), walk the call tree and
bin inherited %time by the *Nth* non-target ancestor (--levels controls
how far up). Default --levels=2 — distinguishes which user code is
calling the matcher SCCs.
"""

import re
import sys
import argparse
from collections import defaultdict

TARGETS = {
    'compare-Qualified': re.compile(r'\bNames\.hs:233:23-25\b'),
    'compare-ModuleName': re.compile(r'\bNames\.hs:192:23-25\b'),
    'compare-ProperName': re.compile(r'\bNames\.hs:160:23-25\b'),
    'compare-QualifiedBy': re.compile(r'\bNames\.hs:209:23-25\b'),
    'eq-PSString': re.compile(r'\bPSString\.hs:52:13-14\b'),
    'compare-Type': re.compile(r'\bTypes\.hs:962:3-9\b'),
    'compare-Constraint': re.compile(r'\bTypes\.hs:1045:1-9\b'),
}

# Skip these as parents — they're transitive name-compare nodes and the
# AST pattern-synonym matchers themselves.
SKIP_AS_PARENT_SCC = re.compile(
    r'^(compare\b|==\b|>=\b|<=\b|<\b|>\b|/=\b|'
    r'compareConstraint\b|eqConstraint\b|compareType\b|eqType\b)$'
)
# Pattern-synonym matchers — NOT the user-callable code, walk through them too
SKIP_AS_PARENT_MATCHER = re.compile(
    r'^\$m(TypeConstructor|TypeApp|KindApp|RCons|TypeOp|ConstrainedType|'
    r'TUnknown|TypeVar|TypeLevelString|TypeLevelInt|TypeWildcard|ForAll|'
    r'Skolem|REmpty|KindedType|BinaryNoParensType|ParensInType)\.'
)


def parse_line(line):
    if not line.strip() or line.startswith('COST CENTRE'):
        return None
    indent = len(line) - len(line.lstrip(' '))
    parts = line.split()
    if len(parts) < 7:
        return None
    try:
        ind_alloc = float(parts[-3])
        ind_pct = float(parts[-4])
        inh_alloc = float(parts[-1])
        inh_pct = float(parts[-2])
        int(parts[-6])
        int(parts[-5])
    except (ValueError, IndexError):
        return None
    return (indent, parts[0], parts[1], parts[2], ind_pct, inh_pct)


def is_skip(scc):
    return bool(SKIP_AS_PARENT_SCC.search(scc) or SKIP_AS_PARENT_MATCHER.search(scc))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('prof')
    ap.add_argument('--levels', type=int, default=2,
                    help='How many non-skipped ancestors to record (1 = parent, '
                    '2 = grandparent label, etc.)')
    ap.add_argument('--top', type=int, default=15,
                    help='Top-N rows per target')
    args = ap.parse_args()

    with open(args.prof) as f:
        lines = f.readlines()

    tree_start = None
    for i, line in enumerate(lines):
        if 'no.' in line and '%time' in line and '%alloc' in line:
            tree_start = i + 1
            break
    if tree_start is None:
        print('Could not find call tree', file=sys.stderr)
        sys.exit(1)

    ancestors = {}
    aggregates = defaultdict(lambda: defaultdict(float))
    target_total = defaultdict(float)

    for line in lines[tree_start:]:
        parsed = parse_line(line.rstrip('\n'))
        if not parsed:
            continue
        indent, scc, module, src, ind_pct, inh_pct = parsed
        ancestors[indent] = (scc, module, src, inh_pct)
        for k in list(ancestors.keys()):
            if k > indent:
                del ancestors[k]

        for tgt_name, regex in TARGETS.items():
            if regex.search(src) or regex.search(scc):
                # Walk up, skip transitive compares + pattern-synonym matchers
                non_skipped = []
                for k in sorted(ancestors.keys(), reverse=True):
                    if k >= indent:
                        continue
                    parent_scc, parent_mod, parent_src, _ = ancestors[k]
                    if not is_skip(parent_scc):
                        non_skipped.append((parent_scc, parent_mod, parent_src))
                        if len(non_skipped) >= args.levels:
                            break
                if non_skipped:
                    # Use Nth ancestor as the bin label, with full chain in side label
                    bin_scc, bin_mod, bin_src = non_skipped[-1]
                    bin_label = f'{bin_scc}\t{bin_mod}\t{bin_src}'
                else:
                    bin_label = '(root)\t-\t-'
                if inh_pct > 0:
                    aggregates[tgt_name][bin_label] += inh_pct
                    target_total[tgt_name] += inh_pct

    for tgt_name in sorted(TARGETS.keys()):
        agg = aggregates.get(tgt_name, {})
        if not agg:
            continue
        print(f'\n=== {tgt_name} (sum inherited% = {target_total[tgt_name]:.2f}, levels={args.levels}) ===')
        sorted_items = sorted(agg.items(), key=lambda x: -x[1])
        for parent, pct in sorted_items[:args.top]:
            scc, mod, src = parent.split('\t', 2) if '\t' in parent else (parent, '', '')
            print(f'  {pct:6.2f}%  {scc[:50]:50s}  {src[:60]}')


if __name__ == '__main__':
    main()
