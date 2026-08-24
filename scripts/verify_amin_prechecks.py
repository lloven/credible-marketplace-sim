#!/usr/bin/env python3
"""Numeric certificate for the Appendix-Z constants of Trilogy Paper 2A.

Certifies the two population-level claims the restated Appendix-Z results rest
on, in exact rational arithmetic: (1) genericity of eta_Z, i.e. the corrected
load-weighted constant of Lemma Z.0' is strictly positive on the operative
event tau_dagger(e*) > 0 -- exactly where the manuscript's printed eta is
identically 0 -- and never exceeds the true admissible perturbation sup; and
(2) welfare-maximality of the supportable allocations, i.e. every allocation
carrying a Definition-2.1 equilibrium is welfare-maximising, which Theorem Z.1
uses when it quantifies over ARBITRARY Definition-2.1 equilibria.

SWEEP 1 -- generic positivity of the CORRECTED load-weighted constant of Z.0':
    ell(b,r)  = |b cap Alloc(e*)| - 1[e* in r]          (perturbation load)
    eta_Z     = min { slack(b,r)/ell(b,r) : x*_r(b)=0, ell(b,r) >= 1 }   (min over empty = +inf)
    eps_bar_Z = min( eta_Z , u_dagger_min(e*) )         [eq:u-min keeps only u>0]
  The repair needs eta_Z > 0 generically on the OPERATIVE event tau_dagger(e*) > 0
  (which is exactly where the printed eta is identically 0).

SWEEP 2 -- risk g-2: Theorem Z.1 restated for ARBITRARY Definition-2.1 equilibria uses
  welfare-maximality of x*.  For every feasible allocation we test exact LP feasibility of
  Amin Def-2.1 (5)-(8) as the appendix restates them, and compare the supportable set
  against the welfare-maximising set.

Model, instance generation and equilibrium machinery come from amin_z_harness.py
(imported -- its own assertion suite runs on import).  Exact rationals throughout:
Fraction for the equilibrium arithmetic and for the Def-2.1 LP feasibility
(hand-rolled phase-1 simplex, cross-checked against scipy/HiGHS on every call).

Usage:  python3 scripts/verify_amin_prechecks.py [n_target_operative] [n_sweep2_instances]
                                                 [--out DIR]
        python3 scripts/verify_amin_prechecks.py selftest
Prints PASS/FAIL.  With --out DIR, also writes sweep1.json, sweep2.json and
witnesses.json there.  Requires scipy (float cross-check of every LP verdict).
"""
import json
import os
import random
import statistics
import sys
from fractions import Fraction as F

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)

print("[import] running the Appendix-Z model harness amin_z_harness.py ...")
from amin_z_harness import analyze, mkV, enum_allocations, all_trips  # noqa: E402
print("[import] harness OK\n")

from scipy.optimize import linprog as sp_linprog                  # noqa: E402


def s(f):
    return str(f)


# --------------------------------------------------------------------------- instances
def gen_instance(rng, mode="coarse"):
    """Three arms of the SAME model.
    coarse     -- verify_z.py's own sweep pattern (verify_z.py:176-183), a tie-rich lattice
    fine       -- identical ranges on a 1000x finer rational lattice (ties become rare)
    degenerate -- deliberately tie-rich small integers, to HUNT for eta_Z == 0"""
    n = rng.choice([2, 3, 4])
    if mode == "fine":
        ag = {m: (F(rng.randint(2000, 24000), 2000), F(rng.randint(1000, 10000), 4000))
              for m in range(1, n + 1)}
        da = F(rng.randint(0, 12000), 10000)
    elif mode == "degenerate":
        ag = {m: (F(rng.randint(1, 5)), F(rng.randint(1, 2)))
              for m in range(1, n + 1)}
        da = F(rng.randint(0, 1))
    else:
        ag = {m: (F(rng.randint(2, 24), 2), F(rng.randint(1, 10), 4))
              for m in range(1, n + 1)}
        da = F(rng.randint(0, 12), 10)
    return dict(agents=ag, A=rng.choice([1, 2]), q1=rng.choice([1, 2]),
                q2=rng.choice([1, 2, 5]), d1=F(rng.randint(0, 2)), d2=F(rng.randint(1, 3)),
                da=da)


def dA_of(inst):
    da = inst['da']
    return lambda k: F(0) if k == 1 else da


def run_analyze(inst):
    return analyze(inst['agents'], A=inst['A'], d1=inst['d1'], q1=inst['q1'],
                   d2=inst['d2'], q2=inst['q2'], dA=dA_of(inst))


def inst_json(inst):
    return dict(agents={str(m): [s(a), s(b)] for m, (a, b) in inst['agents'].items()},
                A=inst['A'], q1=inst['q1'], q2=inst['q2'],
                d1=s(inst['d1']), d2=s(inst['d2']), dAlpha_k_ge_2=s(inst['da']))


# ------------------------------------------------------------------- SWEEP 1 quantities
def zconstants(R):
    """eta_Z, u_dagger_min(e*), eps_bar_Z and the true admissible sup, from analyze()'s output.

    R['slacks'][(b_tuple, r)] is the eq:eta-stab slack of unused trip (b,r) at tau_dagger;
    route r==1 is the route through e*, so Alloc(e*) = {m : route[m] == 1}."""
    route = R['route']
    caps, loads = [], []
    for (btup, r), sl in R['slacks'].items():
        k = sum(1 for m in btup if route.get(m) == 1)
        ell = k - (1 if r == 1 else 0)
        if ell >= 1:
            caps.append(F(sl, ell))
            loads.append(((btup, r), ell, sl, F(sl, ell)))
    eta_Z = min(caps) if caps else None                       # None == +infinity
    u_min = R['u_min']                                        # eq:u-min: min over u > 0
    if eta_Z is None:
        eps_bar_Z = u_min
    elif u_min is None:
        eps_bar_Z = eta_Z
    else:
        eps_bar_Z = min(eta_Z, u_min)
    return dict(eta_Z=eta_Z, u_min=u_min, eps_bar_Z=eps_bar_Z,
                true_sup=R['eps_sup'], n_load_trips=len(caps), loads=loads)


def sweep1(n_target=600, max_draws=40000, seed=20260821):
    out = {}
    for k, arm in enumerate(("coarse", "fine", "degenerate")):
        rng = random.Random(seed + k)
        rows, draws, n_clean = [], 0, 0
        while draws < max_draws and sum(1 for r in rows if r['tau'] > 0) < n_target:
            draws += 1
            inst = gen_instance(rng, mode=arm)
            R = run_analyze(inst)
            if R is None or R['eta'] is None:
                continue
            n_clean += 1
            Z = zconstants(R)
            rows.append(dict(inst=inst, tau=R['tau'], eta_old=R['eta'], **Z))
        op = [r for r in rows if r['tau'] > 0]
        finite = [r['eta_Z'] for r in op if r['eta_Z'] is not None]
        zeros = [r for r in op if r['eta_Z'] is not None and r['eta_Z'] == 0]
        inf = [r for r in op if r['eta_Z'] is None]
        eps_zeros = [r for r in op if r['eps_bar_Z'] is not None and r['eps_bar_Z'] == 0]
        eps_none = [r for r in op if r['eps_bar_Z'] is None]
        # soundness of eps_bar_Z as a bound on the admissible perturbation
        unsound = [r for r in op if r['eps_bar_Z'] is not None and r['true_sup'] is not None
                   and r['eps_bar_Z'] > r['true_sup']]
        offop = [r for r in rows if r['tau'] == 0]
        offop_zeros = [r for r in offop if r['eta_Z'] is not None and r['eta_Z'] == 0]
        q = {}
        if finite:
            fl = sorted(float(x) for x in finite)
            q = dict(min=fl[0], p10=fl[int(.10 * (len(fl) - 1))], p25=fl[int(.25 * (len(fl) - 1))],
                     median=statistics.median(fl), p75=fl[int(.75 * (len(fl) - 1))],
                     p90=fl[int(.90 * (len(fl) - 1))], max=fl[-1], mean=statistics.fmean(fl))
        out[arm] = dict(
            draws=draws, clean=n_clean, operative=len(op),
            operative_old_eta_zero=sum(1 for r in op if r['eta_old'] == 0),
            eta_Z_positive=len(op) - len(zeros), eta_Z_zero=len(zeros), eta_Z_infinite=len(inf),
            eps_bar_Z_positive=len(op) - len(eps_zeros) - len(eps_none),
            eps_bar_Z_zero=len(eps_zeros), eps_bar_Z_undefined=len(eps_none),
            eps_bar_Z_exceeds_true_sup=len(unsound),
            offoperative=len(offop), offoperative_eta_Z_zero=len(offop_zeros),
            eta_Z_distribution=q,
            witnesses=dict(
                eta_Z_zero=[witness1(r) for r in zeros[:10]],
                eps_bar_Z_zero=[witness1(r) for r in eps_zeros[:10]],
                eps_bar_Z_unsound=[witness1(r) for r in unsound[:10]]))
        print(f"[sweep1/{arm}] draws={draws} clean={n_clean} operative(tau>0)={len(op)}  "
              f"eta_Z>0: {len(op) - len(zeros)}/{len(op)}  (zero: {len(zeros)}, +inf: {len(inf)})")
        print(f"[sweep1/{arm}] eps_bar_Z>0: {out[arm]['eps_bar_Z_positive']}/{len(op)}  "
              f"eps_bar_Z>true_sup: {len(unsound)}   off-operative eta_Z==0: "
              f"{len(offop_zeros)}/{len(offop)}")
        if q:
            print(f"[sweep1/{arm}] eta_Z dist: min={q['min']:.6g} p10={q['p10']:.6g} "
                  f"med={q['median']:.6g} p90={q['p90']:.6g} max={q['max']:.6g}")
    return out


def witness1(r):
    return dict(instance=inst_json(r['inst']), tau_dagger=s(r['tau']), eta_old=s(r['eta_old']),
                eta_Z=(None if r['eta_Z'] is None else s(r['eta_Z'])),
                u_dagger_min=(None if r['u_min'] is None else s(r['u_min'])),
                eps_bar_Z=(None if r['eps_bar_Z'] is None else s(r['eps_bar_Z'])),
                true_sup=(None if r['true_sup'] is None else s(r['true_sup'])),
                n_load_trips=r['n_load_trips'],
                load_trips=[[list(b), rr, ell, s(sl), s(cap)] for (b, rr), ell, sl, cap in r['loads']])


# ------------------------------------------------------- SWEEP 2: Def-2.1 vs welfare-max
def def21_matrices(x, M, A, q1, q2, V, d1, d2):
    """Amin Def-2.1 (5)-(8) at allocation x, as restated in appendix-z-theorem-z1.tex, in
    matrix form over the variables [u_m : m allocated] + [tau_r : r saturated] (all >= 0):
      (5)  u_m >= 0                     -- the variable bound
      (7b) u_m = 0 for unallocated m    -- the agent is simply not a variable
      (8)  tau_r > 0 only if saturated  -- an unsaturated route's price is not a variable
      (7a) sum_{m in b} u_m + tau_r = V_r(b)  on used trips
      (6)  sum_{m in b} u_m + tau_r >= V_r(b) on every trip (b,r) in B x R
    Returns (varlist, A_ub, b_ub, A_eq, b_eq)."""
    n = {1: sum(1 for b, r in x if r == 1), 2: sum(1 for b, r in x if r == 2)}
    q = {1: q1, 2: q2}
    alloc = set()
    for b, _ in x:
        alloc |= set(b)
    variables = [('u', m) for m in M if m in alloc] + \
                [('t', r) for r in (1, 2) if n[r] == q[r]]
    idx = {v: i for i, v in enumerate(variables)}
    d = {1: d1, 2: d2}

    def row(b, r):
        z = [F(0)] * len(variables)
        for m in b:
            if ('u', m) in idx:
                z[idx[('u', m)]] += F(1)
        if ('t', r) in idx:
            z[idx[('t', r)]] += F(1)
        return z
    A_eq, b_eq, A_ub, b_ub = [], [], [], []
    used = set(x)
    for b, r in x:
        A_eq.append(row(b, r))
        b_eq.append(V(b, d[r]))
    for b, r in all_trips(M, A):
        if (b, r) in used:
            continue
        A_ub.append([-z for z in row(b, r)])            # -(sum u + tau) <= -V
        b_ub.append(-V(b, d[r]))
    return variables, A_ub, b_ub, A_eq, b_eq


def _pack(variables, vals, M, x, q1, q2):
    n = {1: sum(1 for b, r in x if r == 1), 2: sum(1 for b, r in x if r == 2)}
    u = {m: F(0) for m in M}
    tau = {1: F(0), 2: F(0)}
    for v, val in zip(variables, vals):
        (u if v[0] == 'u' else tau)[v[1]] = val
    return dict(u=u, tau=tau)


def _phase1(rows, rhs, nvar):
    """Exact phase-1 simplex (Fractions, Bland's rule) for {x >= 0 : Ax = b}.
    Returns a feasible x (first nvar coordinates) or None if infeasible.

    Hand-rolled because sympy 1.14's simplex is unreliable on systems of this shape: its
    symbolic `lpmin` returns points violating equality constraints, and its matrix-form
    `linprog` returns a bogus point instead of raising on infeasible systems (both
    reproduced; see the verdict file).  Every verdict here is cross-checked against
    scipy/HiGHS and every certificate re-verified by check_cert()."""
    m = len(rows)
    if m == 0:
        return [F(0)] * nvar
    n = len(rows[0])
    A = [r[:] for r in rows]
    b = list(rhs)
    for i in range(m):
        if b[i] < 0:
            A[i] = [-v for v in A[i]]
            b[i] = -b[i]
    N = n + m                                    # artificials appended
    T = [A[i] + [F(1) if j == i else F(0) for j in range(m)] + [b[i]] for i in range(m)]
    basis = list(range(n, N))
    # reduced-cost row of w = sum(artificials): rc_j = sum_i A_ij for real/slack columns,
    # 0 for the artificial columns themselves (they are the initial basis); rc_N = w.
    cost = ([sum(T[i][j] for i in range(m)) for j in range(n)] + [F(0)] * m
            + [sum(b)])
    while True:
        col = -1
        inbasis = set(basis)
        for j in range(n):                      # artificials never re-enter the basis
            if j not in inbasis and cost[j] > 0:
                col = j
                break
        if col < 0:
            break
        prow, best, bestvar = -1, None, None
        for i in range(m):
            if T[i][col] > 0:
                ratio = T[i][N] / T[i][col]
                if best is None or ratio < best or (ratio == best and basis[i] < bestvar):
                    best, prow, bestvar = ratio, i, basis[i]
        if prow < 0:
            break
        pv = T[prow][col]
        T[prow] = [v / pv for v in T[prow]]
        for i in range(m):
            if i != prow and T[i][col] != 0:
                f = T[i][col]
                T[i] = [T[i][j] - f * T[prow][j] for j in range(N + 1)]
        if cost[col] != 0:
            f = cost[col]
            cost = [cost[j] - f * T[prow][j] for j in range(N + 1)]
        basis[prow] = col
    if cost[N] != 0:
        return None
    xs = [F(0)] * n
    for i, bi in enumerate(basis):
        if bi < n:
            xs[bi] = T[i][N]
    return xs[:nvar]


def supports_def21(x, M, A, q1, q2, V, d1, d2):
    """Exact (rational) LP feasibility of Def-2.1 at x.  Returns a certificate or None."""
    variables, A_ub, b_ub, A_eq, b_eq = def21_matrices(x, M, A, q1, q2, V, d1, d2)
    nv = len(variables)
    if not nv:                                           # no free variable: pure arithmetic
        ok = all(bb == 0 for bb in b_eq) and all(0 <= bb for bb in b_ub)
        return dict(u={m: F(0) for m in M}, tau={1: F(0), 2: F(0)}) if ok else None
    ns = len(A_ub)
    rows = [A_ub[i] + [F(1) if k == i else F(0) for k in range(ns)] for i in range(ns)]
    rows += [r + [F(0)] * ns for r in A_eq]
    sol = _phase1(rows, list(b_ub) + list(b_eq), nv)
    return None if sol is None else _pack(variables, sol, M, x, q1, q2)


def supports_def21_float(x, M, A, q1, q2, V, d1, d2):
    """Independent float cross-check of the feasibility verdict (scipy/HiGHS)."""
    variables, A_ub, b_ub, A_eq, b_eq = def21_matrices(x, M, A, q1, q2, V, d1, d2)
    if not variables:
        return all(bb == 0 for bb in b_eq) and all(0 <= bb for bb in b_ub)
    res = sp_linprog([0.0] * len(variables),
                     A_ub=[[float(z) for z in r] for r in A_ub] or None,
                     b_ub=[float(z) for z in b_ub] or None,
                     A_eq=[[float(z) for z in r] for r in A_eq] or None,
                     b_eq=[float(z) for z in b_eq] or None,
                     bounds=(0, None), method='highs')
    return bool(res.success)


def check_cert(x, M, A, q1, q2, V, d1, d2, cert):
    """Re-verify a certificate against (5)-(8) in exact Fractions (independent of sympy)."""
    u, tau, d = cert['u'], cert['tau'], {1: d1, 2: d2}
    n = {1: sum(1 for b, r in x if r == 1), 2: sum(1 for b, r in x if r == 2)}
    alloc = set()
    for b, _ in x:
        alloc |= set(b)
    ok = all(u[m] >= 0 for m in M)                                       # (5)
    ok &= all(u[m] == 0 for m in M if m not in alloc)                    # (7b)
    ok &= all(tau[r] >= 0 for r in (1, 2))
    ok &= all(tau[r] == 0 for r in (1, 2) if n[r] < [0, q1, q2][r])      # (8)
    ok &= all(sum(u[m] for m in b) == V(b, d[r]) - tau[r] for b, r in x)  # (7a)
    ok &= all(sum(u[m] for m in b) >= V(b, d[r]) - tau[r]                # (6)
              for b, r in all_trips(M, A))
    return ok


def sweep2(n_instances=500, seed=20260821, max_allocations=4000, prefilter=True):
    rng = random.Random(seed + 7)
    rows, agree, disagree, skipped, no_eq = [], 0, 0, 0, 0
    solver_mismatch = 0
    covers = 0
    done = 0
    while done < n_instances:
        inst = gen_instance(rng)
        M = sorted(inst['agents'])
        V = mkV(inst['agents'], dA_of(inst), lambda k: F(0))
        allocs = enum_allocations(M, inst['A'], inst['q1'], inst['q2'])
        if len(allocs) > max_allocations:
            skipped += 1
            continue
        done += 1
        d = {1: inst['d1'], 2: inst['d2']}
        w = [sum(V(b, d[r]) for b, r in x) for x in allocs]
        wmax = max(w)
        wset = {i for i, ww in enumerate(w) if ww == wmax}
        eq_idx, certs = [], {}
        for i, x in enumerate(allocs):
            # exact necessary conditions first (cheap; NOT the welfare theorem):
            # every used trip needs V_r(b) = sum_b u + tau_r >= 0
            if prefilter and any(V(b, d[r]) < 0 for b, r in x):
                continue
            # a trip whose coalition is entirely unallocated on an unsaturated route
            # has LHS 0 and tau_r = 0 in (6)
            allocset = set()
            for b, _ in x:
                allocset |= set(b)
            nn = {1: sum(1 for b, r in x if r == 1), 2: sum(1 for b, r in x if r == 2)}
            sat = {1: nn[1] == inst['q1'], 2: nn[2] == inst['q2']}
            if prefilter and any((not sat[r]) and (not (set(b) & allocset)) and V(b, d[r]) > 0
                                 for b, r in all_trips(M, inst['A'])):
                continue
            cert = supports_def21(x, M, inst['A'], inst['q1'], inst['q2'], V, d[1], d[2])
            if bool(cert is not None) != supports_def21_float(
                    x, M, inst['A'], inst['q1'], inst['q2'], V, d[1], d[2]):
                solver_mismatch += 1
            if cert is not None:
                assert check_cert(x, M, inst['A'], inst['q1'], inst['q2'], V, d[1], d[2], cert), \
                    "sympy certificate failed the exact Fraction re-check"
                eq_idx.append(i)
                certs[i] = cert
        bad = [i for i in eq_idx if i not in wset]
        if bad:
            disagree += 1
            rows.append(dict(inst=inst, allocs=allocs, w=w, wmax=wmax, bad=bad, certs=certs))
        else:
            agree += 1
        if not eq_idx:
            no_eq += 1
        if set(wset) <= set(eq_idx):
            covers += 1
        if done % 50 == 0:
            print(f"[sweep2] {done}/{n_instances} instances  agree={agree} disagree={disagree} "
                  f"no-equilibrium={no_eq}")
    print(f"[sweep2] DONE {done} instances: agreement {agree}/{done}, disagreements {disagree}, "
          f"instances with no Def-2.1 equilibrium at any allocation {no_eq}, "
          f"welfare-max fully supportable {covers}/{done}, skipped(too many allocations) {skipped}")
    wit = []
    for r in rows[:10]:
        for i in r['bad']:
            wit.append(dict(instance=inst_json(r['inst']),
                            allocation=[[sorted(b), rr] for b, rr in r['allocs'][i]],
                            welfare=s(r['w'][i]), welfare_max=s(r['wmax']),
                            certificate=dict(u={str(m): s(v) for m, v in r['certs'][i]['u'].items()},
                                             tau={str(k): s(v) for k, v in r['certs'][i]['tau'].items()})))
    print(f"[sweep2] exact(simplex) vs float(HiGHS) feasibility-oracle mismatches: "
          f"{solver_mismatch}")
    return dict(instances=done, agreement=agree, disagreement=disagree, no_equilibrium=no_eq,
                welfare_max_supportable=covers, skipped_too_large=skipped,
                solver_mismatches=solver_mismatch, witnesses=wit)


# ---------------------------------------------------------------------------- self-check
def selftest():
    """Pin both quantities on the two audit instances of verify-appendix-z-trio.md."""
    A_inst = dict(agents={1: (F(10), F(2)), 2: (F(9), F(2)), 3: (F(4), F(1)), 4: (F(3, 2), F(3))},
                  A=1, d1=F(0), q1=2, d2=F(1), q2=2, da=F(0))
    B_inst = dict(agents={1: (F(10), F(1)), 2: (F(10), F(1)), 3: (F(4), F(1, 2))},
                  A=2, d1=F(1), q1=2, d2=F(2), q2=10, da=F(3, 5))
    for tag, inst in (("A", A_inst), ("B", B_inst)):
        R = run_analyze(inst)
        Z = zconstants(R)
        print(f"  instance {tag}: tau_dagger={R['tau']} eta_old={R['eta']} "
              f"eta_Z={Z['eta_Z']} u_min={Z['u_min']} eps_bar_Z={Z['eps_bar_Z']} "
              f"true_sup={Z['true_sup']}")
        assert R['eta'] == 0 and Z['eta_Z'] == F(1, 2) and Z['eps_bar_Z'] == F(1, 2), tag
        assert Z['eps_bar_Z'] == Z['true_sup'], tag
    # Def-2.1 machinery: on instance B the welfare optimum is supportable, and the
    # equilibrium prices reproduce tau_dagger = 1/2 as the minimum admissible edge price.
    inst = B_inst
    M = sorted(inst['agents'])
    V = mkV(inst['agents'], dA_of(inst), lambda k: F(0))
    allocs = enum_allocations(M, inst['A'], inst['q1'], inst['q2'])
    d = {1: inst['d1'], 2: inst['d2']}
    w = [sum(V(b, d[r_]) for b, r_ in x) for x in allocs]
    wmax = max(w)
    opt = [x for x, ww in zip(allocs, w) if ww == wmax]
    assert len(opt) == 1
    cert = supports_def21(opt[0], M, inst['A'], inst['q1'], inst['q2'], V, d[1], d[2])
    assert cert is not None and check_cert(opt[0], M, inst['A'], inst['q1'], inst['q2'],
                                           V, d[1], d[2], cert)
    print(f"  instance B welfare-max allocation supportable, certificate tau={cert['tau']}, "
          f"u={cert['u']}")
    # a deliberately sub-optimal allocation (drop the highest-value agent) must NOT be
    # supportable -- exercises the infeasibility branch
    sub = [x for x, ww in zip(allocs, w) if ww < wmax and len(x) == 2]
    n_sub_eq = sum(1 for x in sub
                   if supports_def21(x, M, inst['A'], inst['q1'], inst['q2'], V, d[1], d[2]))
    print(f"  instance B: {n_sub_eq}/{len(sub)} strictly-sub-optimal 2-trip allocations "
          f"admit a Def-2.1 equilibrium (expect 0)")
    assert n_sub_eq == 0
    print("  SELFTEST OK")


# ------------------------------------------------------------------------------- driver
def main(argv):
    argv = list(argv)
    outdir = None
    if "--out" in argv:
        i = argv.index("--out")
        outdir = argv[i + 1]
        del argv[i:i + 2]
    if argv and argv[0] == "selftest":
        selftest()
        return True

    n_target = int(argv[0]) if argv else 600
    n_inst2 = int(argv[1]) if len(argv) > 1 else 500
    selftest()
    print()
    s1 = sweep1(n_target=n_target)
    print()
    s2 = sweep2(n_instances=n_inst2)
    print("\n[sweep2/nofilter] re-running a subsample with the cheap prefilters DISABLED "
          "(exhaustiveness control)")
    s2['nofilter_control'] = sweep2(n_instances=max(100, n_inst2 // 8), prefilter=False)
    if outdir:
        os.makedirs(outdir, exist_ok=True)
        with open(os.path.join(outdir, "sweep1.json"), "w") as fh:
            json.dump(s1, fh, indent=1)
        with open(os.path.join(outdir, "sweep2.json"), "w") as fh:
            json.dump(s2, fh, indent=1)
        with open(os.path.join(outdir, "witnesses.json"), "w") as fh:
            json.dump(dict(sweep1={k: v['witnesses'] for k, v in s1.items()},
                           sweep2=s2['witnesses']), fh, indent=1)
        print(f"\nwrote sweep1.json, sweep2.json, witnesses.json to {outdir}")

    # ------------------------------------------------------------------ verdict
    fails = []
    for arm, r in s1.items():
        if r['eta_Z_zero']:
            fails.append(f"sweep1/{arm}: eta_Z == 0 on {r['eta_Z_zero']} operative instances")
        if r['eps_bar_Z_zero'] or r['eps_bar_Z_undefined']:
            fails.append(f"sweep1/{arm}: eps_bar_Z not positive on "
                         f"{r['eps_bar_Z_zero'] + r['eps_bar_Z_undefined']} operative instances")
        if r['eps_bar_Z_exceeds_true_sup']:
            fails.append(f"sweep1/{arm}: eps_bar_Z exceeds the true sup on "
                         f"{r['eps_bar_Z_exceeds_true_sup']} instances")
        if not r['operative']:
            fails.append(f"sweep1/{arm}: no operative (tau_dagger > 0) instance drawn")
    for tag, r in (("sweep2", s2), ("sweep2/nofilter", s2['nofilter_control'])):
        if r['disagreement']:
            fails.append(f"{tag}: {r['disagreement']} instances support a "
                         f"non-welfare-maximising allocation")
        if r['solver_mismatches']:
            fails.append(f"{tag}: {r['solver_mismatches']} exact-vs-float LP verdict mismatches")
        if r['welfare_max_supportable'] != r['instances']:
            fails.append(f"{tag}: welfare-max unsupportable on "
                         f"{r['instances'] - r['welfare_max_supportable']} instances")
    print()
    for f in fails:
        print(f"  FAIL: {f}")
    return not fails


if __name__ == "__main__":
    ok = main(sys.argv[1:])
    print("\nALL CHECKS PASSED -- PASS" if ok else "\nFAIL")
    sys.exit(0 if ok else 1)
