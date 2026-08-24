#!/usr/bin/env python3
"""Numeric certificate for the Appendix-Z Amin instances of Trilogy Paper 2A.

Certifies, in exact rational arithmetic (fractions.Fraction, no floats), the
RESTATED Appendix-Z results as they stand in the manuscript:

  Lemma Z.0'  the corrected load-weighted constant
              ell(b,r) = |b cap Alloc(e*)| - 1[e* in r],
              eta_Z    = min{ slack(b,r)/ell(b,r) : x*_r(b) = 0, ell(b,r) >= 1 },
              eps_bar_Z = min(eta_Z, u_min): lower-bounds the true admissible
              perturbation everywhere, and attains it in the singleton regime.
  Lemma Z.1   restated hypotheses (i)-(iv) are satisfiable, and conclusions
              (L1) the perturbed point is a full Definition-2.1 equilibrium,
              (L2) revenue increment = eps * q_{e*},
              (L3) same-cardinality witness (a faithful min-price run on the
                   shifted profile delivers the perturbed observation exactly),
              plus the corrected Step 2 slack-decrement identity
              slack' = slack - eps * ell(b,r) on every unused trip.
  Theorem Z.1 the Step 3 chain: the Definition-2.1 equilibria at x* form the
              interval [tau_dagger, tau_sup]; the max-price selection is NOT
              DSIC (explicit profitable misreport); the min-price selection
              survives a DSIC misreport grid; the payer's cap decreases to the
              floor at the allocation boundary (the squeeze).

Three exact rational instances are certified (A, B, R0 -- see INSTANCES), each
one excluded by the OLD hypotheses, plus a 260-instance random sweep with 30
endpoint probes of sup-hood.

Usage: python3 scripts/verify_amin_instance.py   (deterministic; prints PASS/FAIL)
"""
import os
import random
import sys
from fractions import Fraction as F

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
print("[import] running the Appendix-Z model harness amin_z_harness.py ...")
from amin_z_harness import analyze, mkV, welfare_opt, all_trips  # noqa: E402
print("[import] harness OK\n")


# ---------------------------------------------------------------- drafted constants
def zconst(R):
    """eta_Z (load-weighted, min over ell>=1 trips), u_min WITHOUT the u>0 filter,
    eps_bar_Z = min of the two.  Exactly the drafted eq:ell-load / eq:eta-Z /
    eq:u-min (corrected) definitions, evaluated at analyze()'s equilibrium."""
    route = R['route']
    caps = []
    for (btup, r), sl in R['slacks'].items():
        k = sum(1 for m in btup if route.get(m) == 1)
        ell = k - (1 if r == 1 else 0)
        if ell >= 1:
            caps.append(F(sl, ell))
    eta_Z = min(caps) if caps else None                     # None == +infinity
    umin = min([R['u'][m] for m in route if route[m] == 1], default=None)
    eps_bar = min([x for x in (eta_Z, umin) if x is not None], default=None)
    return eta_Z, umin, eps_bar


def def21_check(agents, A, d1, q1, d2, q2, dA, x, tau1, uvec):
    """Full Amin Def-2.1 check, exact: IR (5), stability (6) on ALL trips,
    budget balance (7a)-(7b) [singleton realised trips], clearing (8).
    tau2 = 0 assumed (r2 unsaturated in the clean regime)."""
    M = sorted(agents)
    V = mkV(agents, dA, lambda k: F(0))
    d = {1: d1, 2: d2}
    route = {}
    for b, r in x:
        for m in b:
            route[m] = r
    n1 = sum(1 for b, r in x if r == 1)
    n2 = sum(1 for b, r in x if r == 2)
    ok = all(uvec[m] >= 0 for m in M)                                    # (5)
    ok &= all(uvec[m] == 0 for m in M if m not in route)                 # (7b)
    ok &= (tau1 == 0) or (n1 == q1)                                      # (8)
    ok &= n2 < q2                                                        # tau2=0 valid
    for b, r in all_trips(M, A):                                         # (6)
        ok &= sum(uvec[m] for m in b) >= V(b, d[r]) - (tau1 if r == 1 else F(0))
    for b, r in x:                                                       # (7a), singleton
        assert len(b) == 1
        (m,) = b
        ok &= uvec[m] == V(b, d[r]) - (tau1 if r == 1 else F(0))
    return ok


def payments(R, tau1):
    """Per-agent payments at edge price tau1, singleton trips (p = route price)."""
    return {m: (tau1 if R['route'].get(m) == 1 else F(0)) for m in R['agents']}


# =================================================================== the 3 instances
dA2 = lambda k: F(0) if k == 1 else F(3, 5)                 # noqa: E731

INSTANCES = {
    # walrasian-audit instance: tau_dagger = 3/2 > 0, OLD eps_bar = 0 (excluded)
    'A': dict(agents={1: (F(10), F(2)), 2: (F(9), F(2)), 3: (F(4), F(1)),
                      4: (F(3, 2), F(3))},
              A=1, d1=F(0), q1=2, d2=F(1), q2=2, dA=lambda k: F(0),
              pivot=4, eps=F(1, 4)),
    # perturbation-audit instance: the eps = 0.3 repair point
    'B': dict(agents={1: (F(10), F(1)), 2: (F(10), F(1)), 3: (F(4), F(1, 2))},
              A=2, d1=F(1), q1=2, d2=F(2), q2=10, dA=dA2,
              pivot=3, eps=F(3, 10)),
    # tau_dagger = 0 witness: OLD hypothesis (ii) ("consequently tau > 0") excluded it
    'R0': dict(agents={1: (F(10), F(1)), 2: (F(10), F(1)), 3: (F(1, 2), F(1))},
               A=2, d1=F(1), q1=2, d2=F(2), q2=10, dA=dA2,
               pivot=3, eps=F(1, 2)),
}


def run(inst):
    return analyze(inst['agents'], A=inst['A'], d1=inst['d1'], q1=inst['q1'],
                   d2=inst['d2'], q2=inst['q2'], dA=inst['dA'])


def shifted_profile(tag, inst, R, eps):
    """The drafted same-cardinality pivot shift, per instance.
    A : pivot 4 unallocated, alpha-shift so V_r1({4}) = tau_dagger + eps.
    B : pivot 3 allocated on r2, beta-shift (rate d2 - d1 = 1): beta = tau + eps.
    R0: pivot 3 unallocated,  alpha-shift so V_r1({3}) = eps."""
    ag = {m: v for m, v in inst['agents'].items()}
    p = inst['pivot']
    a, b = ag[p]
    if tag == 'A':
        ag[p] = (R['tau'] + eps + b * inst['d1'], b)   # d1 = 0: alpha = tau + eps
    elif tag == 'B':
        ag[p] = (a, R['tau'] + eps)                    # floor = beta (verified below)
    else:
        ag[p] = (R['tau'] + eps + b * inst['d1'], b)   # d1 = 1: alpha = 1 + eps
    return ag


def main():
    print("=" * 74)
    print("CHECK 1 -- restated Lemma Z.1 on 3 instances (hypotheses + (L1)-(L3))")
    print("=" * 74)
    for tag, inst in INSTANCES.items():
        R = run(inst)
        assert R is not None, tag
        eta_Z, umin, eps_bar = zconst(R)
        eps = inst['eps']
        old_excluded = (R['tau'] > 0 and R['eps_bar'] == 0) or R['tau'] == 0
        # hypotheses of the RESTATED lemma
        assert R['S'] > 0, "(i)"
        # (ii) saturated edge: analyze() enforces n1 == q1
        assert eps_bar is not None and eps_bar > 0, "(iii) eps_bar_Z > 0"
        assert 0 < eps < eps_bar
        piv = inst['pivot']
        e_users = [m for m, r in R['route'].items() if r == 1]
        assert piv not in e_users, "(iv) pivot outside Alloc(e*)"
        # (L1): perturbed point is a full Def-2.1 equilibrium
        tau_p = R['tau'] + eps
        u_p = {m: (R['u'][m] - eps if R['route'].get(m) == 1 else R['u'].get(m, F(0)))
               for m in inst['agents']}
        assert def21_check(inst['agents'], inst['A'], inst['d1'], inst['q1'],
                           inst['d2'], inst['q2'], inst['dA'], R['x'], tau_p, u_p), \
            f"(L1) fails at {tag}"
        # (L2): revenue increment = eps * q_{e*}
        p0, p1 = payments(R, R['tau']), payments(R, tau_p)
        assert sum(p1.values()) - sum(p0.values()) == eps * inst['q1'], "(L2)"
        # (L3): same-cardinality witness -- faithful min-price run on the shifted
        # profile delivers each e*-user's perturbed observation exactly
        ag_shift = shifted_profile(tag, inst, R, eps)
        Rs = analyze(ag_shift, A=inst['A'], d1=inst['d1'], q1=inst['q1'],
                     d2=inst['d2'], q2=inst['q2'], dA=inst['dA'])
        assert Rs is not None, f"(L3) shifted profile left the clean regime at {tag}"
        assert set(Rs['x']) == set(R['x']), "(L3) allocation changed"
        assert Rs['tau'] == tau_p, f"(L3) delivered tau {Rs['tau']} != {tau_p}"
        for m in e_users:
            assert m != piv
            p_del = payments(Rs, Rs['tau'])[m]
            assert p_del == p0[m] + eps, f"(L3) delivered payment to {m}"
        # non-e*-users: observation unchanged under the deviation (trivial witness)
        for m in inst['agents']:
            if R['route'].get(m) != 1:
                assert p1[m] == p0[m] and u_p[m] == R['u'].get(m, F(0))
        print(f"  [{tag}] tau={R['tau']} old_eps_bar={R['eps_bar']} "
              f"(old hyps excluded: {old_excluded})  eta_Z={eta_Z} u_min={umin} "
              f"eps_bar_Z={eps_bar}  eps={eps}: (L1) (L2) (L3) all EXACT")
        assert old_excluded

    # witness continuum on B: eps_w = eps_bar_Z at the repair instance
    instB, RB = INSTANCES['B'], run(INSTANCES['B'])
    for eps in (F(1, 10), F(1, 5), F(3, 10), F(2, 5), F(9, 20)):
        Rs = analyze(shifted_profile('B', instB, RB, eps), A=2, d1=F(1), q1=2,
                     d2=F(2), q2=10, dA=dA2)
        assert Rs is not None and Rs['tau'] == RB['tau'] + eps and set(Rs['x']) == set(RB['x'])
    print("  [B] witness continuum: delivered tau = tau_dagger + eps at 5 eps points "
          "(eps_w = eps_bar_Z here)")

    print()
    print("=" * 74)
    print("CHECK 2 -- eps_bar_Z vs the true admissible sup (random sweep)")
    print("=" * 74)
    random.seed(20260821)
    n_clean = n_eq = n_lb = n_operative = 0
    probes = 0
    while n_clean < 260:
        n = random.choice([2, 3, 4])
        ag = {m: (F(random.randint(2, 24), 2), F(random.randint(1, 10), 4))
              for m in range(1, n + 1)}
        A_ = random.choice([1, 2])
        q1 = random.choice([1, 2])
        q2 = random.choice([1, 2, 5])
        d1_, d2_ = F(random.randint(0, 2)), F(random.randint(1, 3))
        da = F(random.randint(0, 12), 10)
        dAf = (lambda k, da=da: F(0) if k == 1 else da)
        R = analyze(ag, A=A_, d1=d1_, q1=q1, d2=d2_, q2=q2, dA=dAf)
        if R is None or R['eta'] is None:
            continue
        n_clean += 1
        if R['tau'] > 0:
            n_operative += 1
        eta_Z, umin, eps_bar = zconst(R)
        assert eps_bar is not None and R['eps_sup'] is not None
        assert eps_bar <= R['eps_sup'], "eps_bar_Z must lower-bound the sup"
        n_lb += 1
        if eps_bar == R['eps_sup']:
            n_eq += 1
        if probes < 30:                       # direct endpoint probes of sup-hood
            probes += 1
            u_p = {m: (R['u'][m] - eps_bar if R['route'].get(m) == 1
                       else R['u'].get(m, F(0))) for m in ag}
            assert def21_check(ag, A_, d1_, q1, d2_, q2, dAf, R['x'],
                               R['tau'] + eps_bar, u_p), "endpoint must be feasible"
            over = eps_bar + F(1, 10**6)
            u_o = {m: (R['u'][m] - over if R['route'].get(m) == 1
                       else R['u'].get(m, F(0))) for m in ag}
            assert not def21_check(ag, A_, d1_, q1, d2_, q2, dAf, R['x'],
                                   R['tau'] + over, u_o), "above the sup must fail"
    print(f"  clean instances: {n_clean} (operative tau>0: {n_operative})")
    print(f"  eps_bar_Z <= true sup: {n_lb}/{n_clean}   equality (singleton regime): "
          f"{n_eq}/{n_clean}   endpoint probes (feasible at eps_bar_Z, infeasible "
          f"above): {probes}/30")
    assert n_lb == n_clean and n_eq == n_clean and n_clean >= 200

    print()
    print("=" * 74)
    print("CHECK 3 -- Theorem Z.1 Step 3 chain (equilibrium continuum, DSIC squeeze)")
    print("=" * 74)
    instA, RA = INSTANCES['A'], run(INSTANCES['A'])
    # (a) the Def-2.1 equilibrium set at x* is the interval [tau_dagger, tau_sup]
    tau_sup = RA['tau'] + RA['eps_sup']
    for t, expect in ((RA['tau'], True), (tau_sup, True), (RA['tau'] - F(1, 100), False),
                      (tau_sup + F(1, 100), False)):
        u_t = {m: (RA['u'][m] - (t - RA['tau']) if RA['route'].get(m) == 1
                   else RA['u'].get(m, F(0))) for m in instA['agents']}
        got = def21_check(instA['agents'], 1, F(0), 2, F(1), 2, instA['dA'],
                          RA['x'], t, u_t)
        assert got == expect, f"continuum at tau={t}"
    print(f"  [A] Def-2.1 equilibria at x* form [{RA['tau']}, {tau_sup}]: endpoints "
          f"feasible, outside infeasible")
    # (b) max-price selection is NOT DSIC: agent 1 under-reports beta 2 -> 8/5
    ag_mis = dict(instA['agents'])
    ag_mis[1] = (F(10), F(8, 5))
    Rm = analyze(ag_mis, A=1, d1=F(0), q1=2, d2=F(1), q2=2, dA=instA['dA'])
    assert Rm is not None and set(Rm['x']) == set(RA['x'])
    tau_sup_mis = Rm['tau'] + Rm['eps_sup']
    u_true_truth = F(10) - tau_sup            # true V_r1({1}) = 10 (d1 = 0)
    u_true_mis = F(10) - tau_sup_mis
    assert tau_sup == 2 and tau_sup_mis == F(8, 5) and u_true_mis > u_true_truth
    print(f"  [A] max-price selection: truthful tau={tau_sup}, u_1={u_true_truth}; "
          f"misreport beta=8/5 gives tau={tau_sup_mis}, u_1={u_true_mis} "
          f"(gain {u_true_mis - u_true_truth} > 0) => NOT DSIC")
    # (c) min-price selection: DSIC misreport grid on A and B
    for tag in ('A', 'B'):
        inst, R0_ = INSTANCES[tag], run(INSTANCES[tag])
        V_true = mkV(inst['agents'], inst['dA'], lambda k: F(0))
        d = {1: inst['d1'], 2: inst['d2']}
        checked = skipped = 0
        for m in inst['agents']:
            a0, b0 = inst['agents'][m]
            r_truth = R0_['route'].get(m)
            u_truth = (V_true(frozenset([m]), d[r_truth]) -
                       (R0_['tau'] if r_truth == 1 else F(0))) if r_truth else F(0)
            for da_ in (F(-2), F(-1), F(-1, 2), F(0), F(1, 2), F(1), F(2)):
                for db_ in (F(-1, 2), F(-1, 4), F(0), F(1, 4), F(1, 2)):
                    if (da_, db_) == (F(0), F(0)) or a0 + da_ <= 0 or b0 + db_ <= 0:
                        continue
                    ag2 = dict(inst['agents'])
                    ag2[m] = (a0 + da_, b0 + db_)
                    R2 = analyze(ag2, A=inst['A'], d1=inst['d1'], q1=inst['q1'],
                                 d2=inst['d2'], q2=inst['q2'], dA=inst['dA'])
                    if R2 is None:
                        # fall back: if m is unallocated at the reported optimum,
                        # its true utility is 0 regardless of regime cleanliness
                        V2 = mkV(ag2, inst['dA'], lambda k: F(0))
                        _, x2, nopt2 = welfare_opt(sorted(ag2), inst['A'], inst['q1'],
                                                   inst['q2'], V2, inst['d1'], inst['d2'])
                        alloc2 = set().union(*[set(b) for b, _ in x2]) if x2 else set()
                        if nopt2 == 1 and m not in alloc2:
                            assert F(0) <= u_truth, "DSIC: unallocated misreport"
                            checked += 1
                        else:
                            skipped += 1
                        continue
                    r2 = R2['route'].get(m)
                    u_mis = (V_true(frozenset([m]), d[r2]) -
                             (R2['tau'] if r2 == 1 else F(0))) if r2 else F(0)
                    assert u_mis <= u_truth, \
                        f"DSIC violation of the min-price selection: {tag} m={m}"
                    checked += 1
        print(f"  [{tag}] min-price DSIC grid: {checked} misreports checked, "
              f"0 profitable ({skipped} outside the clean regime, skipped)")
    # (d) boundary squeeze on A, payer 1: cap decreases to the floor tau_dagger
    caps_seen = []
    for bt in (F(2), F(19, 10), F(17, 10), F(8, 5), F(31, 20)):
        ag2 = dict(instA['agents'])
        ag2[1] = (F(10), bt)
        R2 = analyze(ag2, A=1, d1=F(0), q1=2, d2=F(1), q2=2, dA=instA['dA'])
        assert R2 is not None and set(R2['x']) == set(RA['x']) and R2['tau'] == RA['tau']
        caps_seen.append(R2['tau'] + R2['eps_sup'])
        assert caps_seen[-1] == bt                       # binding cap = payer's own
    assert caps_seen == sorted(caps_seen, reverse=True)
    ag2 = dict(instA['agents'])
    ag2[1] = (F(10), F(29, 20))                          # below the boundary 3/2
    R2 = analyze(ag2, A=1, d1=F(0), q1=2, d2=F(1), q2=2, dA=instA['dA'])
    assert R2 is None or set(R2['x']) != set(RA['x'])
    print(f"  [A] payer-1 cap along beta-reports 2 -> 31/20: {caps_seen} "
          f"(decreasing to floor {RA['tau']}); report 29/20 exits the cell "
          f"(allocation changes) => inf cap over the cell = floor = tau_dagger")
    # (e) revenue identity under c^fix (already asserted per instance in CHECK 1)
    print("  revenue identity R(delta_eps) - R(id) = eps * q_{e*}: asserted in CHECK 1")

    print()
    print("=" * 74)
    print("CHECK 4 -- corrected Step 2 on the audit's omitted-case witness (B)")
    print("=" * 74)
    eps = F(3, 10)
    VB = RB['V']
    routeB = RB['route']
    dB = {1: F(1), 2: F(2)}
    tau_p = RB['tau'] + eps
    u_p = {m: (RB['u'][m] - eps if routeB.get(m) == 1 else RB['u'].get(m, F(0)))
           for m in instB['agents']}
    for (btup, r), sl_before in sorted(RB['slacks'].items()):
        b = frozenset(btup)
        k = sum(1 for m in btup if routeB.get(m) == 1)
        ell = k - (1 if r == 1 else 0)
        sl_after = (sum(u_p[m] for m in b) - VB(b, dB[r]) + (tau_p if r == 1 else F(0)))
        assert sl_after == sl_before - eps * ell, "slack-decrement identity"
        assert sl_after >= 0
    omitted = RB['slacks'][((1,), 2)]
    after_omitted = u_p[1] - VB(frozenset([1]), F(2))
    assert omitted == F(1, 2) and after_omitted == F(1, 5) and after_omitted != omitted
    blocker = RB['slacks'][((3,), 1)]
    after_blocker = u_p[3] - VB(frozenset([3]), F(1)) + tau_p
    assert blocker == 0 and after_blocker == eps
    print(f"  omitted case ({{1}}, r2), e* not in r, agent 1 in Alloc(e*): slack "
          f"{omitted} -> {after_omitted} (drops by eps*ell = {eps}, ell=1; the "
          f"manuscript's 'both sides unchanged' is false, the restated (L1)/Step 2 "
          f"covers it)")
    print(f"  old blocker ({{3}}, r1), ell=-1: slack {blocker} -> {after_blocker} "
          f"(LOOSENS by eps; needs no eta bound, fixing the direction inversion)")
    print(f"  slack-decrement identity slack' = slack - eps*ell(b,r) holds on all "
          f"{len(RB['slacks'])} unused trips of B, all slacks stay >= 0")

    print()


if __name__ == "__main__":
    try:
        main()
    except AssertionError as exc:
        print(f"\nFAIL: {exc}")
        sys.exit(1)
    print("\nALL CHECKS PASSED -- PASS")
