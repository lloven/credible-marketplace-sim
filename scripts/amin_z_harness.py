#!/usr/bin/env python3
"""Exact-rational model harness for Appendix Z (Amin et al. two-route instance).

Shared machinery for scripts/verify_amin_instance.py and
scripts/verify_amin_prechecks.py: welfare optimisation, the minimum-total-price
Definition-2.1 equilibrium, the eq:eta-stab slacks and the true admissible
perturbation sup.  Importing this module runs its own assertion suite (the
instances and sweeps below), so an import that returns is itself a check.

Model taken ONLY from the appendix's own restatement (appendix-z-theorem-z1.tex):
  - two parallel single-edge routes r1 (edge e*, delay d1, cap q1), r2 (edge e2, d2, q2)
  - V_r(b) = sum_{m in b}(alpha_m - beta_m d_r) - |b|(dA(|b|) + dB(|b|) d_r)   [Amin Eq.1 restated]
  - equilibrium (Def 2.1 restated): IR (5) u_m>=0; stability (6) sum_b u >= V_r(b) - sum_{e in r} tau_e
    for ALL trips; budget balance (7a) sum_b p = sum_{e in r} tau_e on used trips; (7b) p=0 unallocated;
    market clearing (8) tau_e>0 only on saturated edges.
  - Thm 3.10 restated (lines 123, 159, 201-202, 242): VCG-equivalent equilibrium = utility-maximal
    = MINIMUM total edge price sum_e q_e tau_e.
  - eq:eta-stab (line 125): eta = min over UNUSED trips of [sum_b u† - V_r(b) + sum_{e in r} tau†_e].
  - eq:u-min (line 126); eps_bar = min(eta, u_min).
All arithmetic exact (Fraction).
"""
from fractions import Fraction as F
from itertools import combinations
import random

def mkV(agents, dA, dB):
    def V(b, d):
        k = len(b)
        return sum(agents[m][0] - agents[m][1]*d for m in b) - k*(dA(k) + dB(k)*d)
    return V

def enum_allocations(M, A, q1, q2):
    """All feasible trip sets. Trip = (frozenset b, route in {1,2}). Caps: #trips per route."""
    out = []
    def rec(rem, trips, c1, c2):
        if not rem:
            out.append(tuple(trips)); return
        m, rest = rem[0], rem[1:]
        rec(rest, trips, c1, c2)  # m unallocated
        for extra in range(A):
            for co in combinations(rest, extra):
                b = frozenset((m,)+co)
                rest2 = tuple(x for x in rest if x not in co)
                if c1 < q1: rec(rest2, trips+[(b,1)], c1+1, c2)
                if c2 < q2: rec(rest2, trips+[(b,2)], c1, c2+1)
    rec(tuple(M), [], 0, 0)
    return out

def welfare_opt(M, A, q1, q2, V, d1, d2):
    best, bestx, nopt = None, None, 0
    for x in enum_allocations(M, A, q1, q2):
        w = sum(V(b, d1 if r==1 else d2) for b,r in x)
        if best is None or w > best: best, bestx, nopt = w, x, 1
        elif w == best: nopt += 1
    return best, bestx, nopt

def all_trips(M, A):
    ts = []
    for k in range(1, A+1):
        for co in combinations(sorted(M), k):
            ts.append((frozenset(co),1)); ts.append((frozenset(co),2))
    return ts

def analyze(agents, A, d1, q1, d2, q2, dA=lambda k: F(0), dB=lambda k: F(0), verbose=True):
    """Returns dict with tau_dagger (t on e*), eta, u_min, eps_bar, eps_sup, VCG check.
    Requires: x* unique, all used trips singleton, r1 saturated, r2 unsaturated."""
    M = sorted(agents)
    V = mkV(agents, dA, dB)
    S, x, nopt = welfare_opt(M, A, q1, q2, V, d1, d2)
    used = set(x)
    n1 = sum(1 for b,r in x if r==1); n2 = sum(1 for b,r in x if r==2)
    if nopt != 1 or any(len(b)>1 for b,_ in used) or n1 != q1 or n2 >= q2:
        return None  # outside the clean singleton / r1-saturated / r2-unsaturated regime
    route = {}
    for b,r in used:
        for m in b: route[m] = r
    # u_m(t): allocated on r1 -> V1({m}) - t ; on r2 -> V2({m}) (tau_e2 = 0 by (8)) ; else 0
    def u(m, t):
        if m not in route: return F(0)
        d = d1 if route[m]==1 else d2
        return V(frozenset([m]), d) - (t if route[m]==1 else F(0))
    lo, hi = [F(0)], []
    unused = [tr for tr in all_trips(M,A) if tr not in used]
    def slack_slope(b, r):
        d = d1 if r==1 else d2
        c0 = sum(u(m, F(0)) for m in b) - V(b, d) + (F(0) if r==2 else F(0))  # at t=0, tau terms: r1 price t
        # slack(t) = sum u_m(t) - V_r(b) + t*1[r==1]
        k1 = sum(1 for m in b if route.get(m)==1)
        slope = (1 if r==1 else 0) - k1
        c0 = c0 + (F(0))  # tau term at t=0 is 0 either way
        return c0, slope
    for b,r in unused:
        c0, sl = slack_slope(b,r)
        if sl < 0: hi.append(F(c0, -sl))
        elif sl > 0: lo.append(F(-c0, sl))
        else:
            assert c0 >= 0, f"infeasible flat constraint {b,r}"
    for m in M:  # IR upper bounds on t for r1 users
        if route.get(m)==1: hi.append(u(m, F(0)))
    t = max(lo)
    assert not hi or t <= min(hi), "empty equilibrium interval"
    # eta per eq:eta-stab (min over ALL unused trips of raw slack at t)
    slacks = {}
    for b,r in unused:
        c0, sl = slack_slope(b,r)
        slacks[(tuple(sorted(b)),r)] = c0 + sl*t
    eta = min(slacks.values()) if slacks else None
    umin_set = [u(m,t) for m in M if route.get(m)==1 and u(m,t) > 0]
    u_min = min(umin_set) if umin_set else None
    # true sup of upward perturbation eps (raise t by eps): binding = slope<0 constraints + IR
    caps = [F(slacks[(tuple(sorted(b)),r)], -sl) for b,r in unused
            for c0,sl in [slack_slope(b,r)] if sl < 0]
    caps += [u(m,t) for m in M if route.get(m)==1]
    eps_sup = min(caps) if caps else None
    # VCG utilities
    vcg = {}
    for m in M:
        Sm, _, _ = welfare_opt([x_ for x_ in M if x_!=m], A, q1, q2, V, d1, d2)
        vcg[m] = S - Sm
    u_at_t = {m: u(m,t) for m in M}
    return dict(S=S, x=x, tau=t, eta=eta, u_min=u_min,
                eps_bar=(min(eta,u_min) if (eta is not None and u_min is not None) else None),
                eps_sup=eps_sup, slacks=slacks, vcg=vcg, u=u_at_t, route=route, V=V,
                d1=d1, d2=d2, q1=q1, q2=q2, A=A, agents=agents, dA=dA, dB=dB)

def show(tag, R):
    print(f"--- {tag} ---")
    print(f"  x* = {sorted((tuple(sorted(b)),r) for b,r in R['x'])}, S = {R['S']}")
    print(f"  tau_dagger(e*) = {R['tau']}   (min total edge price)")
    print(f"  u_dagger = {dict(sorted(R['u'].items()))}")
    print(f"  VCG S(M)-S(M-m) = {dict(sorted(R['vcg'].items()))}  match u_dagger: "
          f"{all(R['u'][m]==R['vcg'][m] for m in R['u'])}")
    print(f"  unused-trip slacks (eq:eta-stab terms): {dict(sorted(R['slacks'].items()))}")
    print(f"  eta = {R['eta']}, u_min(e*) = {R['u_min']}, eps_bar = min = {R['eps_bar']}")
    print(f"  TRUE sup of admissible upward eps = {R['eps_sup']}")

# ============ Instance A (walrasian-gap audit) ============
agentsA = {1:(F(10),F(2)), 2:(F(9),F(2)), 3:(F(4),F(1)), 4:(F(3,2),F(3))}
RA = analyze(agentsA, A=1, d1=F(0), q1=2, d2=F(1), q2=2)
show("Instance A: e*(d=0,q=2), e2(d=1,q=2); alpha=(10,9,4,1.5), beta=(2,2,1,3), A=1", RA)
assert RA['tau'] == F(3,2) and RA['eta'] == 0 and RA['u_min'] == F(15,2)
assert RA['eps_bar'] == 0 and RA['eps_sup'] == F(1,2)
assert RA['vcg'] == RA['u']

# ============ Instance B (perturbation audit) ============
dA2 = lambda k: F(0) if k==1 else F(3,5)
agentsB = {1:(F(10),F(1)), 2:(F(10),F(1)), 3:(F(4),F(1,2))}
RB = analyze(agentsB, A=2, d1=F(1), q1=2, d2=F(2), q2=10, dA=dA2)
show("Instance B: e*(d=1,q=2), e2(d=2,q=10); (10,1),(10,1),(4,0.5), A=2, dAlpha(2)=0.6", RB)
assert RB['tau'] == F(1,2) and RB['eta'] == 0 and RB['u_min'] == F(17,2)
assert RB['eps_bar'] == 0 and RB['eps_sup'] == F(1,2)
assert RB['vcg'] == RB['u']

# ---- Full Def-2.1 re-check of the perturbed point eps = 0.3 on instance B ----
eps = F(3,10); t = RB['tau'] + eps
V = RB['V']; route = RB['route']
up = {m: (V(frozenset([m]), F(1)) - t) if route.get(m)==1 else
         (V(frozenset([m]), F(2)) if route.get(m)==2 else F(0)) for m in agentsB}
p  = {m: (t if route.get(m)==1 else F(0)) for m in agentsB}
ok_IR   = all(up[m] >= 0 for m in agentsB)
ok_stab = all(sum(up[m] for m in b) >= V(b, F(1) if r==1 else F(2)) - (t if r==1 else F(0))
              for b,r in all_trips(sorted(agentsB), 2))
ok_bb   = all(sum(p[m] for m in b) == (t if r==1 else F(0)) for b,r in RB['x'])
ok_mc   = True  # e* saturated (2=q1) so t>0 admissible; e2 unsaturated with tau=0
print(f"\nPerturbed point eps=3/10 on B: tau'={t}, u'={dict(sorted(up.items()))}, "
      f"IR:{ok_IR} stab(6):{ok_stab} bb(7a):{ok_bb} mc(8):{ok_mc}")
assert ok_IR and ok_stab and ok_bb and ok_mc

# ---- Entrant continuum on B (rem:amin-price-ladder check) ----
print("\nEntrant (4, bt) on B -> tau* of min-price equilibrium:")
row = []
for bt in [F(1,2), F(13,25), F(11,20), F(3,5), F(7,10), F(4,5), F(9,10), F(99,100)]:
    ag = dict(agentsB); ag[9] = (F(4), bt)
    R = analyze(ag, A=2, d1=F(1), q1=2, d2=F(2), q2=10, dA=dA2)
    row.append((bt, R['tau'] if R else None))
print("  " + ", ".join(f"bt={float(b):.2f}->tau={float(t):.2f}" for b,t in row))
assert all(t == b for b,t in row), "price is a continuum equal to entrant beta"

# ============ Structural sweep: tau_dagger>0  <=>  eta==0 (random instances) ============
random.seed(7)
n_pos = n_zero = n_checked = 0
for trial in range(4000):
    n = random.choice([2,3,4])
    ag = {m:(F(random.randint(2,24),2), F(random.randint(1,10),4)) for m in range(1,n+1)}
    A_ = random.choice([1,2]); q1 = random.choice([1,2]); q2 = random.choice([1,2,5])
    d1_, d2_ = F(random.randint(0,2)), F(random.randint(1,3))
    da = F(random.randint(0,12),10)
    R = analyze(ag, A=A_, d1=d1_, q1=q1, d2=d2_, q2=q2,
                dA=(lambda k, da=da: F(0) if k==1 else da))
    if R is None or R['eta'] is None: continue
    n_checked += 1
    if R['tau'] > 0:
        n_pos += 1
        assert R['eta'] == 0, f"COUNTEREXAMPLE to audit mechanism: {ag} tau={R['tau']} eta={R['eta']}"
    if R['eta'] > 0:
        n_zero += 1
        assert R['tau'] == 0, f"eta>0 with tau>0: {ag}"
print(f"\nSweep: {n_checked} clean random instances; tau>0 in {n_pos} (eta==0 in ALL); "
      f"eta>0 in {n_zero} (tau==0 in ALL). No counterexample to mutual exclusivity.")

# ============ tau=0 regime: eps_bar>0 IS attainable (nuance on Z.0' literal claim) ============
ag0 = {1:(F(10),F(1)), 2:(F(10),F(1)), 3:(F(1,2),F(1))}  # agent 3 worthless on both routes
R0 = analyze(ag0, A=2, d1=F(1), q1=2, d2=F(2), q2=10, dA=dA2)
show("tau=0 witness: (10,1),(10,1),(0.5,1); A=2", R0)
assert R0['tau'] == 0 and R0['eta'] > 0 and R0['eps_bar'] > 0

# ============ Theorem Z.1 Step 1: Myerson payments vs homogeneous edge price ============
# Single edge e*, q=2, d=1; v=alpha-beta; v1,v3~U[0,1] (phi=2v-1), v2~U[0,2] (phi=2v-2).
v1,v2,v3 = F(9,10), F(3,2), F(7,10)
phi = {1: 2*v1-1, 2: 2*v2-2, 3: 2*v3-1}
assert phi == {1:F(4,5), 2:F(1), 3:F(2,5)}
# winners: top-2 positive virtual values -> {1,2}; losing phi = 0.4
p1 = (F(2,5)+1)/2   # phi1(v)=2v-1=0.4
p2 = (F(2,5)+2)/2   # phi2(v)=2v-2=0.4
print(f"\nMyerson instance: phi={ {k: float(x) for k,x in phi.items()} }, winners {{1,2}}, "
      f"critical payments p1={float(p1)}, p2={float(p2)} (unequal by {float(p2-p1)})")
assert p1 == F(7,10) and p2 == F(6,5) and p1 != p2
# Amin (7a) with singleton trips on the single route forces p_m = tau_{e*} for BOTH -> impossible.

# ============ Theorem Z.1 Step 2 saturation bound: |M| <= min-cut => Pr = 0 ============
# |M|=2, min_C cap(C)=5: #{m: alpha_m >= abar} <= 2 < 6 always; Pr[count >= 6] = 0. Arithmetic.
print("\nStep-2 saturation bound: |M|=2, min-cut=5 -> Pr[count>=6] = 0 (count<=2). Trivially verified.")
print("\nALL ASSERTIONS PASSED")
