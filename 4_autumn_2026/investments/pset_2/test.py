"""Q2: Generate and save the risk/return diagrams for parts (b) and (e)."""
import numpy as np
import matplotlib.pyplot as plt

# ---------- Inputs ----------
rf = 0.04             # Portfolio A (risk-free)
EB, sB = 0.19, 0.25   # Portfolio B
EC, sC = 0.10, 0.15   # Portfolio C


def sharpe(E, s):
    return (E - rf) / s


# ---------- Calculations ----------
# (a) A + B with the same risk as C
w_a = sC / sB
E_a = rf + w_a * (EB - rf)

# (c) MVE of B and C (rho = 0)
zB, zC = (EB - rf) / sB**2, (EC - rf) / sC**2
wB = zB / (zB + zC)
wC = 1 - wB
E_m = wB * EB + wC * EC
s_m = np.sqrt(wB**2 * sB**2 + wC**2 * sC**2)

# (d) A + MVE with the same risk as C
w_d = sC / s_m
E_d = rf + w_d * (E_m - rf)

x = np.array([0, 0.30])   # x-range for the CAL lines


def add_points(ax, points):
    """Plot and label points. Each point: (label, sigma, E, label offset)."""
    for name, s, e, off in points:
        ax.scatter(s, e, zorder=3)
        ax.annotate(name, (s, e), xytext=off, textcoords="offset points")


def format_axes(ax, title):
    ax.set_xlabel("std")
    ax.set_ylabel("expected return")
    ax.set_title(title)
    ax.set_xlim(0, 0.30)
    ax.set_ylim(0, 0.25)
    ax.legend(fontsize=8)


# ---------- Figure (b) ----------
fig, ax = plt.subplots(figsize=(6, 4.5))
ax.plot(x, rf + sharpe(EB, sB) * x, label="AB")
ax.vlines(sC, EC, E_a, linestyles=":", colors="gray")   # P is directly above C
add_points(ax, [
    ("A", 0, rf, (5, -12)),
    ("B", sB, EB, (5, -12)),
    ("C", sC, EC, (5, -12)),
    ("P (a)", sC, E_a, (6, -10)),
])
format_axes(ax, "Risk/return diagram")
fig.savefig("q2b.png", dpi=150, bbox_inches="tight")
plt.close(fig)

# ---------- Figure (e) ----------
w = np.linspace(-0.2, 1.4, 400)                       # weight in B
E_f = w * EB + (1 - w) * EC
s_f = np.sqrt(w**2 * sB**2 + (1 - w)**2 * sC**2)      # rho = 0

fig, ax = plt.subplots(figsize=(6, 4.5))
ax.plot(s_f, E_f, label="BC")
ax.plot(x, rf + sharpe(EB, sB) * x, "--", label="AB")
ax.plot(x, rf + sharpe(E_m, s_m) * x, lw=2.5, label="A + MVE (efficient set)")
add_points(ax, [
    ("A", 0, rf, (5, -12)),
    ("B", sB, EB, (5, -12)),
    ("C", sC, EC, (5, -12)),
    ("MVE", s_m, E_m, (-32, 4)),
    ("P (a)", sC, E_a, (6, -10)),
    ("P (d)", sC, E_d, (6, -6)),
])
format_axes(ax, "Efficient set")
fig.savefig("q2e.png", dpi=150, bbox_inches="tight")
plt.close(fig)

print("Saved q2b.png and q2e.png")