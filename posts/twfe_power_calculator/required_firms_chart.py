# Recreates the chart in the TWFE power calculator post using the calculator's
# default settings and its closed-form AR(1) formula. Run: python3 required_firms_chart.py
from math import ceil, log10
from statistics import NormalDist

z = NormalDist().inv_cdf
S2, TPRE, TPOST, P, J = 0.25 * (1 - 0.60), 3, 4, 0.10, 21
N_AVAIL = int(15000 * 0.65 * 0.55)

def var_did(rho):
    pre = sum(rho ** abs(a - b) for a in range(TPRE) for b in range(TPRE)) / TPRE**2
    post = sum(rho ** abs(a - b) for a in range(TPOST) for b in range(TPOST)) / TPOST**2
    cross = sum(rho ** ((TPRE - a) + b) for a in range(1, TPRE + 1) for b in range(1, TPOST + 1)) / (TPRE * TPOST)
    return S2 * (pre + post - 2 * cross)

def firms(tau, rho):
    return ceil((z(0.975) + z(0.80)) ** 2 * var_did(rho) / (P * (1 - P) * tau**2)) + J + 4

W, H, L, R, T, B = 760, 420, 78, 24, 46, 58
xs = [x / 10 for x in range(10, 101, 2)]
ymin, ymax = 300, 100000
def px(x): return L + (x - 1) / 9 * (W - L - R)
def py(y): return T + (log10(ymax) - log10(y)) / (log10(ymax) - log10(ymin)) * (H - T - B)
def path(rho): return "M" + " L".join(f"{px(x):.1f},{py(firms(x / 100, rho)):.1f}" for x in xs)

navy, teal, slate, grid = "#1f3a5f", "#2a7f62", "#5b6b7d", "#e3e8ee"
out = [f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" font-family="Source Sans 3, Segoe UI, sans-serif" font-size="13">',
       f'<rect width="{W}" height="{H}" fill="#ffffff"/>',
       f'<text x="{L}" y="24" font-size="16" font-weight="600" fill="{navy}">Firms needed to detect an effect, by size of effect</text>']
for y in [300, 1000, 3000, 10000, 30000, 100000]:
    out.append(f'<line x1="{L}" x2="{W-R}" y1="{py(y):.1f}" y2="{py(y):.1f}" stroke="{grid}"/>')
    out.append(f'<text x="{L-8}" y="{py(y)+4:.1f}" text-anchor="end" fill="{slate}">{y:,}</text>')
for x in range(1, 11):
    out.append(f'<text x="{px(x):.1f}" y="{H-B+20}" text-anchor="middle" fill="{slate}">{x}%</text>')
out.append(f'<text x="{(L+W-R)/2}" y="{H-14}" text-anchor="middle" fill="{slate}">Minimum detectable effect on output (log points, roughly %)</text>')
out.append(f'<line x1="{L}" x2="{W-R}" y1="{py(N_AVAIL):.1f}" y2="{py(N_AVAIL):.1f}" stroke="{teal}" stroke-width="2" stroke-dasharray="6 4"/>')
out.append(f'<text x="{W-R-4}" y="{py(N_AVAIL)-7:.1f}" text-anchor="end" fill="{teal}" font-weight="600">Expected linked panel: {N_AVAIL:,} firms</text>')
out.append(f'<path d="{path(0.0)}" fill="none" stroke="{slate}" stroke-width="2" stroke-dasharray="2 3"/>')
out.append(f'<path d="{path(0.3)}" fill="none" stroke="{navy}" stroke-width="2.5"/>')
out.append(f'<circle cx="{px(2):.1f}" cy="{py(firms(0.02,0.3)):.1f}" r="4.5" fill="{navy}"/>')
out.append(f'<text x="{px(2)+9:.1f}" y="{py(firms(0.02,0.3))-6:.1f}" fill="{navy}">2% effect: {firms(0.02,0.3):,} firms</text>')
lx, ly = px(6.2), py(40000)
out.append(f'<line x1="{lx}" x2="{lx+26}" y1="{ly}" y2="{ly}" stroke="{navy}" stroke-width="2.5"/><text x="{lx+32}" y="{ly+4}" fill="#22303e">Serial correlation of 0.3</text>')
out.append(f'<line x1="{lx}" x2="{lx+26}" y1="{ly+20}" y2="{ly+20}" stroke="{slate}" stroke-width="2" stroke-dasharray="2 3"/><text x="{lx+32}" y="{ly+24}" fill="#22303e">No serial correlation</text>')
out.append('</svg>')
open("required_firms.svg", "w").write("\n".join(out))
print(firms(0.02, 0.3), firms(0.02, 0.0), N_AVAIL)
