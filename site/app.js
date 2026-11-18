/* Prix du m² — prototype de site statique
 * Données : site/data/*.geojson produits par pipeline/05_export_site.R
 * Sections cadastrales : contours chargés à la demande depuis cadastre.data.gouv.fr
 */
'use strict';

// ---------------------------------------------------------------------------
// Constantes
// ---------------------------------------------------------------------------
const BASEMAP = 'https://tiles.openfreemap.org/styles/positron';
const CADASTRE = (com) => `https://cadastre.data.gouv.fr/bundler/cadastre-etalab/communes/${com}/geojson/sections`;
const GEO_API = 'https://geo.api.gouv.fr/communes';

// Classes de prix fixes (identiques à tous les niveaux, pour comparer)
const BINS = [1000, 1500, 2000, 2500, 3000, 3500, 4000, 5000, 6000, 8000, 10000];
// Rampe séquentielle bleue (une teinte, clair -> foncé), 12 classes
const RAMP = ['#cde2fb', '#b7d3f6', '#9ec5f4', '#86b6ef', '#6da7ec', '#5598e7',
              '#3987e5', '#2a78d6', '#256abf', '#1c5cab', '#184f95', '#104281'];
const NODATA = '#d9d8d3';
const TYPE_LABEL = { A: 'appartements', M: 'maisons' };
const PLM = { '75056': '751', '69123': '6938', '13055': '132' }; // Paris, Lyon, Marseille

const fmtInt = new Intl.NumberFormat('fr-FR', { maximumFractionDigits: 0 });
const fmtPrix = (v) => (v == null ? '–' : `${fmtInt.format(Math.round(v))} €/m²`);
const MOIS = ['janv.', 'févr.', 'mars', 'avr.', 'mai', 'juin', 'juil.', 'août', 'sept.', 'oct.', 'nov.', 'déc.'];

// ---------------------------------------------------------------------------
// État
// ---------------------------------------------------------------------------
const state = { type: 'A', periode: '12m', dep: null, com: null, sec: null };
const stats = { FR: null, dep: new Map(), com: new Map(), sec: new Map() };
const noms = { dep: new Map(), com: new Map() };
const geo = { dep: null, com: new Map(), sec: new Map() };   // FeatureCollections
const charges = { secStats: new Set() };
let finDonnees = null;    // 'YYYY-MM-DD'
let annees = [];          // années disponibles (chaînes)

// ---------------------------------------------------------------------------
// Utilitaires
// ---------------------------------------------------------------------------
const $ = (id) => document.getElementById(id);
const depDeCommune = (c) => (c.startsWith('97') ? c.slice(0, 3) : c.slice(0, 2));

function lireStat(s, type, periode) {
  const a = s && s[type] && s[type][periode];
  if (!a) return null;
  return { n: a[0], q05: a[1], q25: a[2], med: a[3], q75: a[4], q95: a[5] };
}

async function chargerJSON(url) {
  const r = await fetch(url);
  if (!r.ok) throw new Error(`${r.status} ${url}`);
  return r.json();
}

function bbox(features) {
  let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
  const visit = (c) => {
    if (typeof c[0] === 'number') {
      if (c[0] < x0) x0 = c[0]; if (c[0] > x1) x1 = c[0];
      if (c[1] < y0) y0 = c[1]; if (c[1] > y1) y1 = c[1];
    } else c.forEach(visit);
  };
  features.forEach((f) => f.geometry && visit(f.geometry.coordinates));
  return [[x0, y0], [x1, y1]];
}

function libellePeriode(p, court = false) {
  if (p === '12m') {
    if (!finDonnees) return '12 derniers mois';
    const [y, m] = finDonnees.split('-').map(Number);
    const debM = m % 12, debY = m === 12 ? y : y - 1;
    return court ? '12 derniers mois'
      : `12 derniers mois (${MOIS[debM]} ${debY} – ${MOIS[m - 1]} ${y})`;
  }
  if (finDonnees && p === finDonnees.slice(0, 4) && finDonnees.slice(5, 7) !== '12') {
    const m = Number(finDonnees.slice(5, 7));
    return court ? `${p} (partiel)` : `${p} (janv. – ${MOIS[m - 1]})`;
  }
  return p;
}

function nomSection(code) {
  const prefixe = code.slice(5, 8), sec = code.slice(8).replace(/^0/, '');
  return prefixe === '000' ? `Section ${sec}` : `Section ${sec} (${prefixe})`;
}

// Propriétés utilisées par le style de carte : v = médiane (ou -1), n
function decorer(fc, statsMap) {
  for (const f of fc.features) {
    const st = lireStat(statsMap.get(f.properties.code), state.type, state.periode);
    f.properties.v = st && st.med != null ? st.med : -1;
    f.properties.n = st ? st.n : 0;
  }
  return fc;
}

// ---------------------------------------------------------------------------
// Carte
// ---------------------------------------------------------------------------
const couleur = ['case', ['<', ['get', 'v'], 0], NODATA,
  ['step', ['get', 'v'], RAMP[0], ...BINS.flatMap((b, i) => [b, RAMP[i + 1]])]];

const map = new maplibregl.Map({
  container: 'map', style: BASEMAP, center: [2.4, 46.6], zoom: 4.8, minZoom: 3,
  attributionControl: { compact: true },
});
map.addControl(new maplibregl.NavigationControl({ showCompass: false }), 'top-right');

const VIDE = { type: 'FeatureCollection', features: [] };
const NIVEAUX = ['sec', 'com', 'dep'];            // ordre de priorité au survol / clic
let survol = null;                                  // {source, id}

function ajouterNiveau(id, avant, opacite) {
  map.addSource(id, { type: 'geojson', data: VIDE, promoteId: 'code' });
  map.addLayer({ id: `${id}-fill`, type: 'fill', source: id,
    paint: { 'fill-color': couleur, 'fill-opacity': opacite } }, avant);
  map.addLayer({ id: `${id}-line`, type: 'line', source: id,
    paint: {
      'line-color': ['case', ['boolean', ['feature-state', 'hover'], false], '#0b0b0b', '#ffffff'],
      'line-width': ['case', ['boolean', ['feature-state', 'hover'], false], 2, id === 'dep' ? 1 : 0.6],
      'line-opacity': 0.9,
    } }, avant);
  map.addLayer({ id: `${id}-sel`, type: 'line', source: id, filter: ['==', ['get', 'code'], ''],
    paint: { 'line-color': '#0b0b0b', 'line-width': 2.5 } }, avant);
}

// Les données initiales se chargent en parallèle de la carte : le panneau
// s'affiche même si la carte (WebGL, fond de carte) tarde ou échoue.
let cartePrete = false;
const donneesInitiales = (async () => {
  const [fr, dep] = await Promise.all([
    chargerJSON('data/france.geojson'), chargerJSON('data/departements.geojson')]);
  const pfr = fr.features[0].properties;
  stats.FR = pfr.s; finDonnees = pfr.fin;
  $('fin-donnees').textContent = `${MOIS[Number(finDonnees.slice(5, 7)) - 1]} ${finDonnees.slice(0, 4)}`;
  annees = Object.keys(pfr.s.A).filter((k) => k !== '12m').sort();
  for (const f of dep.features) {
    stats.dep.set(f.properties.code, f.properties.s);
    noms.dep.set(f.properties.code, f.properties.nom);
    delete f.properties.s;
  }
  geo.dep = dep;
  remplirPeriodes();
  lireURL();
})();
donneesInitiales.catch((e) => { console.error(e); indice('Impossible de charger les données'); });

map.on('error', (e) => console.error('Carte :', e.error || e));
map.on('load', async () => {
  const premierSymbole = map.getStyle().layers.find((l) => l.type === 'symbol');
  const avant = premierSymbole && premierSymbole.id;
  ajouterNiveau('dep', avant, 0.78);
  ajouterNiveau('com', avant, 0.8);
  ajouterNiveau('sec', avant, 0.82);
  cartePrete = true;
  await donneesInitiales;
  rafraichir();
});

function rafraichirSources() {
  if (!cartePrete) return;
  if (geo.dep) map.getSource('dep').setData(decorer(geo.dep, stats.dep));
  const c = state.dep && geo.com.get(state.dep);
  map.getSource('com').setData(c ? decorer(c, stats.com) : VIDE);
  const s = state.com && geo.sec.get(state.com);
  map.getSource('sec').setData(s ? decorer(s, stats.sec) : VIDE);
  // la zone sélectionnée laisse voir le niveau inférieur
  map.setFilter('dep-fill', state.dep ? ['!=', ['get', 'code'], state.dep] : null);
  map.setFilter('com-fill', state.com ? ['!=', ['get', 'code'], state.com] : null);
  map.setFilter('dep-sel', ['==', ['get', 'code'], state.dep || '']);
  map.setFilter('com-sel', ['==', ['get', 'code'], state.com || '']);
  map.setFilter('sec-sel', ['==', ['get', 'code'], state.sec || '']);
}

function rafraichir() {
  rafraichirSources();
  dessinerLegende();
  majPanneau();
  majIndice();
  ecrireURL();
}

// --- Navigation -------------------------------------------------------------
async function chargerCommunes(dep) {
  if (geo.com.has(dep)) return geo.com.get(dep);
  const fc = await chargerJSON(`data/communes/${dep}.geojson`);
  for (const f of fc.features) {
    stats.com.set(f.properties.code, f.properties.s);
    noms.com.set(f.properties.code, f.properties.nom);
    delete f.properties.s;
  }
  geo.com.set(dep, fc);
  return fc;
}

async function chargerStatsSections(dep) {
  if (charges.secStats.has(dep)) return;
  const fc = await chargerJSON(`data/sections/${dep}.geojson`);
  for (const f of fc.features) stats.sec.set(f.properties.code, f.properties.s);
  charges.secStats.add(dep);
}

async function choisirDepartement(dep, { cadrer = true, cadrerSur = null } = {}) {
  indice('Chargement des communes…');
  state.dep = dep; state.com = null; state.sec = null;
  let fc;
  try { fc = await chargerCommunes(dep); }
  catch (e) { indice('Données indisponibles pour ce département'); state.dep = null; rafraichir(); return; }
  rafraichir();
  if (cadrer) {
    const feats = cadrerSur ? fc.features.filter((f) => f.properties.code.startsWith(cadrerSur)) : fc.features;
    map.fitBounds(bbox(feats), { padding: 40, maxZoom: 12 });
  }
}

async function choisirCommune(com, { cadrer = true } = {}) {
  const dep = depDeCommune(com);
  if (state.dep !== dep) await choisirDepartement(dep, { cadrer: false });
  state.com = com; state.sec = null;
  rafraichir();
  const fcCom = geo.com.get(dep);
  const fCom = fcCom && fcCom.features.find((f) => f.properties.code === com);
  if (cadrer && fCom) map.fitBounds(bbox([fCom]), { padding: 40, maxZoom: 15 });

  indice('Chargement des sections cadastrales…');
  try {
    const [sec] = await Promise.all([
      geo.sec.has(com) ? geo.sec.get(com) : chargerJSON(CADASTRE(com)),
      chargerStatsSections(dep)]);
    for (const f of sec.features) f.properties = { code: f.properties.id };
    geo.sec.set(com, sec);
  } catch (e) {
    indice('Sections cadastrales indisponibles pour cette commune');
    return;
  }
  if (state.com === com) rafraichir();
}

function choisirSection(sec) { state.sec = sec; rafraichir(); }

function allerFrance() {
  state.dep = state.com = state.sec = null;
  rafraichir();
  map.fitBounds([[-5.2, 41.3], [9.6, 51.1]], { padding: 20 });
}

// --- Survol et clic -----------------------------------------------------------
function entiteSous(point) {
  const couches = NIVEAUX.map((n) => `${n}-fill`).filter((l) => map.getLayer(l));
  const feats = map.queryRenderedFeatures(point, { layers: couches });
  for (const niv of NIVEAUX) {
    const f = feats.find((x) => x.layer.id === `${niv}-fill`);
    if (f) return { niv, f };
  }
  return null;
}

map.on('mousemove', (e) => {
  const hit = entiteSous(e.point);
  if (survol) map.setFeatureState(survol, { hover: false });
  survol = null;
  if (!hit) { cacherInfobulle(); map.getCanvas().style.cursor = ''; return; }
  survol = { source: hit.niv, id: hit.f.properties.code };
  map.setFeatureState(survol, { hover: true });
  map.getCanvas().style.cursor = 'pointer';
  const code = hit.f.properties.code;
  const nom = hit.niv === 'dep' ? `${noms.dep.get(code)} (${code})`
    : hit.niv === 'com' ? noms.com.get(code) : nomSection(code);
  const st = lireStat(stats[hit.niv].get(code), state.type, state.periode);
  montrerInfobulle(e.originalEvent, nom, st);
});
map.on('mouseout', () => { cacherInfobulle(); });

map.on('click', (e) => {
  const hit = entiteSous(e.point);
  if (!hit) return;
  const code = hit.f.properties.code;
  if (hit.niv === 'dep' && code !== state.dep) choisirDepartement(code);
  else if (hit.niv === 'com' && code !== state.com) choisirCommune(code);
  else if (hit.niv === 'sec') choisirSection(code);
});

// ---------------------------------------------------------------------------
// Infobulle (carte et graphique)
// ---------------------------------------------------------------------------
const tip = $('tooltip');
function montrerInfobulle(ev, titre, st, sousTitre) {
  let corps;
  if (!st) corps = '<span class="tt-muted">Aucune vente</span>';
  else if (st.med == null) corps = `<span class="tt-muted">${st.n} vente${st.n > 1 ? 's' : ''} : trop peu pour une statistique</span>`;
  else corps = `<span class="tt-value">${fmtPrix(st.med)}</span><br>
    <span class="tt-muted">${fmtInt.format(st.n)} ventes · ${fmtInt.format(st.q25)} – ${fmtInt.format(st.q75)} €/m²</span>`;
  tip.innerHTML = `<strong>${titre}</strong>${sousTitre ? `<span class="tt-muted">${sousTitre}</span><br>` : ''}${corps}`;
  tip.hidden = false;
  const w = tip.offsetWidth, h = tip.offsetHeight;
  let x = ev.clientX + 14, y = ev.clientY + 14;
  if (x + w > window.innerWidth - 8) x = ev.clientX - w - 14;
  if (y + h > window.innerHeight - 8) y = ev.clientY - h - 14;
  tip.style.left = `${x}px`; tip.style.top = `${y}px`;
}
function cacherInfobulle() { tip.hidden = true; }

// ---------------------------------------------------------------------------
// Légende, indice, filtres
// ---------------------------------------------------------------------------
function dessinerLegende() {
  const pas = 27;
  const ticks = [1000, 2000, 3000, 4000, 6000, 10000]
    .map((b) => `<span style="left:${(BINS.indexOf(b) + 1) * pas - 1}px">${fmtInt.format(b)}</span>`).join('');
  $('legend').innerHTML = `
    <div class="legend-title">Prix médian au m² · ${TYPE_LABEL[state.type]}</div>
    <div class="legend-ramp">${RAMP.map((c) => `<span style="background:${c}"></span>`).join('')}</div>
    <div class="legend-ticks" style="width:${RAMP.length * pas}px">${ticks}</div>
    <div class="legend-nodata"><i></i>Moins de 5 ventes</div>`;
}

let indiceForce = null;
function indice(t) { indiceForce = t; $('map-hint').textContent = t; $('map-hint').hidden = false; }
function majIndice() {
  indiceForce = null;
  const h = $('map-hint');
  h.hidden = false;
  if (!state.dep) h.textContent = 'Cliquez sur un département pour voir ses communes';
  else if (!state.com) h.textContent = 'Cliquez sur une commune pour voir ses sections cadastrales';
  else if (!state.sec) h.textContent = 'Cliquez sur une section cadastrale';
  else h.hidden = true;
}

function remplirPeriodes() {
  const sel = $('periode');
  sel.innerHTML = ['12m', ...annees.slice().reverse()]
    .map((p) => `<option value="${p}">${libellePeriode(p)}</option>`).join('');
  sel.value = state.periode;
}
$('periode').addEventListener('change', (e) => { state.periode = e.target.value; rafraichir(); });
document.querySelectorAll('.segmented button').forEach((b) => b.addEventListener('click', () => {
  state.type = b.dataset.type;
  document.querySelectorAll('.segmented button').forEach((x) =>
    x.setAttribute('aria-checked', String(x === b)));
  rafraichir();
}));

// ---------------------------------------------------------------------------
// Panneau
// ---------------------------------------------------------------------------
function zoneCourante() {
  if (state.sec) return { nom: nomSection(state.sec), s: stats.sec.get(state.sec) };
  if (state.com) return { nom: noms.com.get(state.com) || state.com, s: stats.com.get(state.com) };
  if (state.dep) return { nom: `${noms.dep.get(state.dep)} (${state.dep})`, s: stats.dep.get(state.dep) };
  return { nom: 'France', s: stats.FR };
}

function majPanneau() {
  // Fil d'Ariane
  const parts = [{ label: 'France', go: allerFrance }];
  if (state.dep) parts.push({ label: noms.dep.get(state.dep), go: () => choisirDepartement(state.dep) });
  if (state.com) parts.push({ label: noms.com.get(state.com) || state.com, go: () => choisirCommune(state.com) });
  if (state.sec) parts.push({ label: nomSection(state.sec) });
  const nav = $('crumbs'); nav.innerHTML = '';
  parts.forEach((p, i) => {
    if (i) nav.insertAdjacentHTML('beforeend', '<span class="sep">›</span>');
    if (i === parts.length - 1) nav.insertAdjacentHTML('beforeend', `<span class="current">${p.label}</span>`);
    else { const b = document.createElement('button'); b.textContent = p.label; b.onclick = p.go; nav.append(b); }
  });

  const z = zoneCourante();
  $('zone-nom').textContent = z.nom;
  const st = lireStat(z.s, state.type, state.periode);
  $('kpi-med').textContent = st && st.med != null ? fmtPrix(st.med) : '–';
  $('kpi-iq').textContent = st && st.q25 != null ? `${fmtInt.format(st.q25)} – ${fmtInt.format(st.q75)} €/m²` : '–';
  $('kpi-n').textContent = st ? fmtInt.format(st.n) : '0';
  $('kpi-note').textContent = `${TYPE_LABEL[state.type][0].toUpperCase() + TYPE_LABEL[state.type].slice(1)} · ${libellePeriode(state.periode)}`
    + (st && st.med == null ? ' · trop peu de ventes pour une statistique fiable' : '')
    + (!st ? ' · aucune vente sur la période' : '');
  $('chart-title').textContent = `Évolution du prix médian au m² · ${TYPE_LABEL[state.type]}`;
  dessinerGraphique(z);
}

// ---------------------------------------------------------------------------
// Graphique d'évolution (SVG) : médiane + bande Q1–Q3, infobulle au survol
// ---------------------------------------------------------------------------
function dessinerGraphique(z) {
  const el = $('chart');
  const pts = annees.map((a) => ({ a, st: lireStat(z.s, state.type, a) }))
    .map((d) => ({ ...d, ok: d.st && d.st.med != null }));
  const valides = pts.filter((d) => d.ok);
  tableau(pts);
  if (valides.length < 2) {
    el.innerHTML = '<div class="chart-empty">Pas assez de ventes pour tracer une évolution</div>';
    return;
  }
  const W = el.clientWidth || 360, H = el.clientHeight || 220;
  const m = { l: 46, r: 10, t: 10, b: 22 };
  const lo = Math.min(...valides.map((d) => d.st.q25)), hi = Math.max(...valides.map((d) => d.st.q75));
  const pasY = niceStep((hi - lo) / 4);
  const y0 = Math.max(0, Math.floor(lo / pasY) * pasY), y1 = Math.ceil(hi / pasY) * pasY;
  const x = (i) => m.l + (i * (W - m.l - m.r)) / (annees.length - 1);
  const y = (v) => m.t + ((y1 - v) * (H - m.t - m.b)) / (y1 - y0);

  // segments continus (une année sans statistique coupe la courbe)
  const segs = []; let cur = [];
  pts.forEach((d, i) => { if (d.ok) cur.push({ ...d, i }); else if (cur.length) { segs.push(cur); cur = []; } });
  if (cur.length) segs.push(cur);

  const partielle = finDonnees && finDonnees.slice(5, 7) !== '12' ? finDonnees.slice(0, 4) : null;
  let svg = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Évolution du prix médian au m² de ${z.nom}">`;
  svg += '<g class="grid axis">';
  for (let v = y0; v <= y1 + 1e-9; v += pasY)
    svg += `<line x1="${m.l}" x2="${W - m.r}" y1="${y(v)}" y2="${y(v)}"/><text x="${m.l - 6}" y="${y(v) + 3.5}" text-anchor="end">${fmtInt.format(v)}</text>`;
  pts.forEach((d, i) => {
    if (annees.length > 8 && i % 2 && i !== annees.length - 1) return;
    const lab = d.a === partielle ? `${d.a}*` : d.a;
    svg += `<text x="${x(i)}" y="${H - 6}" text-anchor="middle">${lab}</text>`;
  });
  svg += '</g>';
  for (const s of segs) {
    if (s.length > 1) {
      const haut = s.map((d) => `${x(d.i)},${y(d.st.q75)}`).join(' L');
      const bas = s.slice().reverse().map((d) => `${x(d.i)},${y(d.st.q25)}`).join(' L');
      svg += `<path class="band" d="M${haut} L${bas} Z"/>`;
      const plein = s.filter((d) => d.a !== partielle);
      if (plein.length > 1) svg += `<path class="line" d="M${plein.map((d) => `${x(d.i)},${y(d.st.med)}`).join(' L')}"/>`;
      const last = s[s.length - 1];
      if (last.a === partielle && s.length > 1) {
        const prev = s[s.length - 2];
        svg += `<path class="line partial" d="M${x(prev.i)},${y(prev.st.med)} L${x(last.i)},${y(last.st.med)}"/>`;
      }
    }
  }
  for (const d of valides) {
    const i = annees.indexOf(d.a), sel = d.a === state.periode;
    svg += `<circle class="dot" cx="${x(i)}" cy="${y(d.st.med)}" r="${sel ? 5.5 : 4}"/>`;
  }
  svg += `<line class="cross" id="cross" x1="0" x2="0" y1="${m.t}" y2="${H - m.b}" visibility="hidden"/>`;
  svg += `<rect id="chart-hit" x="${m.l - 10}" y="0" width="${W - m.l - m.r + 20}" height="${H}" fill="transparent"/>`;
  svg += '</svg>';
  const note = partielle ? `<div class="kpi-note">Bande : la moitié des ventes (entre le 1er et le 3e quartile). * ${libellePeriode(partielle)}</div>`
    : '<div class="kpi-note">Bande : la moitié des ventes (entre le 1er et le 3e quartile).</div>';
  el.innerHTML = svg;
  const old = el.parentElement.querySelector('.chart-note'); if (old) old.remove();
  el.insertAdjacentHTML('afterend', note.replace('class="kpi-note"', 'class="kpi-note chart-note"'));

  const hit = el.querySelector('#chart-hit'), cross = el.querySelector('#cross');
  hit.addEventListener('mousemove', (ev) => {
    const r = el.getBoundingClientRect();
    const px = ((ev.clientX - r.left) * W) / r.width;
    const i = Math.max(0, Math.min(annees.length - 1, Math.round(((px - m.l) * (annees.length - 1)) / (W - m.l - m.r))));
    cross.setAttribute('x1', x(i)); cross.setAttribute('x2', x(i)); cross.setAttribute('visibility', 'visible');
    montrerInfobulle(ev, libellePeriode(annees[i]), pts[i].st);
  });
  hit.addEventListener('mouseleave', () => { cross.setAttribute('visibility', 'hidden'); cacherInfobulle(); });
}

function niceStep(raw) {
  const p = Math.pow(10, Math.floor(Math.log10(raw || 1)));
  const r = raw / p;
  return (r <= 1 ? 1 : r <= 2 ? 2 : r <= 2.5 ? 2.5 : r <= 5 ? 5 : 10) * p;
}

function tableau(pts) {
  const lignes = pts.slice().reverse().map((d) => {
    const st = d.st;
    const c = (v) => (v == null ? '–' : fmtInt.format(v));
    return `<tr><td>${libellePeriode(d.a, true)}</td><td>${st ? c(st.n) : '0'}</td><td>${st ? c(st.q25) : '–'}</td><td>${st ? c(st.med) : '–'}</td><td>${st ? c(st.q75) : '–'}</td></tr>`;
  }).join('');
  $('table').innerHTML = `<table><thead><tr><th>Année</th><th>Ventes</th><th>Q1</th><th>Médiane</th><th>Q3</th></tr></thead><tbody>${lignes}</tbody></table>`;
}

window.addEventListener('resize', () => { if (stats.FR) dessinerGraphique(zoneCourante()); });

// ---------------------------------------------------------------------------
// Recherche de commune (API Géo)
// ---------------------------------------------------------------------------
const input = $('search-input'), liste = $('search-results');
let minuterie = null, resultats = [], actif = -1;

input.addEventListener('input', () => {
  clearTimeout(minuterie);
  const q = input.value.trim();
  if (q.length < 2) { liste.hidden = true; return; }
  minuterie = setTimeout(() => rechercher(q), 200);
});
input.addEventListener('keydown', (e) => {
  if (liste.hidden) return;
  if (e.key === 'ArrowDown') { actif = Math.min(actif + 1, resultats.length - 1); marquer(); e.preventDefault(); }
  else if (e.key === 'ArrowUp') { actif = Math.max(actif - 1, 0); marquer(); e.preventDefault(); }
  else if (e.key === 'Enter') { choisirResultat(resultats[Math.max(actif, 0)]); e.preventDefault(); }
  else if (e.key === 'Escape') liste.hidden = true;
});
document.addEventListener('click', (e) => { if (!e.target.closest('.search')) liste.hidden = true; });

async function rechercher(q) {
  const param = /^\d{5}$/.test(q) ? `codePostal=${q}` : `nom=${encodeURIComponent(q)}`;
  try {
    resultats = await chargerJSON(`${GEO_API}?${param}&fields=code,nom,codeDepartement&boost=population&limit=8`);
  } catch { resultats = []; }
  actif = resultats.length ? 0 : -1;
  liste.innerHTML = resultats.length
    ? resultats.map((r, i) => `<li role="option" data-i="${i}">${r.nom}<span class="dep">${r.codeDepartement}</span></li>`).join('')
    : '<li class="tt-muted">Aucune commune trouvée</li>';
  liste.querySelectorAll('li[data-i]').forEach((li) =>
    li.addEventListener('click', () => choisirResultat(resultats[Number(li.dataset.i)])));
  marquer();
  liste.hidden = false;
}
function marquer() {
  liste.querySelectorAll('li[data-i]').forEach((li, i) => li.setAttribute('aria-selected', String(i === actif)));
}
function choisirResultat(r) {
  if (!r) return;
  liste.hidden = true; input.value = r.nom; input.blur();
  if (PLM[r.code]) choisirDepartement(r.codeDepartement, { cadrerSur: PLM[r.code] });
  else choisirCommune(r.code);
}

// ---------------------------------------------------------------------------
// URL partageable : #A/12m/34172
// ---------------------------------------------------------------------------
function ecrireURL() {
  const zone = state.sec || state.com || state.dep || '';
  history.replaceState(null, '', `#${state.type}/${state.periode}${zone ? `/${zone}` : ''}`);
}
function lireURL() {
  const [t, p, zone] = location.hash.replace('#', '').split('/');
  if (t === 'A' || t === 'M') {
    state.type = t;
    document.querySelectorAll('.segmented button').forEach((x) => x.setAttribute('aria-checked', String(x.dataset.type === t)));
  }
  if (p && (p === '12m' || annees.includes(p))) { state.periode = p; $('periode').value = p; }
  if (!zone) { rafraichir(); return; }
  if (zone.length === 10) choisirCommune(zone.slice(0, 5)).then(() => choisirSection(zone));
  else if (zone.length === 5) choisirCommune(zone);
  else choisirDepartement(zone);
}
