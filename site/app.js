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
const GEOCODAGE = 'https://data.geopf.fr/geocodage';               // IGN Géoplateforme (BAN)
const APICARTO = 'https://apicarto.ign.fr/api/cadastre/parcelle';  // parcelle sous un point
const DEPS_MODELE = ['34'];   // départements pour lesquels un modèle est exporté (prototype)

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
const state = { type: 'A', periode: '12m', dep: null, com: null, sec: null,
                couche: 'obs',          // 'obs' : ventes observées, 'mod' : estimation du modèle
                onglet: 'explorer' };   // 'explorer' | 'estimer'
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

// Propriétés utilisées par le style de carte : v = médiane observée ou estimation
// du modèle selon la couche (-1 : pas de valeur), n
function decorer(fc, niv) {
  for (const f of fc.features) {
    const code = f.properties.code;
    const st = lireStat(stats[niv].get(code), state.type, state.periode);
    const v = state.couche === 'mod' ? valeurModele(niv, code)
      : st && st.med != null ? st.med : null;
    f.properties.v = v != null ? v : -1;
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
  // modèles disponibles : chargés en arrière-plan (couche « Estimation », niveau département)
  Promise.all(DEPS_MODELE.map(chargerModele)).then(rafraichir);
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
  if (geo.dep) map.getSource('dep').setData(decorer(geo.dep, 'dep'));
  const c = state.dep && geo.com.get(state.dep);
  map.getSource('com').setData(c ? decorer(c, 'com') : VIDE);
  const s = state.com && geo.sec.get(state.com);
  map.getSource('sec').setData(s ? decorer(s, 'sec') : VIDE);
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
  try { [fc] = await Promise.all([chargerCommunes(dep), chargerModele(dep)]); }
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
    await estimerSections(com, sec);
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
  map.getCanvas().style.cursor = state.onglet === 'estimer' ? 'crosshair' : 'pointer';
  const code = hit.f.properties.code;
  const nom = hit.niv === 'dep' ? `${noms.dep.get(code)} (${code})`
    : hit.niv === 'com' ? noms.com.get(code) : nomSection(code);
  const st = lireStat(stats[hit.niv].get(code), state.type, state.periode);
  montrerInfobulle(e.originalEvent, nom, st, null, state.couche === 'mod' ? valeurModele(hit.niv, code) : undefined);
});
map.on('mouseout', () => { cacherInfobulle(); });

map.on('click', (e) => {
  if (state.onglet === 'estimer') { placerBien(e.lngLat.lng, e.lngLat.lat); return; }
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
// est : estimation du modèle (couche « Estimation ») ; undefined en couche observée
function montrerInfobulle(ev, titre, st, sousTitre, est) {
  let corps;
  if (!st) corps = '<span class="tt-muted">Aucune vente</span>';
  else if (st.med == null) corps = `<span class="tt-muted">${st.n} vente${st.n > 1 ? 's' : ''} : trop peu pour une statistique</span>`;
  else corps = `<span class="tt-value">${fmtPrix(st.med)}</span><br>
    <span class="tt-muted">${fmtInt.format(st.n)} ventes · ${fmtInt.format(st.q25)} – ${fmtInt.format(st.q75)} €/m²</span>`;
  if (est !== undefined) {
    const obs = st && st.med != null ? `observé : ${fmtPrix(st.med)} (${fmtInt.format(st.n)} ventes)`
      : 'trop peu de ventes observées';
    corps = est == null ? '<span class="tt-muted">Hors zone du modèle</span>'
      : `<span class="tt-value">${fmtPrix(est)}</span><br><span class="tt-muted">estimé pour un ${libelleBienType()}<br>${obs}</span>`;
  }
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
  const mod = state.couche === 'mod';
  $('legend').innerHTML = `
    <div class="legend-title">${mod ? `Prix estimé au m² · ${libelleBienType()}` : `Prix médian au m² · ${TYPE_LABEL[state.type]}`}</div>
    <div class="legend-ramp">${RAMP.map((c) => `<span style="background:${c}"></span>`).join('')}</div>
    <div class="legend-ticks" style="width:${RAMP.length * pas}px">${ticks}</div>
    <div class="legend-nodata"><i></i>${mod ? 'Hors zone du modèle (prototype : Hérault)' : 'Moins de 5 ventes'}</div>`;
}

let indiceForce = null;
function indice(t) { indiceForce = t; $('map-hint').textContent = t; $('map-hint').hidden = false; }
function majIndice() {
  indiceForce = null;
  const h = $('map-hint');
  h.hidden = false;
  if (state.onglet === 'estimer') h.textContent = 'Cliquez sur la carte pour placer le bien';
  else if (!state.dep) h.textContent = 'Cliquez sur un département pour voir ses communes';
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
function radio(groupe, attr, auChoix) {
  const boutons = document.querySelectorAll(`${groupe} button`);
  boutons.forEach((b) => b.addEventListener('click', () => {
    boutons.forEach((x) => x.setAttribute('aria-checked', String(x === b)));
    auChoix(b.dataset[attr]);
  }));
}
function cocher(groupe, attr, valeur) {
  document.querySelectorAll(`${groupe} button`).forEach((x) =>
    x.setAttribute('aria-checked', String(x.dataset[attr] === valeur)));
}
radio('#type-carte', 'type', (t) => { state.type = t; rafraichir(); });
radio('#couche', 'couche', (c) => {
  state.couche = c;
  $('periode').disabled = c === 'mod';   // le modèle donne le prix au dernier semestre
  rafraichir();
});

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

  // estimation du modèle pour la zone (bien type, dernier semestre)
  const niv = state.sec ? 'sec' : state.com ? 'com' : state.dep ? 'dep' : null;
  const code = state.sec || state.com || state.dep;
  const est = niv ? valeurModele(niv, code) : null;
  $('kpi-modele').hidden = est == null;
  if (est != null) {
    const m = modeles.get(niv === 'dep' ? code : depDeCommune(code));
    $('kpi-modele-label').textContent = `Estimation du modèle · ${libelleBienType()} · ${libelleSemestre(m.semestre)}`;
    $('kpi-modele-val').textContent = fmtPrix(est);
  }
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
  if (state.onglet === 'estimer') return;
  const zone = state.sec || state.com || state.dep || '';
  history.replaceState(null, '', `#${state.type}/${state.periode}${zone ? `/${zone}` : ''}`);
}
function lireURL() {
  const [t, p, zone] = location.hash.replace('#', '').split('/');
  if (t === 'estimer') {               // #estimer ou #estimer/lon,lat[/M]
    rafraichir();
    changerOnglet('estimer');
    if (zone === 'M') { estim.type = 'M'; cocher('#estim-type', 'type', 'M'); $('champ-terrain').hidden = false; }
    const [lon, lat] = (p || '').split(',').map(Number);
    if (Number.isFinite(lon) && Number.isFinite(lat)) placerBien(lon, lat);
    return;
  }
  if (t === 'A' || t === 'M') { state.type = t; cocher('#type-carte', 'type', t); }
  if (p && (p === '12m' || annees.includes(p))) { state.periode = p; $('periode').value = p; }
  if (!zone) { rafraichir(); return; }
  if (zone.length === 10) choisirCommune(zone.slice(0, 5)).then(() => choisirSection(zone));
  else if (zone.length === 5) choisirCommune(zone);
  else choisirDepartement(zone);
}

// ---------------------------------------------------------------------------
// Modèle de prix (INLA, exporté par modele/04_export_modele.R)
//
// log(prix/m²) = somme de composantes (moyenne, écart-type a posteriori) :
//   constante + dépendances + f(surface) + f(pièces) [+ f(terrain)] + tendance
//   + champ spatial (interpolé dans le maillage) + effet section + effet immeuble
// Intervalle à 80 % : moyenne ± 1,2816 × k × racine(somme des variances + bruit),
// k calibré sur les ventes des 12 derniers mois (voir l'export).
// ---------------------------------------------------------------------------
const Z80 = 1.2816;
const modeles = new Map();     // dep -> modèle (null si absent)
const effetsCom = new Map();   // commune -> { sec: {A, M}, par: {A, M} }
const estimSec = new Map();    // section -> { A, M } : €/m² du bien type

async function chargerModele(dep) {
  if (!DEPS_MODELE.includes(dep)) return null;
  if (modeles.has(dep)) return modeles.get(dep);
  let m = null;
  try {
    m = (await chargerJSON(`data/modele/${dep}.geojson`)).modele;
    for (const t of ['A', 'M']) {
      preparerMaillage(m[t].mesh);
      const v = Object.values(m[t].communes).sort((a, b) => a - b);
      m[t].dep_med = v[Math.floor(v.length / 2)];     // niveau département : médiane des communes
    }
  } catch (e) { console.warn('Modèle indisponible', dep, e); m = null; }
  modeles.set(dep, m);
  return m;
}

async function chargerEffets(com) {
  if (effetsCom.has(com)) return effetsCom.get(com);
  let e = { sec: {}, par: {} };
  try { e = (await chargerJSON(`data/modele/${depDeCommune(com)}/${com}.geojson`)).modele; } catch { /* aucune vente */ }
  effetsCom.set(com, e);
  return e;
}

const modeleType = (dep, t = state.type) => { const m = modeles.get(dep); return m ? m[t] : null; };
const libelleBienType = (t = state.type) => {
  const mt = DEPS_MODELE.map((d) => modeleType(d, t)).find(Boolean);
  return mt ? mt.libelle_bien_type : TYPE_LABEL[t];
};
const libelleSemestre = (s) => (s ? `${s.endsWith('1') ? '1er' : '2e'} semestre ${s.slice(0, 4)}` : '');

function valeurModele(niv, code) {
  if (niv === 'dep') { const mt = modeleType(code); return mt ? mt.dep_med : null; }
  if (niv === 'com') { const mt = modeleType(depDeCommune(code)); return mt ? mt.communes[code] ?? null : null; }
  const e = estimSec.get(code);
  return e ? e[state.type] : null;
}

// Boîtes englobantes des triangles (recherche du triangle contenant un point)
function preparerMaillage(ms) {
  const bb = new Float64Array(ms.tri.length * 4);
  ms.tri.forEach(([a, b, c], i) => {
    bb[4 * i] = Math.min(ms.lon[a], ms.lon[b], ms.lon[c]);
    bb[4 * i + 1] = Math.min(ms.lat[a], ms.lat[b], ms.lat[c]);
    bb[4 * i + 2] = Math.max(ms.lon[a], ms.lon[b], ms.lon[c]);
    bb[4 * i + 3] = Math.max(ms.lat[a], ms.lat[b], ms.lat[c]);
  });
  ms.bb = bb;
}

// Champ spatial au point (interpolation barycentrique), null hors du maillage
function champAuPoint(mt, lon, lat) {
  const ms = mt.mesh, bb = ms.bb;
  for (let i = 0; i < ms.tri.length; i++) {
    if (lon < bb[4 * i] || lon > bb[4 * i + 2] || lat < bb[4 * i + 1] || lat > bb[4 * i + 3]) continue;
    const [a, b, c] = ms.tri[i];
    const x1 = ms.lon[a], y1 = ms.lat[a], x2 = ms.lon[b], y2 = ms.lat[b], x3 = ms.lon[c], y3 = ms.lat[c];
    const det = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3);
    const l1 = ((y2 - y3) * (lon - x3) + (x3 - x2) * (lat - y3)) / det;
    const l2 = ((y3 - y1) * (lon - x3) + (x1 - x3) * (lat - y3)) / det;
    const l3 = 1 - l1 - l2;
    if (l1 < -1e-9 || l2 < -1e-9 || l3 < -1e-9) continue;
    const ch = mt.champ;
    return { m: l1 * ch.m[a] + l2 * ch.m[b] + l3 * ch.m[c], s: l1 * ch.s[a] + l2 * ch.s[b] + l3 * ch.s[c] };
  }
  return null;
}

function interp1(x, y, v) {
  const n = x.length;
  if (v <= x[0]) return y[0];
  if (v >= x[n - 1]) return y[n - 1];
  let i = 1;
  while (x[i] < v) i++;
  return y[i - 1] + ((v - x[i - 1]) / (x[i] - x[i - 1])) * (y[i] - y[i - 1]);
}

// bien : { surface, pieces, dependance, terrain } ; lieu : { lon, lat, sec, par }
function predire(mt, t, bien, lieu, eff) {
  const ch = champAuPoint(mt, lieu.lon, lieu.lat);
  if (!ch) return null;
  const parts = [mt.b0, [ch.m, ch.s]];
  if (bien.dependance === 1) parts.push(mt.d1);
  else if (bien.dependance >= 2) parts.push(mt.d2);
  const ls = Math.log(bien.surface);
  parts.push([interp1(mt.surf.x, mt.surf.m, ls), interp1(mt.surf.x, mt.surf.s, ls)]);
  const ip = mt.pieces.x.indexOf(Math.min(Math.max(Math.round(bien.pieces), 1), 7));
  parts.push([mt.pieces.m[ip], mt.pieces.s[ip]]);
  if (mt.terrain) {
    const lt = Math.log1p(bien.terrain || 0);
    parts.push([interp1(mt.terrain.x, mt.terrain.m, lt), interp1(mt.terrain.x, mt.terrain.s, lt)]);
  }
  parts.push([mt.tendance.m[mt.t_courant], mt.tendance.s[mt.t_courant]]);
  // effets section et immeuble : estimés s'il y a eu des ventes, sinon 0 ± écart-type du groupe
  const es = lieu.sec && eff && eff.sec && eff.sec[t] && eff.sec[t][lieu.sec];
  const ep = lieu.par && eff && eff.par && eff.par[t] && eff.par[t][lieu.par];
  parts.push(es || [0, mt.sd_sec], ep || [0, mt.sd_par]);
  const mu = parts.reduce((a, p) => a + p[0], 0);
  const v = parts.reduce((a, p) => a + p[1] * p[1], 0) + mt.sd_bruit * mt.sd_bruit;
  return { mu, sd: mt.k * Math.sqrt(v), sec: es ? es[0] : null, par: ep ? ep[0] : null };
}

// Point représentatif d'un polygone : centre de gravité de son plus grand anneau extérieur
function centrePolygone(geom) {
  const polys = geom.type === 'Polygon' ? [geom.coordinates] : geom.coordinates;
  let best = null;
  for (const p of polys) {
    const r = p[0];
    let a = 0, cx = 0, cy = 0;
    for (let i = 0, j = r.length - 1; i < r.length; j = i++) {
      const f = r[j][0] * r[i][1] - r[i][0] * r[j][1];
      a += f; cx += (r[j][0] + r[i][0]) * f; cy += (r[j][1] + r[i][1]) * f;
    }
    if (a !== 0 && (!best || Math.abs(a) > best.a)) best = { a: Math.abs(a), lon: cx / (3 * a), lat: cy / (3 * a) };
  }
  return best;
}

// Couche « Estimation » au niveau section : bien type au centre de la section
async function estimerSections(com, fc) {
  const dep = depDeCommune(com);
  if (!(await chargerModele(dep))) return;
  const eff = await chargerEffets(com);
  for (const f of fc.features) {
    if (!f.geometry) continue;
    const c = centrePolygone(f.geometry);
    if (!c) continue;
    const e = {};
    for (const t of ['A', 'M']) {
      const mt = modeleType(dep, t);
      const p = predire(mt, t, mt.bien_type, { lon: c.lon, lat: c.lat, sec: f.properties.code }, eff);
      e[t] = p ? Math.round(Math.exp(p.mu)) : null;
    }
    estimSec.set(f.properties.code, e);
  }
}

// ---------------------------------------------------------------------------
// Onglets
// ---------------------------------------------------------------------------
function changerOnglet(o) {
  state.onglet = o;
  $('tab-explorer').setAttribute('aria-selected', String(o === 'explorer'));
  $('tab-estimer').setAttribute('aria-selected', String(o === 'estimer'));
  $('vue-explorer').hidden = o !== 'explorer';
  $('vue-estimer').hidden = o !== 'estimer';
  if (marqueur) marqueur.getElement().style.display = o === 'estimer' ? '' : 'none';
  if (o === 'estimer') setTimeout(() => $('adr-input').focus(), 0);
  majIndice();
  if (o === 'explorer') ecrireURL();
  else if (!estim.lieu) history.replaceState(null, '', '#estimer');
}
$('tab-explorer').addEventListener('click', () => changerOnglet('explorer'));
$('tab-estimer').addEventListener('click', () => changerOnglet('estimer'));

// ---------------------------------------------------------------------------
// Estimer un bien
// ---------------------------------------------------------------------------
const estim = { type: 'A', lieu: null };
let marqueur = null;

radio('#estim-type', 'type', (t) => {
  estim.type = t;
  $('champ-terrain').hidden = t !== 'M';
  if (estim.lieu) estimer();
});
for (const id of ['estim-surface', 'estim-pieces', 'estim-dep', 'estim-terrain'])
  $(id).addEventListener('change', () => { if (estim.lieu) estimer(); });
$('estim-form').addEventListener('submit', (e) => { e.preventDefault(); estimer(); });

// Recherche d'adresse (géocodage IGN, Base Adresse Nationale)
const adrInput = $('adr-input'), adrListe = $('adr-results');
let adrMinuterie = null, adrRes = [], adrActif = -1;
adrInput.addEventListener('input', () => {
  clearTimeout(adrMinuterie);
  const q = adrInput.value.trim();
  if (q.length < 4) { adrListe.hidden = true; return; }
  adrMinuterie = setTimeout(() => chercherAdresse(q), 250);
});
adrInput.addEventListener('keydown', (e) => {
  if (adrListe.hidden) return;
  if (e.key === 'ArrowDown') { adrActif = Math.min(adrActif + 1, adrRes.length - 1); marquerAdr(); e.preventDefault(); }
  else if (e.key === 'ArrowUp') { adrActif = Math.max(adrActif - 1, 0); marquerAdr(); e.preventDefault(); }
  else if (e.key === 'Enter') { choisirAdresse(adrRes[Math.max(adrActif, 0)]); e.preventDefault(); }
  else if (e.key === 'Escape') adrListe.hidden = true;
});
document.addEventListener('click', (e) => { if (!e.target.closest('.adresse')) adrListe.hidden = true; });

async function chercherAdresse(q) {
  try {
    const r = await chargerJSON(`${GEOCODAGE}/search?q=${encodeURIComponent(q)}&index=address&autocomplete=1&limit=6`);
    adrRes = r.features || [];
  } catch { adrRes = []; }
  adrActif = adrRes.length ? 0 : -1;
  adrListe.innerHTML = adrRes.length
    ? adrRes.map((f, i) => `<li role="option" data-i="${i}">${f.properties.label}</li>`).join('')
    : '<li class="tt-muted">Aucune adresse trouvée</li>';
  adrListe.querySelectorAll('li[data-i]').forEach((li) =>
    li.addEventListener('click', () => choisirAdresse(adrRes[Number(li.dataset.i)])));
  marquerAdr();
  adrListe.hidden = false;
}
function marquerAdr() {
  adrListe.querySelectorAll('li[data-i]').forEach((li, i) => li.setAttribute('aria-selected', String(i === adrActif)));
}
function choisirAdresse(f) {
  if (!f) return;
  adrListe.hidden = true;
  adrInput.value = f.properties.label;
  const [lon, lat] = f.geometry.coordinates;
  placerBien(lon, lat, { label: f.properties.label, com: f.properties.citycode });
}

// Place le bien : repère, parcelle cadastrale (API Carto), commune, effets du modèle
async function placerBien(lon, lat, { label = null, com = null } = {}) {
  const lieu = { lon, lat, label, com, sec: null, par: null };
  estim.lieu = lieu;
  history.replaceState(null, '', `#estimer/${lon.toFixed(5)},${lat.toFixed(5)}${estim.type === 'M' ? '/M' : ''}`);
  if (!marqueur) {
    marqueur = new maplibregl.Marker({ color: '#0b0b0b', draggable: true }).setLngLat([lon, lat]).addTo(map);
    marqueur.on('dragend', () => { const p = marqueur.getLngLat(); placerBien(p.lng, p.lat); });
  } else marqueur.setLngLat([lon, lat]);
  if (map.getZoom() < 14) map.flyTo({ center: [lon, lat], zoom: 16 });
  else map.easeTo({ center: [lon, lat] });

  afficherLieu('Recherche de la parcelle…');
  const [fp, adr] = await Promise.all([
    parcelleProche(lon, lat),
    label ? null : chargerJSON(`${GEOCODAGE}/reverse?lon=${lon}&lat=${lat}&index=address&limit=1`).catch(() => null),
  ]);
  if (estim.lieu !== lieu) return;           // un autre point a été placé entre-temps
  if (fp) {
    lieu.par = fp.properties.idu;
    lieu.sec = lieu.par.slice(0, 10);
    lieu.com = lieu.par.slice(0, 5);
  }
  const fa = adr && adr.features && adr.features[0];
  if (fa) { lieu.label = `Près du ${fa.properties.label}`; lieu.com = lieu.com || fa.properties.citycode; }
  if (!lieu.label) lieu.label = `${lat.toFixed(5)}, ${lon.toFixed(5)}`;

  if (lieu.com) {
    const dep = depDeCommune(lieu.com);
    await Promise.all([chargerModele(dep), chargerEffets(lieu.com), chargerCommunes(dep).catch(() => null)]);
    // affiche les sections de la commune autour du bien (sans recadrer la carte)
    if (state.com !== lieu.com) choisirCommune(lieu.com, { cadrer: false });
  }
  if (estim.lieu !== lieu) return;
  afficherLieu();
  estimer();
}

// Parcelle cadastrale sous le point ; à défaut (point d'adresse sur la voirie),
// la plus proche dans un carré d'une vingtaine de mètres
async function parcelleProche(lon, lat) {
  const req = (g) => chargerJSON(`${APICARTO}?geom=${encodeURIComponent(JSON.stringify(g))}`)
    .then((r) => (r && r.features) || []).catch(() => []);
  let f = await req({ type: 'Point', coordinates: [lon, lat] });
  if (f.length) return f[0];
  const dx = 0.00025, dy = 0.00018;   // ~20 m
  f = await req({ type: 'Polygon', coordinates: [[[lon - dx, lat - dy], [lon + dx, lat - dy],
    [lon + dx, lat + dy], [lon - dx, lat + dy], [lon - dx, lat - dy]]] });
  if (!f.length) return null;
  // distance du point au sommet le plus proche de chaque parcelle
  const kx = Math.cos((lat * Math.PI) / 180);
  const dist = (feat) => {
    let best = Infinity;
    const visit = (c) => {
      if (typeof c[0] === 'number') best = Math.min(best, ((c[0] - lon) * kx) ** 2 + (c[1] - lat) ** 2);
      else c.forEach(visit);
    };
    visit(feat.geometry.coordinates);
    return best;
  };
  return f.reduce((a, b) => (dist(b) < dist(a) ? b : a));
}

function afficherLieu(attente) {
  const el = $('estim-lieu'), l = estim.lieu;
  el.hidden = false;
  if (attente) { el.innerHTML = `<span class="tt-muted">${attente}</span>`; return; }
  const parc = l.par
    ? `Parcelle ${l.par.slice(10).replace(/^0+/, '')} · ${nomSection(l.sec).replace('Section', 'section')} · ${noms.com.get(l.com) || l.com}`
    : 'Parcelle cadastrale introuvable à cet endroit (voirie ?) : estimation sans effet du quartier ni de l\'immeuble';
  el.innerHTML = `<strong>${l.label}</strong><span class="tt-muted">${parc}</span>`;
}

function lireBien() {
  const num = (id) => Number($(id).value);
  return {
    surface: num('estim-surface'), pieces: num('estim-pieces'),
    dependance: num('estim-dep'), terrain: estim.type === 'M' ? num('estim-terrain') : 0,
  };
}

function estimer() {
  const out = $('estim-result');
  const l = estim.lieu;
  if (!l) { out.innerHTML = '<p class="estim-msg">Indiquez une adresse ou cliquez sur la carte.</p>'; return; }
  const bien = lireBien();
  if (!(bien.surface >= 9 && bien.surface <= 500)) {
    out.innerHTML = '<p class="estim-msg">La surface doit être comprise entre 9 et 500 m².</p>'; return;
  }
  if (estim.type === 'M' && !(bien.terrain >= 0 && bien.terrain <= 20000)) {
    out.innerHTML = '<p class="estim-msg">Le terrain doit être compris entre 0 et 20 000 m².</p>'; return;
  }
  const dep = l.com ? depDeCommune(l.com) : null;
  const m = dep && modeles.get(dep);
  if (!m) {
    out.innerHTML = `<p class="estim-msg">Pas encore de modèle pour ce département${dep ? ` (${dep})` : ''}. Le prototype couvre l'Hérault (34).</p>`;
    return;
  }
  const mt = m[estim.type];
  const p = predire(mt, estim.type, bien, l, effetsCom.get(l.com));
  if (!p) { out.innerHTML = '<p class="estim-msg">Ce point est en dehors de la zone couverte par le modèle.</p>'; return; }

  const pm2 = Math.exp(p.mu), lo = Math.exp(p.mu - Z80 * p.sd), hi = Math.exp(p.mu + Z80 * p.sd);
  const arrondi = (v) => Math.round(v / 1000) * 1000;
  const fmtE = (v) => `${fmtInt.format(arrondi(v))} €`;
  const pct = (e) => `${e >= 0 ? '+' : '−'}${fmtInt.format(Math.abs(100 * (Math.exp(e) - 1)))} %`;
  const facteurs = [];
  if (p.sec != null) facteurs.push(`<li><span>Section cadastrale</span><b>${pct(p.sec)}</b><em>par rapport au prix attendu à cet endroit</em></li>`);
  if (p.par != null) facteurs.push(`<li><span>${estim.type === 'A' ? 'Immeuble' : 'Parcelle'}</span><b>${pct(p.par)}</b><em>d'après les ventes passées sur la même parcelle</em></li>`);
  else if (l.par) facteurs.push(`<li><span>${estim.type === 'A' ? 'Immeuble' : 'Parcelle'}</span><b>–</b><em>aucune vente connue sur cette parcelle : fourchette plus large</em></li>`);
  out.innerHTML = `
    <div class="kpi kpi-main">
      <span class="kpi-label">Valeur estimée · ${estim.type === 'A' ? 'appartement' : 'maison'} de ${fmtInt.format(bien.surface)} m²</span>
      <span class="kpi-value">${fmtE(pm2 * bien.surface)}</span>
      <span class="kpi-sub">soit ${fmtInt.format(Math.round(pm2 / 10) * 10)} €/m²</span>
    </div>
    <div class="fourchette">
      <div class="fourchette-titre">8 chances sur 10 entre</div>
      <div class="fourchette-barre" aria-hidden="true"><span class="fourchette-point"></span></div>
      <div class="fourchette-bornes"><b>${fmtE(lo * bien.surface)}</b><b>${fmtE(hi * bien.surface)}</b></div>
      <div class="fourchette-bornes tt-muted"><span>${fmtInt.format(Math.round(lo / 10) * 10)} €/m²</span><span>${fmtInt.format(Math.round(hi / 10) * 10)} €/m²</span></div>
    </div>
    ${facteurs.length ? `<ul class="facteurs">${facteurs.join('')}</ul>` : ''}
    <p class="kpi-note">Prix estimé au ${libelleSemestre(m.semestre)} (ventes jusqu'au ${m.fin.split('-').reverse().join('/')}).</p>`;
}
