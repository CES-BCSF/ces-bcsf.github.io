// CES · BCSF — prototipo: cinta de series, contenido dinamico (JSON) y buscador
//
// window.CES_ROOT   debe apuntar a la raiz del repo desde la pagina actual
// window.CES_CATEGORY  (solo paginas de categoria) clave dentro de "categories" en el JSON

const CES_DATA_URL = () => (window.CES_ROOT || "") + "assets/data/site-content.json";

function resolveUrl(url) {
  if (/^https?:\/\//.test(url)) return url;
  return (window.CES_ROOT || "") + url;
}

function formatDate(iso) {
  const d = new Date(iso);
  const datePart = d.toLocaleDateString("es-AR", { day: "2-digit", month: "short", year: "numeric" });
  const timePart = d.toLocaleTimeString("es-AR", { hour: "2-digit", minute: "2-digit" });
  return `${datePart.replace(".", "")} · ${timePart}`;
}

function buildRibbons() {
  document.querySelectorAll(".ces-ribbon").forEach((ribbon) => {
    const bars = 60;
    let html = "";
    for (let i = 0; i < bars; i++) {
      const envelope = Math.sin((i / bars) * Math.PI);
      const noise = Math.random();
      const height = Math.round(15 + envelope * 60 * (0.4 + noise * 0.6));
      const cls = i % 7 === 0 ? "grain" : i % 3 === 0 ? "forest" : "";
      html += `<span class="${cls}" style="height:${height}%"></span>`;
    }
    ribbon.innerHTML = html;
  });
}

function setupSearch(inputId, listId, emptyId) {
  const input = document.getElementById(inputId);
  const list = document.getElementById(listId);
  const empty = document.getElementById(emptyId);
  if (!input || !list) return;

  input.addEventListener("input", () => {
    const filter = input.value.trim().toUpperCase();
    const items = list.querySelectorAll("[data-search]");
    let visible = 0;
    items.forEach((item) => {
      const match = item.dataset.search.toUpperCase().includes(filter);
      item.style.display = match ? "" : "none";
      if (match) visible++;
    });
    if (empty) empty.style.display = visible === 0 ? "block" : "none";
  });
}

function setupNavToggle() {
  const btn = document.getElementById("navToggle");
  const nav = document.getElementById("siteNav");
  if (!btn || !nav) return;

  btn.addEventListener("click", () => {
    const open = nav.classList.toggle("is-open");
    btn.setAttribute("aria-expanded", open ? "true" : "false");
  });

  nav.querySelectorAll("a").forEach((link) => {
    link.addEventListener("click", () => {
      nav.classList.remove("is-open");
      btn.setAttribute("aria-expanded", "false");
    });
  });
}

// ---------- Landing page: "actualizado recientemente" + contadores de tiles ----------

const MAX_RECENT_UPDATES = 6;

function renderUpdates(data) {
  const grid = document.getElementById("searchableGrid");
  if (!grid) return;

  const updates = [...data.updates]
    .sort((a, b) => new Date(b.updated) - new Date(a.updated))
    .slice(0, MAX_RECENT_UPDATES);

  grid.innerHTML = updates
    .map(
      (u) => `
      <a class="ces-update-card" href="${resolveUrl(u.url)}" data-search="${u.title}">
        <time>${formatDate(u.updated)}</time>
        <h3>${u.title}</h3>
        <p>${u.description}</p>
      </a>`
    )
    .join("");
}

function renderTileCounts(data) {
  document.querySelectorAll(".ces-tile[data-category]").forEach((tile) => {
    const key = tile.dataset.category;
    const category = data.categories[key];
    const countEl = tile.querySelector(".ces-tile-count");
    if (!category || !countEl) return;
    const n = category.items.length;
    const label = n === 1 ? category.countLabel.one : category.countLabel.many;
    countEl.textContent = `${n} ${label}`;
  });
}

// ---------- Paginas de categoria: listado ----------

function renderCategoryList(data) {
  const list = document.getElementById("listaItems");
  const key = window.CES_CATEGORY;
  if (!list || !key) return;

  const category = data.categories[key];
  if (!category) return;

  list.innerHTML = category.items
    .map((item) => {
      const tag = item.updated
        ? `<span class="ces-item-tag">Actualizado ${formatDate(item.updated)}</span>`
        : "";
      const btnClass = item.secondary ? "ces-btn secondary" : "ces-btn";
      const btnLabel = item.buttonLabel || "Ver";
      const downloadAttr = item.download ? "download" : "";
      return `
      <div class="ces-item" data-search="${item.title}">
        <div class="ces-item-icon"><img src="${resolveUrl(item.icon || category.tileIcon)}" alt=""></div>
        <div class="ces-item-info">
          <h3>${item.title}</h3>
          <p>${item.description}</p>
          ${tag}
        </div>
        <div class="ces-item-actions">
          <a href="${resolveUrl(item.url)}" class="${btnClass}" ${downloadAttr}>${btnLabel}</a>
        </div>
      </div>`;
    })
    .join("");
}

document.addEventListener("DOMContentLoaded", () => {
  buildRibbons();
  setupNavToggle();

  fetch(CES_DATA_URL())
    .then((res) => res.json())
    .then((data) => {
      renderUpdates(data);
      renderTileCounts(data);
      renderCategoryList(data);
      setupSearch("searchInput", "listaItems", "emptyState");
      setupSearch("heroSearchInput", "searchableGrid", null);
    })
    .catch((err) => console.error("No se pudo cargar el contenido del sitio:", err));
});
