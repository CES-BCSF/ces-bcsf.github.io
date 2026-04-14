function actualizarFecha() {
  fetch('templates/update_date.html')
    .then(response => {
      if (!response.ok) throw new Error('Error al cargar la fecha');
      return response.text();
    })
    .then(html => {
      // Inyectamos el contenido directamente
      document.getElementById('fecha-dinamica').innerHTML = html;
    })
    .catch(error => {
      console.error('Hubo un problema:', error);
      document.getElementById('fecha-dinamica').innerText = "Error al cargar";
    });
}

function actualizarGlosario() {
  var el = document.getElementById('glosario-indicadores');
  if (!el) return;
  fetch('templates/glosario_indicadores.html')
    .then(response => {
      if (!response.ok) throw new Error('Error al cargar el glosario');
      return response.text();
    })
    .then(html => {
      el.innerHTML = html;
    })
    .catch(error => {
      console.error('Hubo un problema:', error);
      el.innerText = "Error al cargar";
    });
}

// Ejecutar al cargar la página
window.onload = function() {
  actualizarFecha();
  actualizarGlosario();
};

// Ajuste dinámico de la altura de los iframes según el contenido
window.addEventListener('message', function(event) {
  if (event.data.height) {
    // Buscar cual iframe envió el mensaje
    var iframes = document.querySelectorAll('iframe[id^="iframe-panel"]');
    iframes.forEach(function(iframe) {
      if (iframe.contentWindow === event.source) {
        iframe.style.height = (event.data.height + 14) + 'px';
      }
    });
  }
}, false);

// Mobile navbar toggle
document.addEventListener("DOMContentLoaded", function () {
  const toggle = document.getElementById("navbar-toggle");
  const links = document.getElementById("navbar-links");

  if (toggle && links) {
    toggle.addEventListener("click", function () {
      links.classList.toggle("open");
    });
  }

  // Smooth scrolling for internal links
  const navLinks = document.querySelectorAll('a[href^="#"]');
  navLinks.forEach((link) => {
    link.addEventListener("click", function (e) {
      const targetId = this.getAttribute("href");
      const target = document.querySelector(targetId);
      if (target) {
        e.preventDefault();
        const yOffset = -70; // adjust if header height changes
        const y =
          target.getBoundingClientRect().top + window.pageYOffset + yOffset;
        window.scrollTo({ top: y, behavior: "smooth" });
        links.classList.remove("open");
      }
    });
  });
});

// Mostrar el botón cuando el usuario baja 300px
window.onscroll = function() {
    scrollFunction();
};

function scrollFunction() {
    const btn = document.getElementById("btnScrollTop");
    if (document.body.scrollTop > 250 || document.documentElement.scrollTop > 250) {
      btn.classList.add("show")
      // btn.style.display = "block";
    } else {
      btn.classList.remove("show")
      // btn.style.display = "none";
    }
}

// Función que se ejecuta al hacer clic
function scrollTopPage() {
    window.scrollTo({
        top: 0,
        behavior: 'smooth' // Esto hace que el movimiento sea suave 
    });
}

