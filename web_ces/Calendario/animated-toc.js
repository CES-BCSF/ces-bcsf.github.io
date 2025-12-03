function toggleTOC() {
    const content = document.getElementById('tocContent');
    const icon = document.getElementById('toggleIcon');
    
    content.classList.toggle('collapsed');
    icon.classList.toggle('collapsed');
}

function toggleSublist(event) {
  event.stopPropagation(); // evita que el click se propague a niveles superiores

  const parentLink = event.currentTarget;
  const sublist = parentLink.nextElementSibling; // busca el <ul> justo después

  if (sublist && sublist.tagName === 'UL') {
    sublist.classList.toggle('open');

    // Rota la flecha ▼
    const icon = parentLink.querySelector('.toc-accordion-icon');
    if (icon) icon.classList.toggle('open');
  }
}


// Smooth scrolling for anchor links
document.addEventListener('DOMContentLoaded', function() {
    document.querySelectorAll('a[href^="#"]').forEach(anchor => {
        anchor.addEventListener('click', function (e) {
            e.preventDefault();
            const target = document.querySelector(this.getAttribute('href'));
            if (target) {
                target.scrollIntoView({
                    behavior: 'smooth',
                    block: 'start'
                });
            }
        });
    });
});

document.addEventListener('DOMContentLoaded', function() {
  const items = document.querySelectorAll('.toc-item');
  items.forEach((item, index) => {
    item.style.animationDelay = `${(index + 1) * 0.15}s`;  // 0.1s increment
  });
});

