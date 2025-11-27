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
