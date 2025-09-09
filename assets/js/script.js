// Stock Exchange Website JavaScript

// Initialize the application when DOM is loaded
document.addEventListener("DOMContentLoaded", () => {
  initializeApp()
})

function initializeApp() {
  // Add smooth scrolling for anchor links
  addSmoothScrolling()

  // Initialize any interactive elements
  initializeInteractiveElements()

  // Add loading animations
  addLoadingAnimations()
}

function addSmoothScrolling() {
  // Add smooth scrolling behavior to all anchor links
  const links = document.querySelectorAll('a[href^="#"]')
  links.forEach((link) => {
    link.addEventListener("click", function (e) {
      e.preventDefault()
      const target = document.querySelector(this.getAttribute("href"))
      if (target) {
        target.scrollIntoView({
          behavior: "smooth",
          block: "start",
        })
      }
    })
  })
}

function initializeInteractiveElements() {
  // Add click tracking for navigation buttons
  const navButtons = document.querySelectorAll(".nav-button")
  navButtons.forEach((button) => {
    button.addEventListener("click", function () {
      // Add a subtle click effect
      this.style.transform = "scale(0.98)"
      setTimeout(() => {
        this.style.transform = "scale(1)"
      }, 150)
    })
  })

  // Add hover effects for cards
  const cards = document.querySelectorAll(".nav-card, .feature-card")
  cards.forEach((card) => {
    card.addEventListener("mouseenter", function () {
      this.style.transition = "all 0.3s ease"
    })
  })
}

function addLoadingAnimations() {
  // Add fade-in animation to main content
  const container = document.querySelector(".container")
  if (container) {
    container.style.opacity = "0"
    container.style.transform = "translateY(20px)"

    setTimeout(() => {
      container.style.transition = "all 0.6s ease"
      container.style.opacity = "1"
      container.style.transform = "translateY(0)"
    }, 100)
  }
}

// Utility function to format numbers
function formatNumber(num) {
  if (num >= 1000000000) {
    return (num / 1000000000).toFixed(1) + "B"
  }
  if (num >= 1000000) {
    return (num / 1000000).toFixed(1) + "M"
  }
  if (num >= 1000) {
    return (num / 1000).toFixed(1) + "K"
  }
  return num.toString()
}

// Function to update market stats (can be called with real data)
function updateMarketStats(stats) {
  const statNumbers = document.querySelectorAll(".stat-number")
  if (statNumbers.length >= 4 && stats) {
    statNumbers[0].textContent = formatNumber(stats.activeTraders || 15847)
    statNumbers[1].textContent = "$" + formatNumber(stats.dailyVolume || 2400000000)
    statNumbers[2].textContent = formatNumber(stats.listedCompanies || 1250) + "+"
    statNumbers[3].textContent = (stats.uptime || 99.9) + "%"
  }
}

// Add keyboard navigation support
document.addEventListener("keydown", (e) => {
  // ESC key to go back (if back button exists)
  if (e.key === "Escape") {
    const backButton = document.querySelector(".back-button")
    if (backButton) {
      backButton.click()
    }
  }
})
