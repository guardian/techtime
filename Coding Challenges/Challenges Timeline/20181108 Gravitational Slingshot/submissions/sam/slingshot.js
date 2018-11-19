const stars = 512

const G = 6.6740831 * Math.pow(10, -11)

const visualScale = 0.06

// planet
const planetSize = 3390
const planetMass = 4 * Math.pow(10, 23)
const planetX = 800
const planetY = 320

// satellite
const satellite = document.getElementById("satellite")
const satelliteSize = 500
const satelliteVisualSize = satelliteSize * visualScale
const satelliteMass = 2000
const satelliteXStart = 0
const satelliteYStart = 300

function drawPlanet() {
  const planet = document.getElementById("planet")
  const visualSize = planetSize * visualScale

  planet.style.height = `${visualSize}px`
  planet.style.width = `${visualSize}px`
  planet.style.left = `${planetX - (visualSize/2)}px`
  planet.style.top = `${planetY - (visualSize/2)}px`
}

function drawStar(x, y, opacity) {
  const star = document.createElement("div")
  star.className = "star"
  star.style.left = `${x}%`
  star.style.top = `${y}%`
  star.style.backgroundColor = `rgba(255, 255, 255, ${opacity})`
  document.getElementById("starfield").appendChild(star)
}

function fillStarfield() {
  for (let i = 0; i < stars; i++) {
    drawStar(Math.random() * 100, Math.random() * 100, Math.random())
  }
}

let satelliteX = satelliteXStart
let satelliteY = satelliteYStart
let i = 0

let ax = 0
let ay = 0

function run() {
  const satelliteVisible = 
    ((satelliteX + satelliteVisualSize) > 0 && (satelliteX + satelliteVisualSize < (window.innerWidth + 100))) && 
    ((satelliteY + satelliteVisualSize) > 0 && (satelliteY - satelliteVisualSize < (window.innerHeight + 100)))

  if (!satelliteVisible) {
    ax = 0
    ay = 0
    satelliteX = satelliteXStart
    satelliteY = satelliteYStart
  }
  
  const dx = planetX - satelliteX
  const dy = planetY - satelliteY

  const dist = Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2))
  const theta = Math.atan2(dy, dx)

  const F = G * ((planetMass * satelliteMass) / Math.pow(dist * 80000, 2))
  const Fx = F * Math.cos(theta)
  const Fy = F * Math.sin(theta)

  ax += Fx/satelliteMass
  ay += Fy/satelliteMass

  satelliteX += ax + 2
  satelliteY += ay - 1.25
  
  satellite.style.left = satelliteX - (satelliteVisualSize/2)
  satellite.style.top = satelliteY - (satelliteVisualSize/2)
  
  window.requestAnimationFrame(run)
}


satellite.style.fontSize = `${satelliteVisualSize}px`
fillStarfield()
drawPlanet()
window.requestAnimationFrame(run)