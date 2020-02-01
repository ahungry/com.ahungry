// // See if we want to provide multi-language org src samples.
// let lang = 'php'
// const srcNodes = () => document.querySelectorAll('.src')

// const filterSrcNodes = () => {
//   const re = new RegExp(lang, 'gi')
//   const xs = srcNodes()
//   const x = xs.length
//   const unmatch = lang === 'all' ? 'block' : 'none'

//   for (let i = 0; i < x; i++) {
//     const s = xs[i].getAttribute('class')
//     if (!re.test(s)) {
//       xs[i].style.display = unmatch
//     } else {
//       xs[i].style.display = 'block'
//     }
//   }
// }

// function changeLang (e) {
//   lang = e.target.value
//   filterSrcNodes()
// }

// function removeItalics () {
//   const nodes = document.querySelectorAll('span')
//   for (let i = 0; i < nodes.length; i++) {
//     if (nodes[i].innerHTML === '(' ||
//       nodes[i].innerHTML === ')' ||
//       nodes[i].innerHTML === '[]' ||
//       nodes[i].innerHTML === '()' ||
//       nodes[i].innerHTML === '{}' ||
//       nodes[i].innerHTML === '[' ||
//       nodes[i].innerHTML === ']' ||
//       nodes[i].innerHTML === '{' ||
//       nodes[i].innerHTML === '}') {
//       nodes[i].style.fontStyle = 'normal'
//       nodes[i].style.fontWeight = 'normal'
//       nodes[i].style.marginLeft = '2px'
//       nodes[i].style.marginRight = '2px'
//       nodes[i].style.opacity = '0.8'
//     }
//   }
// }

// // Hmm...what does this do, I wonder...
// // function cleanUp () {
// //   const nodes = document.querySelectorAll('iframe[sandbox]')

// //   for (let i = nodes.length - 1; i > -1; i--) {
// //     nodes[i].remove()
// //   }
// // }

// function newTargetLinks () {
//   const nodes = document.querySelectorAll('a')

//   for (let i = 0; i < nodes.length; i++) {
//     const x = nodes[i]

//     if (/^http/.test(x.getAttribute('href')))
//       nodes[i].setAttribute('target', '_blank')
//   }
// }

// window.onload = () => {
//   const maybeChangeLang = document.getElementById('change-lang')
//   if (maybeChangeLang) {
//     document.getElementById('change-lang').addEventListener('change', changeLang)
//   }
//   removeItalics()
//   // setInterval(cleanUp, 1e3)
//   newTargetLinks()
// }

// console.log('fin')
window.onload = () => {
  // Auto size comment box
  const rw = document.getElementById('content').offsetWidth
  const w = Math.floor(0.8 * rw)
  const el = document.getElementById('comments-frame')
  el.width = w
  el.src = 'https://comments.ahungry.com/?w=' + (w)
}
