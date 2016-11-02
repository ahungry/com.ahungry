'use strict'

var eq = {
  item: {
    detail: function () {
      var elem = document.getElementById('item-detail-search')
      var value = elem.getElementsByTagName('input')[0].value

      window.location.href = [
        '/action/eq/item-detail/',
        value.replace(/ /, '+')
      ].join('')

      return false
    },

    reset: function () {
      var elem = document.getElementById('item-detail-search')
      elem.getElementsByTagName('input')[0].value = ''
    }
  }
}

$('#item-search-detail').click(function () {
  var form = document.getElementById('item-detail-search')
  var elem = form.getElementsByTagName('input')[0]
  elem.value = $(this).val()

  window.location.href = [
    '/action/eq/item-detail/',
    elem.value.replace(/ /, '+')
  ].join('')
})

eq.item.reset()
