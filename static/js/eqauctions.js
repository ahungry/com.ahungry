/* global $, alert, localStorage, populateItemSearch */
/* eslint-env jquery, browser */

var mobile = null
// Define regexes
var item = new RegExp(/([A-Z][a-z][A-Za-z ':`\-]+?)([^A-Za-z ':`\-])/g)
var add_ = new RegExp(/([a-z])([A-Z])/g)
var sell = new RegExp(/([ ]*?)(Selling|Seller|WTS)([: ]*?)/gi)
var buy = new RegExp(/([ ]*?)(Offering|Buying|WTB)([: ]*?)/gi)
var caps = new RegExp(/( [A-Z][A-Z])/g)
var dash = new RegExp(/[\-\|]+? /g)
var dash2 = new RegExp(/ [\-\|]+?/g)
var dash3 = new RegExp(/[\-]{3,10}/g)
var pipes = new RegExp(/\|/g)

var itemSearchList = ''

function pruneItemSearchList () {
  console.log('filter time')
  $('#item-search, #item-search-detail').html(itemSearchList)

  var term = $('#is-filter').val()
  var filter = new RegExp(term, 'i')
  var found = 0

  $('#item-search, #item-search-detail').find('option').each(function () {
    if (!filter.test($(this).val())) {
      $(this).remove()
    } else {
      found++
    }
  })

  $('#item-search, #item-search-detail').prepend('<option value="' + term + '">[?] ' + term + '</option>')
  found++

  var h = 10 + found * 18
  $('#item-search, #item-search-detail').css({ 'height': h > 200 ? 200 : h })
}

function checkMobile () {
  mobile = /android.+mobile|avantgo|bada\/|blackberry|blazer|compal|elaine|fennec|hiptop|iemobile|ip(hone|od)|iris|kindle|lge |maemo|midp|mmp|netfront|opera m(ob|in)i|palm( os)?|phone|p(ixi|re)\/|plucker|pocket|psp|symbian|treo|up\.(browser|link)|vodafone|wap|windows (ce|phone)|xda|xiino/i.test(navigator.userAgent || navigator.vendor || window.opera)
  if (mobile) {
    $('#wrap').css({ 'margin': '0px' })
    $('#content').css({ 'padding': '0px' })
  }
}

function itemInfo (itemName) {
  if (mobile) return false
  var url = 'http://wiki.project1999.com/index.php/' + itemName
  $('#wiki-box-loading').show()
  $('#wiki-box iframe').hide()
  $('#wiki-box iframe').attr({ 'src': url })
  $('#wiki-box').show()
  setTimeout(function () {
    $('#wiki-box-loading').hide()
    $('#wiki-box iframe').show()
  }, 2000)
  return false
}

function findItems (target) {
  target = target || '.aul'
  $(target).each(function () {
    var text = $(this).html()
    text = text.replace(add_, '$1, $2')
    text = text.replace(sell, '$1(WTS)$3')
    text = text.replace(buy, '$1(WTB)$3')
    text = text.replace(caps, ', $1')
    text = text.replace(dash, ', ')
    text = text.replace(dash2, ', ')
    text = text.replace(dash3, ', ')
    text = text.replace(pipes, ', ')
    text = text.replace(item, '<a href="http://wiki.project1999.com/index.php/$1" alt="$1" class="ii">$1<div class="itemTT" style="font-family:monospace;font-size:12px;"></div></a>$2')

    $(this).html(text)
  })

  initIiWatchers()
}

function searchP () {
  var regex = $('#item-search, #item-search-detail').val()
  var type = $('#search-type').val()
  var server_color = $('#server-color').val() || 'blue'

  $.ajax({
    url: '/action/eq/getItems/' + server_color + '/' + type + '/' + regex,
    data: '',
    type: 'get',
    success: function (res) {
      $('#listings').html(res)
      setTimeout(function () { findItems() }, 1)
    }
  })
}

function toolTipPop () {
  $('.itemTT').hide()
  var itt = $(lastItemHover).find('.itemTT')
  if (itt.html() === '') {
    var item = $(lastItemHover).attr('alt')
    $.ajax({
      url: '/action/eq/getItem/' + item,
      data: '',
      type: 'get',
      success: function (res) {
        itt.html(res).fadeIn(500)
      }
    })
  } else {
    itt.show()
  }
}

var filterTO
var pruneTO
var lastItemHover = null
var statTimeout = null

function initIiWatchers () {
  $('.ii').on('mouseover', function () {
    lastItemHover = $(this)
    statTimeout = setTimeout(function () { toolTipPop() }, 300)
  })

  $('.ii').on('mouseout', function () {
    $('.itemTT').hide()
    clearTimeout(statTimeout)
    statTimeout = null
  })

  $('.ii').on('click', function () {
    var itemName = $(this).attr('alt')
    itemInfo(itemName)
    return false
  })
}

$(document).ready(function () {
  $('#reset').click(function () {
    $('#is-filter').val('')
    $('#item-search, #item-search-detail').html(itemSearchList)
    setTimeout(function () { searchP() }, 2000)
  })

  $('#is-filter-go').click(function () {
    pruneItemSearchList()
  })

  var aucPage = /eqauctions/.test(window.location.href)

  if (!aucPage) { return }

  checkMobile()
  findItems()

  $('#advancedSearch').hide()
  $('#aTog').click(function () {
    $('#advancedSearch').toggle()
  })

  $('#advancedSearch').submit(function () {
    $.ajax({
      url: '/action/eq/searchItem/',
      data: $(this).serialize(),
      type: 'post',
      success: function (res) {
        $('#advancedMatches').html(res)
        findItems('#advancedMatches')
        $('#item-search, #item-search-detail').val(res)
        searchP()
      }
    })
    return false
  })

  initIiWatchers()

  $('#itemGo').click(function () {
    var itemName = $('.item_name').val()
    itemInfo(itemName)
  })

  $('#search-type').change(function () {
    clearTimeout(filterTO)
    filterTO = setTimeout(function () { searchP() }, 100)
  })

  $('#server-color').change(function () {
    clearTimeout(filterTO)
    filterTO = setTimeout(function () { searchP() }, 100)
  })

  $('#is-filter').keyup(function () {
    clearTimeout(pruneTO)
    pruneTO = setTimeout(function () { pruneItemSearchList() }, 500)
  })

  $('#item-search, #item-search-detail').change(function () {
    clearTimeout(filterTO)
    filterTO = setTimeout(function () { searchP() }, 100)
  })

  $('#item-search, #item-search-detail').keyup(function () {
    clearTimeout(filterTO)
    filterTO = setTimeout(function () { searchP() }, 100)
  })
  searchP()

  $('#afForm').submit(function () {
    searchP()
    return false
  })

  $('.equser').submit(function () {
    $.ajax({
      url: '/equser.php',
      type: 'post',
      data: $(this).serialize(),
      success: function (res) {
        alert(res)
        window.location.reload()
      }
    })
    return false
  })

  $('.slr').on('mouseout', function () {
    $('#flavorBar').hide()
  })

  $('.slr').on('mouseover', function () {
    $('#flavorBar').show()
    var who = $(this).html()
    $.ajax({
      url: '/equser.php',
      type: 'get',
      data: 'action=getAliases&who=' + who,
      success: function (res) {
        $('#flavorBar').html(res)
      }
    })
  })

  $('#show-about').click(function () {
    $('.hide-about').slideToggle()
  })

  $('#wiki-box-close').click(function () {
    $('#wiki-box').hide()
  })

  $('#auc-search').submit(function () { return false })

  // Auto refresh the most recent auctions
  setInterval(function () { searchP() }, 30000)

  if (localStorage.itemList) {
    $('#item-search, #item-search-detail').html(localStorage.itemList)
    itemSearchList = $('#item-search, #item-search-detail').html()
  } else {
    $.getScript('/options.js', function () {
      populateItemSearch()
      localStorage.itemList = $('#item-search, #item-search-detail').html()
      itemSearchList = $('#item-search, #item-search-detail').html()
    })
  }
})
