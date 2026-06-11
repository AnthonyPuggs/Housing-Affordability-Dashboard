// Mobile navbar: collapse the menu after a nav link is chosen.
(function() {
  document.addEventListener('click', function(e) {
    var navLink = e.target.closest('.navbar-collapse.show .nav-link');
    if (!navLink) return;

    var mainNav = document.getElementById('main_nav');
    var collapseEl = navLink.closest('.navbar-collapse.show');
    if (!collapseEl) return;
    if (mainNav && !navLink.closest('#main_nav')) return;

    var toggle = mainNav ? mainNav.querySelector('.navbar-toggle, .navbar-toggler') : null;
    var toggleVisible = toggle &&
      window.getComputedStyle(toggle).display !== 'none' &&
      window.getComputedStyle(toggle).visibility !== 'hidden';
    if (!toggleVisible && window.innerWidth >= 992) return;

    var forceHideNavbar = function() {
      collapseEl.classList.remove('show');
      collapseEl.classList.remove('collapsing');
      collapseEl.classList.add('collapse');
      if (toggle) {
        toggle.classList.add('collapsed');
        toggle.setAttribute('aria-expanded', 'false');
      }
    };

    if (window.bootstrap && bootstrap.Collapse) {
      bootstrap.Collapse.getOrCreateInstance(collapseEl, { toggle: false }).hide();
    } else if (window.jQuery && typeof $(collapseEl).collapse === 'function') {
      $(collapseEl).collapse('hide');
    } else {
      forceHideNavbar();
    }
    window.setTimeout(forceHideNavbar, 150);
  });
})();

// Startup loading splash: removed on the first shiny:idle event
// (jQuery is available here because Shiny loads it in <head>).
$(document).one('shiny:idle', function() {
  var splash = document.getElementById('app-loading-splash');
  if (!splash) return;
  splash.classList.add('app-splash-hidden');
  window.setTimeout(function() {
    if (splash.parentNode) splash.parentNode.removeChild(splash);
  }, 450);
});
