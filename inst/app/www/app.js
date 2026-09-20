/* Small general-purpose glue, independent of the globe. Toggles a class on
   an element from the server, so layout state (e.g. whether the map sidebar
   reserves a grid column) is driven by the actual Shiny input value instead
   of parsing a CSS :has()/[style*=] match against another element -- the
   latter depends on browser support and exact inline-style formatting and
   was found to behave unreliably across zoom levels and window sizes. */
document.addEventListener('DOMContentLoaded', () => {
  // R leaflet's layer manager passes string stamp IDs when clearing groups.
  // Resolve IDs to objects for the FeatureGroup used by the draw toolbar.
  if (window.L) {
    const removeLayer = L.FeatureGroup.prototype.removeLayer;
    L.FeatureGroup.prototype.removeLayer = function (layer) {
      if (typeof layer === 'number' || (typeof layer === 'string' && /^\d+$/.test(layer))) {
        layer = this.getLayer(layer);
        if (!layer) return this;
      }
      return removeLayer.call(this, layer);
    };
  }
  // Reference popups must never intercept vertices while defining an AOI.
  if (window.L) L.Map.addInitHook(function () {
    if (this.getContainer().id !== 'map') return;
    const container = this.getContainer();
    this.on('draw:drawstart draw:editstart draw:deletestart', () => container.classList.add('als-drawing'));
    this.on('draw:drawstop draw:editstop draw:deletestop', () => container.classList.remove('als-drawing'));
  });
  const information = document.getElementById('welcome_information');
  if (information) new MutationObserver(() => {
    const title = information.querySelector('#welcome_information_title');
    if (title) requestAnimationFrame(() => {
      title.focus({preventScroll: true});
      information.scrollIntoView({block: 'start', behavior: 'auto'});
    });
  }).observe(information, {childList: true});
  Shiny.addCustomMessageHandler('als-toggle-class', (msg) => {
    document.querySelectorAll(msg.selector).forEach((el) => el.classList.toggle(msg.class, !!msg.on));
  });
});

$(document).on("shiny:connected",function(){Shiny.addCustomMessageHandler("als-clear-review-link",function(message){history.replaceState(null,"",location.pathname);});});
