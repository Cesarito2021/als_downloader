/* Small general-purpose glue, independent of the globe. Toggles a class on
   an element from the server, so layout state (e.g. whether the map sidebar
   reserves a grid column) is driven by the actual Shiny input value instead
   of parsing a CSS :has()/[style*=] match against another element -- the
   latter depends on browser support and exact inline-style formatting and
   was found to behave unreliably across zoom levels and window sizes. */
document.addEventListener('DOMContentLoaded', () => {
  Shiny.addCustomMessageHandler('als-toggle-class', (msg) => {
    document.querySelectorAll(msg.selector).forEach((el) => el.classList.toggle(msg.class, !!msg.on));
  });
});
