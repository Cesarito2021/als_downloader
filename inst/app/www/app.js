/* Small general-purpose glue, independent of the globe. Toggles a class on
   an element from the server, so layout state (e.g. whether the map sidebar
   reserves a grid column) is driven by the actual Shiny input value instead
   of parsing a CSS :has()/[style*=] match against another element -- the
   latter depends on browser support and exact inline-style formatting and
   was found to behave unreliably across zoom levels and window sizes. */
document.addEventListener('DOMContentLoaded', () => {
  const aboutButton = document.getElementById('welcome_about');
  const aboutContent = document.getElementById('welcome_about_content');
  if (aboutButton && aboutContent) {
    aboutButton.addEventListener('click', () => {
      aboutContent.open = !aboutContent.open;
      if (aboutContent.open) {
        aboutContent.scrollIntoView({block: 'nearest'});
        aboutContent.querySelector('summary').focus({preventScroll: true});
      }
    });
    aboutContent.addEventListener('toggle', () => aboutButton.setAttribute('aria-expanded', String(aboutContent.open)));
  }
  Shiny.addCustomMessageHandler('als-toggle-class', (msg) => {
    document.querySelectorAll(msg.selector).forEach((el) => el.classList.toggle(msg.class, !!msg.on));
  });
});
