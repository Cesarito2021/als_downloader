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

// Explicit browser submission; no credentials or background network calls.
document.addEventListener('submit', async function (event) {
  const form = event.target;
  if (!form.matches('form.als-formspree')) return;
  event.preventDefault();
  if (form.dataset.submitted === 'true') return;
  const status = form.querySelector('.als-formspree-status');
  const button = form.querySelector('button[type="submit"]');
  const retry = form.querySelector('.als-formspree-retry');
  const verify = form.querySelector('.als-formspree-verify');
  form.dataset.submitted = 'true';
  button.disabled = true;
  retry.style.display = 'none';
  verify.style.display = 'none';
  status.textContent = 'Submitting proposal...';
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), 35000);
  try {
    if (!/^https:\/\/formspree\.io\/f\/[A-Za-z0-9]+$/.test(form.action)) throw new Error('Invalid endpoint');
    const proposal = form.querySelector('.als-formspree-proposal');
    if (proposal) {
      const transfer = new DataTransfer();
      transfer.items.add(new File([proposal.value], 'zenodo-lidar-proposal.json', {type: 'application/json'}));
      form.querySelector('.als-formspree-attachment').files = transfer.files;
    }
    const response = await fetch(form.action, {method: 'POST', body: new FormData(form),
      headers: {Accept: 'application/json'}, credentials: 'omit', redirect: 'error', signal: controller.signal});
    const data = await response.json().catch(() => ({}));
    const errors = JSON.stringify(data.errors || data.error || '').toLowerCase();
    if (/captcha|challenge/.test(errors) || data.kind === 'challenge') {
      status.textContent = 'Anti-spam verification is required. Continue in the Formspree tab and check its confirmation. No approval has been granted.';
      verify.style.display = '';
    } else if (!response.ok && /monthly|per month|month.{0,30}(limit|quota)|(limit|quota).{0,30}month/.test(errors)) {
      status.textContent = 'The monthly limit of 50 submissions has been reached. Please try again after the monthly quota resets. Save your proposal to keep a copy.';
      retry.style.display = '';
    } else if (response.status === 429) {
      status.textContent = 'The submission service has reached a sending limit. Please try again later. The free plan allows 50 submissions per month; if its monthly quota is exhausted, wait until it resets. Save your proposal to keep a copy.';
      retry.style.display = '';
    } else if (response.ok && data.ok === true) {
      status.textContent = 'Proposal received by Formspree for maintainer review. Publication requires approval. This confirmation does not verify delivery to the maintainer mailbox.';
    } else if (!response.ok) {
      status.textContent = 'The submission was not accepted. Check the form configuration and fields, or try again later. Save your proposal to keep a copy.';
      retry.style.display = '';
    } else {
      status.textContent = 'The service returned an unrecognized confirmation. Receipt could not be verified. Save your proposal and check with the maintainer before retrying.';
      retry.style.display = '';
    }
  } catch (error) {
    status.textContent = 'Delivery could not be confirmed because of a connection or browser error. Save your proposal and check before retrying to avoid duplicates.';
    retry.style.display = '';
  } finally {
    clearTimeout(timer);
  }
});
document.addEventListener('click', function (event) {
  const retry = event.target.closest('.als-formspree-retry');
  const verify = event.target.closest('.als-formspree-verify');
  if (!retry && !verify) return;
  const form = (retry || verify).closest('form.als-formspree');
  if (verify) {
    // Native POST allows provider-hosted CAPTCHA without shipping third-party JS.
    HTMLFormElement.prototype.submit.call(form);
    verify.style.display = 'none';
    form.querySelector('.als-formspree-retry').style.display = '';
    form.querySelector('.als-formspree-status').textContent = 'Check the Formspree tab for verification and the delivery result. No dataset has been published.';
    return;
  }
  delete form.dataset.submitted;
  form.querySelector('button[type="submit"]').disabled = false;
  retry.style.display = 'none';
  form.querySelector('.als-formspree-status').textContent = 'Ready to retry. Do not resend a submission already confirmed by Formspree.';
});
