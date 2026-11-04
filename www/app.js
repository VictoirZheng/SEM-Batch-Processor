// SEM Batch Processor front-end helpers
$(function () {
  // Switch interface language by reloading with ?lang=xx
  Shiny.addCustomMessageHandler('sembp-set-lang', function (lang) {
    var url = new URL(window.location.href);
    url.searchParams.set('lang', lang);
    window.location.href = url.toString();
  });
});
