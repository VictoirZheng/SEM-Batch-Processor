const { chromium } = require('playwright');
const S = process.argv[2], ROOT = process.argv[3];
(async () => {
  const b = await chromium.launch();
  const p = await b.newPage({ viewport: { width: 1440, height: 1000 }, deviceScaleFactor: 2 });
  const errs = [];
  p.on('pageerror', e => errs.push('pageerror: ' + e.message));
  p.on('console', m => { if (m.type() === 'error') errs.push('console: ' + m.text()); });
  const shot = n => p.screenshot({ path: S + '/shots/' + n + '.png', fullPage: true });
  const tab = async t => { await p.click('a[data-value="' + t + '"]'); await p.waitForTimeout(800); };
  const idle = async () => {
    await p.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'), null, { timeout: 120000 });
    await p.waitForTimeout(400);
  };
  const setSel = (id, vals) => p.evaluate(([id, vals]) => {
    document.getElementById(id).selectize.setValue(vals);
  }, [id, vals]);

  await p.goto('http://127.0.0.1:3838/?lang=zh');
  await idle(); await shot('1_home');
  await tab('data');
  await p.setInputFiles('#data_file', ROOT + '/dev/mock_data/couples_missing.csv');
  await idle(); await p.waitForTimeout(1500);
  await p.fill('#missing_codes', '-999').catch(e => errs.push('no missing_codes input: ' + e.message.split('\n')[0]));
  await p.click('#apply_missing'); await idle(); await shot('2_data');

  await tab('model');
  await setSel('template_select', 'mediation');
  await p.click('#load_template'); await idle(); await p.waitForTimeout(800);
  const ids = await p.evaluate(() => [...document.querySelectorAll('#placeholder_ui select')].map(s => s.id));
  console.log('placeholder inputs:', ids.join(', '));
  const find = k => ids.find(i => i.startsWith('ph_') && i.endsWith(k));
  await setSel(find('X'), ['stress_H', 'attach_H', 'stress_W']);
  await setSel(find('M'), ['support_H', 'commun_H']);
  await setSel(find('Y'), ['satis_H', 'commit_H']);
  await idle();
  await p.click('#add_models'); await idle(); await p.waitForTimeout(800); await shot('3_model');

  await tab('run');
  await p.fill('#opt_bootstrap', '200'); await idle(); await shot('4_run_before');
  await p.click('#run_start');
  const t0 = Date.now();
  await p.waitForFunction(() => {
    const el = document.getElementById('run_progress');
    return /100\s*%|12\s*\/\s*12/.test(el ? el.innerText : '');
  }, null, { timeout: 300000 }).catch(() => errs.push('run wait timeout'));
  await p.waitForTimeout(1500);
  console.log('run took ~', Math.round((Date.now() - t0) / 1000), 's');
  console.log('progress text:', (await p.innerText('#run_progress')).replace(/\n+/g, ' | ').slice(0, 300));
  await shot('5_run_done');
  await tab('results'); await idle(); await p.waitForTimeout(1500); await shot('6_results');
  const links = await p.$$('#shiny-tab-results .nav-tabs a');
  const names = [];
  for (let i = 0; i < links.length; i++) names.push((await links[i].innerText()).trim());
  console.log('result tabs:', names.join(' / '));
  for (let i = 1; i < links.length; i++) {
    await links[i].click(); await idle(); await p.waitForTimeout(1200); await shot('6_results_tab' + i);
  }
  await tab('plots'); await idle(); await p.waitForTimeout(2500); await shot('7_plots');
  console.log('errors:', JSON.stringify(errs, null, 1));
  await b.close();
})();
