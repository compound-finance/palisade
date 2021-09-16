#!/usr/bin/env node

"use strict";

const fs = require('fs');
const chokidar = require('chokidar');
const util = require('util');
const {
  buildTranslations,
  getLangFile
} = require('./i18n');
const {
  translate,
  writeLangFile
} = require('./translate');

const writeFileAsync = util.promisify(fs.writeFile);

const argv = require('yargs')
  .alias('h', 'help')
  .help('help')
  .option('a', {
      alias: 'alwaysEnglishPath',
      demandOption: true,
      describe: 'path for english only strings (e.g. always_english_strings.json)',
      type: 'string'
  })
  .option('p', {
      alias: 'translationsPath',
      demandOption: true,
      describe: 'path for translations (e.g. strings.<lang>.json)',
      type: 'string'
  })
  .option('s', {
      alias: 'source',
      demandOption: true,
      describe: 'source language (e.g. en)',
      type: 'string',
  })
  .option('l', {
      alias: 'languages',
      demandOption: true,
      describe: 'languages to translate (e.g. es zh).',
      type: 'array',
  })
  .option('t', {
      alias: 'translate',
      describe: 'should we perform translation',
      type: 'boolean',
  })
  .option('k', {
      alias: 'apiKey',
      describe: 'api key for translations',
      type: 'string',
  })
  .option('o', {
      alias: 'outputFile',
      demandOption: true,
      describe: 'output file (e.g. Translations.elm)',
      type: 'string'
  })
  .option('m', {
      alias: 'module',
      demandOption: true,
      default: 'Translations',
      describe: 'output module (e.g. Strings.Translations)',
      type: 'string'
  })
  .option('w', {
      alias: 'watch',
      describe: 'watch intput file for changes',
      type: 'boolean'
  })
  .usage('Usage: $0 -a [englishOnlyFile] -p [translationsPath] -s en -l [languageCodes] -o [outputFile] -m [elmModule]')
  .argv;

let translateId = 0;
let buildId = 0;

async function translateNew(apiKey, source, languages) {
  let id = ++translateId; // Translate id lets us ignore previous translations if new ones come in

  apiKey = apiKey || process.env['TRANSLATE_KEY'];
  if (!apiKey) {
    console.log(`Using source language to default all untranslated strings.`);
  } else {
    console.log(`Using the specified google translate apiKey to fill untranslated strings.`);
  }

  console.log(`Building machine translations...`);
  const langTranslations = await translate(apiKey, source, languages);

  await Promise.all(Object.entries(langTranslations).map(async ([lang, {values, original, translations}]) => {
    return await writeLangFile(source, lang, values);
  }));

  const totals = Object.entries(langTranslations).reduce((acc, [lang, {translations}]) => {
    return {
      ...acc,
      [lang]: translations.length
    }
  }, {});

  const givenTotals = Object.entries(totals).filter(([lang, total]) => total > 0);

  if (givenTotals.length > 0) {
    const totalString = 
      givenTotals.map(([lang, total]) => `${total} "${lang}" key${total == 1 ? '' : 's'}`);  

    console.log(`Translated ${totalString.join(", ")}`);
  } else {
    console.log(`No translations needed`);
  }
}

async function rebuild(mod, alwaysEnglishPath, translationsPath, source, languages, outputFile) {
  let id = ++buildId; // Build id lets us ignore previous builds if new ones come in

  console.log(`Building elm translation file ...`);
  const output = await buildTranslations(mod, alwaysEnglishPath, translationsPath, source, languages);
  if (id === buildId) {
    await writeFileAsync(outputFile, output);
    console.log(`Saved to ${outputFile}...`);
  }
}

const sourceFile = getLangFile(argv.translationsPath, argv.source);

async function doTranslate() {
  if (argv.translate) {
    return await translateNew(argv.apiKey, argv.source, argv.languages);
  }
}

async function doRebuild() {
  return await rebuild(argv.module, argv.alwaysEnglishPath, argv.translationsPath, argv.source, argv.languages, argv.outputFile);
}

if (argv.watch) {
  const allLanguages = [argv.source].concat(argv.languages);
  const langFiles = allLanguages.map(language => getLangFile(argv.translationsPath, language));
  const englishOnlyFile = argv.alwaysEnglishPath;

  chokidar.watch(sourceFile).on('change', doTranslate);
  chokidar.watch(langFiles).on('change', doRebuild);
  chokidar.watch(englishOnlyFile).on('change', doRebuild);
}

// Always kick off a rebuild, even when watching
doTranslate().then(doRebuild);
