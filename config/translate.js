const googleTranslate = require('google-translate');
const {getLangFile} = require('./i18n');
const fs = require('fs');
const util = require('util');

const readFileAsync = util.promisify(fs.readFile);
const writeFileAsync = util.promisify(fs.writeFile);

async function readLangFile(lang) {
  return await readFileAsync(`src/strings/strings.${lang}.json`, 'utf8');
}

// To be honest, this is a little crazy, but the goal
// is to match the whitespace in the JSON files until
// we choose something that actually lets us add
// any sanity to these files.
async function writeLangFile(source, lang, values) {
  const sourceFile = await readLangFile(source);
  const destFile = await readLangFile(lang);
  const fileName = `src/strings/strings.${lang}.json`;

  const json = JSON.stringify(values, null, 4);
  const sourceLines = sourceFile.split("\n");
  const destLines = json.split("\n");
  const {result, restLines} = sourceLines.reduce(({result, restLines}, sourceLine) => {
    if (sourceLine.match(/^\s*$/)) {
      return {
        result: result.concat(sourceLine),
        restLines
      };
    } else {
      const [x, ...xs] = restLines;

      return {
        result: result.concat(x),
        restLines: xs
      };
    }
  }, {result: [], restLines: destLines})
  const totalResult = result.concat(restLines).join("\n");

  if (destFile !== totalResult) {
    console.log(`Saving ${fileName}...`);

    return await writeFileAsync(fileName, totalResult);
  }
}

function difference(setA, setB) {
  var _difference = new Set(setA);
  for (var elem of setB) {
    _difference.delete(elem);
  }
  return _difference;
}

async function translate(apiKey, source, languages) {
  // Instantiates a client
  const translateObj = googleTranslate(apiKey);
  const translate = util.promisify(translateObj.translate);

  const input = JSON.parse(await readLangFile(source));
  const inputKeys = new Set(Object.keys(input));

  const langs = await languages.reduce(async (acc, lang) => {
    return {
      ...await acc,
      [lang]: JSON.parse(await readLangFile(lang))
    };
  }, Promise.resolve({}));

  const translationsNeeded = Object.entries(langs).reduce((acc, [lang, values]) => {
    const givenKeys =
      Object.entries(values)
      .filter(([k, v]) => !!v)
      .map(([k, v]) => k);

    const langKeys = new Set(givenKeys);
    const missing = difference(inputKeys, langKeys)

    const langTranslationsNeeded = [...missing].map((key) => {
      return {
        key,
        text: input[key]
      };
    });

    return {
      ...acc,
      [lang]: langTranslationsNeeded
    };
  }, {});

  const translated = await Promise.all(Object.entries(translationsNeeded).map(async ([lang, translationsNeeded]) => {
    const texts = translationsNeeded.map(({text}) => text);
    let translationTexts;

    if (texts.length === 0) {
      translationTexts = [];
    } else if (apiKey) {
      translationTexts = await translate(texts, source, lang);
    } else {
      translationTexts = translationsNeeded.map(({text}) => {
        return { 
          originalText: text,
          translatedText: text

        }
      });
    }

    if (!Array.isArray(translationTexts)) {
      translationTexts = [translationTexts];
    }

    const translations = translationsNeeded.map((t, i) => {
      return {
        ...t,
        translation: translationTexts[i].translatedText
      }
    });

    return {
      lang,
      translations
    }
  }));

  const langTranslations = translated.reduce((acc, {lang, translations}) => {
    const langValues = translations.reduce((acc, {key, translation}) => {
      return {
        ...acc,
        [key]: translation
      };
    }, langs[lang]);

    const maybeOrdered = Object.keys(input).reduce((acc, key) => {
      return {
        ...acc,
        [key]: langValues[key]
      };
    }, {});

    return {
      ...acc,
      [lang]: {
        translations: translations,
        values: maybeOrdered,
        original: langs[lang]
      }
    };
  }, {});

  return langTranslations;

  // This works, only issues:
  // 1. It might not be as good with glossary
  // ~~2. We aren't batching calls~~
  // ~~3. We're probably mangling the JSON result~~
  // ~~4. We're using a service account instead of API keys~~
}

module.exports = {
  translate,
  readLangFile,
  writeLangFile
};
