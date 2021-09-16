#!/usr/bin/env node

"use strict";

const fs = require('fs');
const util = require('util');
const readFileAsync = util.promisify(fs.readFile);
const writeFileAsync = util.promisify(fs.writeFile);

function buildElm(...data) {
  return doBuildElm(data, null);
};

function doBuildElm(data, seperator) {
  if (!Array.isArray(data)) {
    data = [data];
  }

  return data.map((datum) => {
    if (Array.isArray(datum)) {
      return doBuildElm(datum, seperator === null ? "" : seperator + "    ");
    } else {
      if (datum === null) {
        return "";
      } else {
        return `${seperator || ""}${datum}`;
      }
    }
  }).join(seperator === null ? "\n\n\n" : "\n");
}

function union(setA, setB) {
  var _union = new Set(setA);
  for (var elem of setB) {
      _union.add(elem);
  }
  return _union;
}

function getLangFile(translations, language) {
  return translations.replace("<lang>", language);
}

async function readAndParseLangFile(langFile) {
  let langData;
  let values;

  try {
    langData = await readFileAsync(langFile);
  } catch (e) {
    console.error(`Error reading ${langFile}`);
    throw e;
  }

  try {
    values = JSON.parse(langData)
  } catch (e) {
    console.error(`Error parsing ${langFile}`);
    throw e;
  }
  return values;
}

function allMatches(regex, str) {
  const result = [];
  let match;
  while (match = regex.exec(str)) {
    result.push(match[1]);
  }
  return result;
}

async function buildTranslations(mod, englishOnlyLangFile, translations, source, languages) {
  const allLanguages = [source].concat(languages);

  let langs = await Promise.all(allLanguages.map(async (language) => {
    const langFile = getLangFile(translations, language);
    const values = await readAndParseLangFile(langFile);

    return {
      language,
      sym: language.charAt(0).toUpperCase() + language.slice(1),
      values
    };
  }));

  //Append english only strings to source langauge.
  const englishOnlyValues = await readAndParseLangFile(englishOnlyLangFile);
  const sourceLang = langs[0];
  Object.assign(sourceLang.values, englishOnlyValues);

  const langTags = langs.map((lang, index) => {
    const sep = index === 0 ? '=' : '|';
    return `${sep} ${lang.sym}`;
  });

  const langCases = langs.map((lang) => {
    return [`"${lang.language}" ->`, [lang.sym, null]];
  });
  const fullLangCases = langCases.concat([[`_ ->`, [sourceLang.sym]]]);

  const keys = Object.keys(sourceLang.values);
  const totalReplacements = keys.reduce((acc, key) => {
    const translations = langs.map((lang) => lang.values[key] || "");
    const val = translations.reduce((acc, translation) => {
      return union(acc, allMatches(/{{([\w_]+)}}/g, translation));
    }, new Set());

    return {
      ...acc,
      [key]: val
    };
  }, {});

  const translationFns = keys.map((key) => {
    const replacementsSet = totalReplacements[key];
    const replacements = [...replacementsSet];
    const replacementTypes = replacements.map((x) => "String");
    const types = ["Lang", ...replacementTypes, "String"];
    const args = ["lang", ...replacements];

    const langTranslations = langs.map((lang, index) => {
      const langVal = lang.values[key] || sourceLang.values[key]; // default to first lang
      const langSanitizedVal = langVal.replace("\"", "\\\"").replace("\n", "\\\n");
      const replacedVal = replacements.reduce((curr, replacement) => {
        return curr.split(`{{${replacement}}}`).join(`" ++ ${replacement} ++ "`);
      }, langSanitizedVal);

      return [
        `${lang.sym} ->`, [`"${replacedVal}"`].concat(index !== langs.length - 1 ? [null] : [])
      ];
    });

    return [
      `${key} : ${types.join(" -> ")}`,
      `${key} ${args.join(" ")} =`,
      [
        `case lang of`,
        ...langTranslations
      ]
    ]
  });

  let exposedKeys = keys.concat(["getLnFromCode"]).sort();
  const exposé = ["Lang(..)", ...exposedKeys];

  return buildElm(
    `module ${mod} exposing (${exposé.join(", ")})`,
    [
      `type Lang`,
      langTags
    ],
    [
      `getLnFromCode : String -> Lang`,
      `getLnFromCode code =`,
      [
        `case code of`,
        ...fullLangCases
      ]
    ],
    ...translationFns
  );
}

async function saveTranslations(mod, translations, source, languages, outputFile) {
  const output = await buildTranslations(mod, translations, source, languages);

  await writeFileAsync(outputFile, output);
}

module.exports = {
  buildTranslations,
  getLangFile,
  saveTranslations
};
