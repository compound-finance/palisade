const fs = require('fs');
const {saveTranslations} = require('../config/i18n');

const txFile = 'src/elm/Strings/Translations.elm';
const moduleName = "Strings.Translations";
const translationsPath = "src/strings/strings.<lang>.json";
const sourceLang = "en";
const languages = ["es", "zh", "fr", "ko"];
const elmFile = "src/elm/strings/Translations.elm";
const buildKey = "BUILD_I18N";

module.exports = function buildI18n() {
  if (process.env[buildKey] || !fs.existsSync(txFile)) {
    return saveTranslations(moduleName, translationsPath, sourceLang, languages, elmFile);
  } else {
    return Promise.resolve(null);
  }
}
