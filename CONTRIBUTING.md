# Contributing to this repository

Copyright 2022, Compound Labs, Inc. and repository contributors. This repository is licensed under GPLv3 (please see [LICENSE](/LICENSE) for the full-text of the license).

All contributors to this repository must release contributed code under this GPLv3 license, free of any other encumbrance. Contributors also agree that contributions may be re-licensed under MIT or BSD-3 licenses in the future without notice. In such instances, all copyright notices will be retained.

## Types of contributions

You can contribute to the Compound web3 front-end in several ways. This repo is a place to discuss and collaborate on interfaces like `app.compound.finance`!

When contributing to this repository, please first discuss the change you wish to make via issue, forum post on [comp.xyz](https://www.comp.xyz/), or Discord.

You, of course, may fork this repository and deploy your own changes. Please note: under GPLv3, you are required to open-source your changes if you make them available on a public website!

### :mega: Discussions

If you'd like help troubleshooting a PR you're working on, have a great new idea, or want to share something amazing, join us in [Discord](https://compound.finance/discord).

### :beetle: Issues

Issues are used to track tasks that contributors can help with. If an issue has a triage label, we haven't reviewed it yet and you shouldn't begin work on it.

If you've found something in the content or the website that should be updated, search open issues to see if someone else has reported the same thing. If it's something new, open an issue. We'll use the issue to have a conversation about the problem you want to fix.

### :earth_asia: Translations

This website is internationalized and available in multiple languages. The source content in this repository is divided into several JSON Language files in each supported language. Adding or updating existing strings for any language will require a Pull Request to modify the appropriate JSON Language file. 

### :hammer_and_wrench: Pull requests

A pull request is a way to suggest changes to this repository. This includes changes for strings content or code changes.

Code changes need to be formatted with [elm-format](https://github.com/avh4/elm-format) for Elm code and [prettier](https://prettier.io/) for javascript code before they can be reviewed. It's a good idea to pick an editor integration so that this format is applied on save.

Pull requests that are opened again the `main` branch are automatically deployed to IPFS assuming the PR builds with no issues. This can help you verify your changes actually perform as expected.

## Working in the repository

Here's some information that might be helpful while working on a PR:

- [Development](/README.md) - This guide describes how to get this app running on your local machine.

- [Elm style guide](https://elm-lang.org/docs/style-guide) - This guide covers Elm-specific information about how we style code. The [Elm design guide](https://package.elm-lang.org/help/design-guidelines) is also a good reference as you develop.

- [BEM style guide](http://getbem.com/introduction/) - This guide covers the BEM style of defining Sass/Css which we follow in this repo.

## Reviewing

The community is expected to help review every single PR. The purpose of reviews is to ensure that any changes provide the safest and best experience for users of the Compound web3 front-end.

:yellow_heart: Reviews are always respectful, acknowledging that everyone did the best possible job with the knowledge they had at the time.  
:yellow_heart: Reviews discuss content, not the person who created it.  
:yellow_heart: Reviews are constructive and start conversation around feedback.  

You should always review your own PR first.

For content changes, make sure that you:
- [ ] Confirm that the changes meet the user experience and do not introduce any regressions.
- [ ] Compare your pull request's source changes to the version deployed to IPFS to confirm that the output matches the source and that everything is rendering as expected. This helps spot issues like typos, or content that isn't rendering correctly.
- [ ] If there are any failing checks in your PR, troubleshoot them until they're all passing.

### Source Code vs app.compound.finance

When changes from pull requests are merged, a build is first created in the repo and then an action snapshots the `/build` directory contents and deploys the directory to IPFS.

A member of the Compound team can then take the built release and update where [app.compound.finance](https://app.compound.finance) redirects to your IPFS content of your release. We'll also notify users in the `#development` channel in Compound Discord to notify the community of a new deploy of the web3 front-end.
