# Unicode Picker #
[![Build Status](https://travis-ci.org/accidentalrebel/emacs-unicode-picker.svg)](https://travis-ci.org/accidentalrebel/emacs-unicode-picker)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

A visual picker tool that makes searching and picking unicode characters easier.

![Imgur](http://i.imgur.com/tVBGIRw.gif)

Searches unicode characters using regular expressions and displays the results to a dedicated buffer. Selected characters from dedicated buffer are inserted back to the point from the calling buffer.

The idea for this tool is to make it easier to search, pick, and insert unicode characters. Made with developing Emacs games in mind but can also be used for other uses. 

## Installation
You can install  manually from github;
`git clone https://github.com/accidentalrebel/emacs-dokuwiki.git`

Then add this to your init:
`(require 'dokuwiki)`

## Usage
Just call `M-x unicode-picker` and then specify the string to search with.

## Thanks to
Purcell's [list-unicode-display](https://github.com/purcell/list-unicode-display) package on how to search for unicode characters.

## Todo
- [ ] Unicode categories for more flexible searching (i.e. emoticons, animals, etc).
- [ ] Should be able to search with the singular form of the given regexp string (i.e. "arrows" also searches for "arrow", "cards" for "card").
- [ ] Show the unicode details in the minibuffer or someplace else when a character is highlighted.
