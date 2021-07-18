[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/logms-badge.svg)](https://melpa.org/#/logms)
[![MELPA Stable](https://stable.melpa.org/packages/logms-badge.svg)](https://stable.melpa.org/#/logms)
[![CI](https://github.com/jcs-elpa/logms/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/logms/actions/workflows/test.yml)

# logms
> Log message with clickable links to context

<p align="center">
  <img src="./etc/demo.gif"/>
</p>

## Usage

### :mag: Step 1. `M-x logms-mode`

Please start `M-x logms-mode` so you can get the correct output from evaluating
buffer.

### :mag: Step 2. Quick example

In `*scratch*` buffer,

```el
(logms "Hello World!")  ; *scratch*:1:22 Hello World!
```

This will produce clickable text so you can navigate to that buffer!

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
