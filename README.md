[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CI](https://github.com/jcs-elpa/logms/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/logms/actions/workflows/test.yml)

# logms
> See where the message came from

## Usage

```el
(liquidmetal-score "FooBar" "foo")   ; 0.950
(liquidmetal-score "FooBar" "fb")    ; 0.917
(liquidmetal-score "Foo Bar" "fb")   ; 0.929
(liquidmetal-score "Foo Bar" "baz")  ; 0.0
(liquidmetal-score "Foo Bar" "")     ; 0.8
```

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
