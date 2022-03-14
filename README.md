# Vorbild: File generation from templates

Vorbild is a simple console app for generating files and their content from
template sources specified by the user.

It may be useful if you have a regular structure and hierarchy of the project files.

## Using Vorbild

### Source

Create your templates:

```bash
<templates-folder>
---<template1-folder>
    ---source
    ---values.json
    ---placeholder.json (optional)
...
---<templateN-folder>
    ---source
    ---values.json
    ---placeholder.json (optional)
```

Directory `source` contains any files with paths and file content placeholder.
For example

```bash
vorbild-templates
---module
    ---source
       ---{{^file_path}}/{{^feature_name}}Controller.kt
    ---values.json
```

`feature_name` is specified by the user, `file_path` depends on `module_name` and `package_name`
(see [values](#values) example).

`{{^feature_name}}Controller` here is just a text file with your extension (.hs, .kt, .py, .txt etc.) 
and content:

```kotlin
package {{^file_path#replace '/' '.'}}

class {{^feature_name}}Controller {
    ...
}
```

Read [here](#modifiers) about `#replace '/' '.'`.

### Values

Values are specified in `values.json`:

```json
[
  {
    "name" : "value_name_1",
    "label" : "Label of value_name_1"
  },
  {
    "name" : "some_dependent_value_N-k",
    "value" : "some_text_1...{{^value_name_1}}...some_text_N...{{^value_name_N}}..."
  },
  {
    "name" : "value_name_N",
    "label" : "Label of value_name_N"
  }
]
```
If an item doesn't contain `value` then it must be specified by the user.

In the example above we have 

```json
[
  {
    "name" : "packange_name",
    "label" : "Specify package name"
  },
  {
    "name" : "module_name",
    "label" : "Specify module name"
  },
    {
    "name" : "feature_name",
    "label" : "Specify feature name"
  },
  {
    "name" : "file_path",
    "value" : "com/{{^packange_name}}/{{^module_name}}"
  }
]
```

Suppose that user input is `package_name = core`, `module_name = temperature`, 
`feature_name = Measurement`. So `vorbild` will generate this:

```kotlin
// com/core/temperature/MeasurementController.kt


package com.core.temperature

class MeasurementController {
    ...
}
```

### Modifiers

Examples above use `replace '/' '.'`. This simply replaces `/` to `.` in the
final `file_path` value. 

More general form of this modifier is `replace '<str1>' '<str2>'`. 

Currently only `replace` and `toLower` (convert text to lower case) modifiers are supported.

`#` is just a separator for modifiers. Several modifiers can be used together:
`#replace 'a' 'b'#toLower#replace 'cd' 'xy'`

### Placeholder

More general form for value placeholders looks like this

`<openTag><valuePrefix><value><modifierSeparator><modifier1>...<modifierSeparator><modifierN><closeTag>`

By default `openTag == {{`, `closeTag = }}`, `valuePrefix = ^`, `modifierSeparator = #`,
but it can be configured with the optional file `placeholder.json`:

```json
{
   "openTag" : "<openTag>",
   "closeTag" : "closeTag",
   "valuePrefix" : "<valuePrefix>",
   "modifierSeparator" : "<modifierSeparator>"
}
```

If you want to use a default config, just do not add this file to the template folder.

### Command

General command looks like 
```bash
   vorbild (-s|--src template path) [-d|--dst destination path]
```

If `-d` is not specified, current directory will be used.

## Installation 

The following installation methods are available at the moment:

1. Just download executable files from [releases](https://github.com/TrueWarg/vorbild/releases)
2. Use snap package manager:

```bash
sudo snap install vorbild
```
3. Download from [AUR](https://aur.archlinux.org/packages/vorbild) 
repository using AUR helper (yay, pacaur etc.)

## License
Vorbild is distributed under the [Apache-2.0 License](https://github.com/TrueWarg/vorbild/blob/master/LICENSE).
