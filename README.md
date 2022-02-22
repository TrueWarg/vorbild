# Vorbild: Generation files from templates

Vorbild is simple console app for generation files and their content from
specified by user template sources.

It may be useful if you have regular structure and hierarchy of project files

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
    ---sorce
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
    ---placeholder.json (optional)
```

`feature_name` is specified by user, `file_path` depends on `module_name` and `packange_name`
(see [values](#values) example).

`{{^feature_name}}Controller` here is just text file with your ext (.hs, .kt, .py, .txt etc.) 
and content:

```kotlin
package {{^file_path#replace '/' '.'}}

class {{^feature_name}}Controller {
    ...
}
```

About `#replace '/' '.'` read [here](#modifiers).

### Values

Values specified in `values.json`:

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
If item doesn't contain `value` it will be specified by user.

In examble above we have 

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

Suppose user input is `packange_name = core`, `module_name = temperature`, 
`feature_name = Measurement`. So vorbild will generate this:

```kotlin
// com/core/temperature/MeasurementController.kt


package com.core.temperature

class MeasurementController {
    ...
}
```

### Modifiers

We use `replace '/' '.'` above. Is just replace `/` to `.` in 
final `file_path` value. 

General expression for this modifier `replace '<str1>' '<str2>'`. 

Currently `replace` and `toLower` (turn text to lower case) modifiers are supported.

`#` is just separator for modifiers. We can use few modifiers together:
`#replace 'a' 'b'#toLower#replace 'cd' 'xy'`

### Placeholder

General expression for value placeholders looks like this

`<openTag><valuePrefix><value><modifierSeparator><modifier1>...<modifierSeparator><modifierN><closeTag>`

By default `openTag == {{`, `closeTag = }}`, `valuePrefix = ^`, `modifierSeparator = #`,
but we can configure it in optional file `placeholder.json`:

```json
{
   "openTag" : "<openTag>",
   "closeTag" : "closeTag",
   "valuePrefix" : "<valuePrefix>",
   "modifierSeparator" : "<modifierSeparator>"
}
```

If you wont use default config, just will not add this file in template folder.

### Command

General command looks like 
```bash
   vorbild (-s|--src template path) [-d|--dst destination path]
```

If -d is not specified, currently directory will be used.

## License
Vorbild is distributed under the [Apache-2.0 License](https://github.com/TrueWarg/vorbild/blob/master/LICENSE).
