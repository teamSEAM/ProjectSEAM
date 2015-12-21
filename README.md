# ProjectSEAM
Primary repo for PLT class project

## How to Build

- Make sure you have [docker](https://www.docker.com/) installed on your machine
- Clone this repo using `git clone https://github.com/teamSEAM/ProjectSEAM.git`
- In the root of the repo, run `docker build -t seam .` to build the project image.
- Enter the dev container environment with `docker run -it --rm seam`
- Do work (warning: when you exit the docker container, it will be destroyed. If you want your container to persist when you exit, omit the `--rm` flag from the `docker run` command

## Directory Structure
The project is organized as follows:

```
ProjectSEAM/
|-docs/
|-lib/
|-refs/
\-src/
  \-tests/
|- changelog/
```

* `docs` documentation source (TeX files, PDFs, etc.)
* `lib` third party libraries
* `refs` references (code, literature, etc.)
* `src` our code!
  - `tests` tests (Edmund FTW)

# Language Reference

## Identifiers
```
id        = (letter | '_') (letter | digit | '_')*
letter    = lowercase | uppercase
lowercase = ['a'-'z']
uppercase = ['A'-'A']
digit     = ['0'-'9']
```

## Keywords
```
else
entity
for
function
if
include
return
while
```

## Types
```
bool
int
string
float
instance
```

## Literals

### bool
`lit_bool = 'true' | 'false'`

### int
```
lit_int = ['-']? digit+
digit   = ['0'-'9']
```

### string
```
lit_string  = '"' string_item* '"'
string_item = [^'"']
```

### float
```
lit_float = ['-']? digit* '.'? digit+ exp?
exp       = ['e' 'E'] ['+' '-']? digit+
digit     = ['0'-'9']
```

## Operators
`+`, `-`, `*`, `/`, `%`,
`==`, `!=`, `>`, `<`, `>=`, `<=`,
`&&`, `||`

## Delimiters
`(`, `)`, `[`, `]`, `,`, `:`, `.`

Change Log
11/8/15
	12 am SWITCHED BACK NVM!
	12:50 am
		Sean Pushed a “working version” that now needs a tester,
		Sean plans to add “if” once this is deemed clean
