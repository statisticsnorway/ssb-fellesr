# Set Working Directory to Project Root

Uses [`find_project_root()`](find_project_root.md) to locate a project
root based on a specified target file, and sets the working directory to
that root if found.

## Usage

``` r
set_project_root(target = "renv.lock", max.iter = 100)
```

## Arguments

- target:

  A character string indicating the target file to search for. If set to
  `".Rproj"`, the function will match any file ending in `.Rproj`.

- max.iter:

  Maximum number of parent directories to search.

## Value

Invisibly returns the same list as
[`find_project_root()`](find_project_root.md), including `project.root`,
`original.dir`, `success`, etc.

## Details

This function changes the working directory as a side effect. If no
target is found, the working directory remains unchanged and a warning
is issued.

## See also

[`find_project_root`](find_project_root.md)

## Examples

``` r
set_project_root("renv.lock")
#> Warning: Project root not found (target = 'renv.lock')
set_project_root(".Rproj")
#> Working directory set to project root: /home/runner/work/ssb-fellesr/ssb-fellesr
```
