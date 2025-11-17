# Find the Project Root Directory

Recursively walks up the directory tree from the current working
directory to locate a project root, identified by a given target file or
file pattern. Common use cases include locating files like `renv.lock`,
`pyproject.toml`, or any `.Rproj` file.

## Usage

``` r
find_project_root(target = "renv.lock", max.iter = 100)
```

## Arguments

- target:

  A character string indicating the target file to search for. If set to
  `".Rproj"`, the function will match any file ending in `.Rproj`.

- max.iter:

  Maximum number of parent directories to search.

## Value

A list with the following components:

- success:

  Logical indicating whether the target was found.

- project.root:

  The path to the directory where the target was found, or `NA` if not
  found.

- original.dir:

  The directory where the search started.

- user.root:

  The user's home directory (search stops here).

- target:

  The search target used.

- iter:

  The number of directory levels ascended during the search.

## Details

This function does not change the working directory; it only returns
metadata about the search.

## Examples

``` r
find_project_root("renv.lock")
find_project_root(".Rproj")
```
