
<!-- README.md is generated from README.Rmd. Please edit that file -->

# factverse <img src="man/figures/logo.png" align="right" height="138" alt="" />

The objective of the `factverse` package is to provide an easy to use,
private package manager by building on private GitHub repositories.

## Architecture

The `factverse` package manager has three components:

1.  A locally stored file with an API key to access the private packages
    on Github. This file should be kept secret!
2.  A private repository on GitHub that contains a list with names of
    packages that can be installed with the `factverse` package manager.
    This repository should be called “package_list” (the name can be
    adapted).
3.  The `factverse` R package. This package is publicly available on
    GitHub and can be installed and updated with
    `pak::pkg_install("factworks/factverse")`.

## The API File

As the `factverse` package has to access private GitHub repositories,
users first have to authenticate themselves with a GitHub Private Access
Token (PAT). These tokens can be generated on GitHub (see below). The
`factverse` package assumes that the PAT to access GitHub is saved in
the one of the following folders:

1.  In the current working directory in `.RSecrets/RSecrets.yaml`
2.  (Windows only) at `.../AppData/Local/R/.RSecrets/RSecrets.yaml`
3.  In the home directory at `~/.RSecrets/RSecrets.yaml`

> Note that the file is in the hidden `.RSecrets` directory.

When looking for the file, `factverse` will go through the directories
in the order outlined above. If it finds a `.RSecrets/RSecrets.yaml`, it
uses that file.

The `RSecrets.yaml` file must be a YAML file and contain the following
section somewhere within it:

    GitHub:
      Account_name_1:
        PAT:
          Key: [12df34d, 5d6g7h8]
          Date_created: [2023-10-12, 2024-10-12]
          Valid_until: [2024-09-01, 2099-04-18]
      Account_name_2:
        PAT:
          Key: [kjlgf84, ljfg45]
          Date_created: [2021-10-12, 2021-10-12]
          Valid_until: [2022-09-01, 2099-04-18]

Note that each key (the PAT from GitHub) is saved together with the date
at which the key was created and the date until which the key is valid.
The file allows for connecting to multiple GitHub accounts.

### Updating the PAT

Occasionally, the PAT must be updated. To this end, go to the fine
grained tokens at <https://github.com/settings/tokens?type=beta>.

1.  Select “Generate new token”
2.  Select your organization as “Resource owner”
3.  Select the repositories that you want to grant access to as
    “Repository access”
4.  In “Repository Permissions” select “Read-only” for “Content”
5.  Generate the token

The PAT we just created gives everyone with this PAT read access to the
selected GitHub repositories. Therefore, it should be treated as a
password and kept a secret.

Once the PAT has been created, add the PAT to the file above (e.g.,
`.../AppData/Local/R/.RSecrets/RSecrets.yaml`). To update the file, add
the new PAT as follows:

    GitHub:
      Account_name_1:
        PAT:
          Key: [12df34d, 5d6g7h8, new_pat]
          Date_created: [2023-10-12, 2024-10-12, YYYY-MM-DD]
          Valid_until: [2024-09-01, 2099-04-18, YYYY-MM-DD]
      Account_name_2:
        PAT:
          Key: [kjlgf84, ljfg45]
          Date_created: [2021-10-12, 2021-10-12]
          Valid_until: [2022-09-01, 2099-04-18]

## The Package List

The package list outlines the packages that are available from an
account. By default, it is assumed that this repository is called
“package_list”. This repository is private, so you will only see it if
you have access to the private repositories. The package_list repository
contains a single file: `packages.yaml`. The `packages.yaml` contains
the names of all R packages that the `factverse` package should provide
access to with the GitHub PAT. It has the following structure:

    ---
    packages: [package_name_1, package_name_2]
    essential: [package_name_1]

Note that there are two fields:

- packages: This field contains a list with the names of all packages
  that are available using the `factverse` package manager. Some of
  these packages may only be relevant for specific projects or specific
  use cases.
- essential: This field contains a list with R packages that all members
  of an organization should have installed.

## The `factverse` Package

The main objective of the `factverse` package is to provide an easy to
use interface for installing and updating their packages from private
repositories. To this end, the `factverse` package builds on two
packages that provide access to GitHub via APIs:

1.  `gh`: A wrapper around GitHubs API.
2.  `remotes`: A package for installing packages from different sources
    (e.g., private GitHub repositories).

The main functions of the `factverse` package are as follows.

### Retrieve the API key for Github

The `factverse` package will search for the `RSecrets.yaml` file
outlined above and return the first GitHub Key it finds that is still
valid. This is achieved with the `get_gh_pat()` function.

``` r
library(factverse)
get_gh_pat()
```

    #> $Account_name_1
    #> [1] "5d6g7h8"
    #> 
    #> $Account_name_2
    #> [1] "ljfg45"

### Get Available Packages

Given the API key is saved in one of the directories outlined above,
`factverse` can now download a list of all available packages from the
package_list repository (see above). To this end, we use the following
function:

``` r
factverse::get_available_packages()
```

As we are currently not connected to a GitHub API, we cannot see the
result here. However, it will look as follows:

    #> $Account_name_1
    #> $Account_name_1$packages
    #> [1] "package_name_1" "package_name_2"
    #> 
    #> $Account_name_1$essential
    #> [1] "package_name_1"

### Installing Packages

To install a package with `factverse`, we can use the `install_packages`
function. For example, to install the `package_name_2` function, we can
run:

``` r
factverse::install_package("package_name_2")
```

To automatically install all essential packages, run:

``` r
factverse::install_essentials()
```

### Updating Packages

The `factverse` package can automatically check if all installed
packages are up to date as follows:

``` r
factverse::check_for_updates()
```

    #> ✔ Your version of package_name_1 is up to date!
    #> ✖ A newer version for package_name_2 is available. Install the latest version with factverse::install_package("package_name_2").

The package can also update all installed packages automatically:

``` r
factverse::update_packages()
```

    #> ✔ Your version of package_name_1 is up to date!
    #> ✖ A newer version for package_name_2 is available. Install the latest version with factverse::install_package("package_name_2").
    #> Installing the latest version of package_name_2
