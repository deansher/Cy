Cy Dependency Management
========================

Each Cy package has a version number with syntax and semantics defined at http://semver.org/
(specifically the v2.0.0-rc.1 version of that standard).  The version number is declared in the
`package` statement at the top of the package definition file.

There is a central registry of all public Cy packages in github:
`https://github.com/Cy-Hub/cy-packages`.  If you want to make your package public, submit a pull
request to update this registry.  (Include a suggestion for how to verify that you own the domain
name or public website path used to construct the package names you are claiming.)

The directory structure of the registry corresponds to Cy package structure, with each package
`jack+advent.com/xyzzy/plugh` being represented by a "package registry file" `plugh.txt` in the
directory `jack+advent.com/xyzzy`. Each line of a package registry file has four
whitespace-separated values with no other delimiters:

* the minimum version number covered by this line of the file

* the URL of the repository containing the package (currently must be git)

* the branch in that repository that contains the package

* the relative path from the root of the repo to a "Cy root directory" that contains at least
  `node-out` and `browser-out`, that preferably contains `src` and `README.md`, and that contains
  `contracts` if it does not contain `src`.  See the *Cy Source Structure* docs for more 
  information about Cy root directories.

The lines of a package registry file are ordered by descending minimum version number.

An individual or organization can manage private source code by creating a local registry with the
same structure as the public package registry and by providing the url of this local registry
repository to the Cy compiler in the `--package-registry` parameter.  Multiple package registries
can be specified by providing this parameter multiple times, in which case all registries will be
searched for each package.  It is an error for the same package to appear in multiple registries,
even if one of those registries is the public registry.  (This means that the standard organization
naming conventions must be followed even within local registries to avoid name collisions.)  This
system permits organizations to host alternative public package registries as "local" registries,
but that practice is discouraged.

For rapid iteration during development, the Cy compiler also supports a `--root` parameter, which is
the local filesystem path of a Cy root directory.  This parameter can be provided multiple times to
form a search path.  The Cy compiler looks for each package in these root directories before
checking the package registries.  The nearest Cy root directory containing the current working
directory is implicitly the first entry on the root directory search path.

The Cy compiler assumes that the first root directory or package registry entry mentioning a given
package contains (or refers to) the entire source for the package; it does not merge source from
multiple Cy root directories within a single package.

The Cy compiler provides a `verify` command that is intended for use as a pre-commit hook,
and that verifies the following invariants on your repo:

* Every Cy source file in the repo must be contained in a well-formed Cy root directory.

* You cannot modify a previously committed output file.  (Instead, you can create a new version.)

* There must be a new output file for each modified source file.

* The version numbers of each source file and its corresponding contract file must match.

* If either the source file or the contract file are updated, then both must be updated
  at least with a new version number.

* Each new output file must have a version that corresponds to the current version of its source
  file, a `__cy_source_md5` that matches the source file, and a `__cy_contract_md5` that matches
  the contract file.

* You cannot delete a previously committed output file that is still the target of any
  version spec symbolic link.

* You cannot delete a previously committed version spec symbolic link except that after a new
  major or major.minor version has existed for one year, only the highest-numbered version
  spec symbolic link must be kept for older major versions or older major.minor versions.
  For example, if the highest version reached with major version number 1 was 1.2.97, then
  one year after version 2.0.0 is released, all version spec symbolic dependency links for
  major version number 1 *except* 1.2.97+.js can be deleted.

* Unless the current source for a package has a new major version number, the current contract must
  be a superset of the previous contract.

* Each package must meet its contract.
