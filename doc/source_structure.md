Cy Source Structure
===================

Cy source code is organized hierarchically into organizations, packages, and modules.

An organization name is an Internet domain name optionally prefixed by a user name within that
domain and a "+" or "@".  When the "@" separator is used, the user name must represent an email
address.  When the "+" separator is used, the user name must represent the primary non-email user
namespace recognized by the domain owner. Rather than exposing an email address to spam by using the
"@" syntax, it is usually better to use a public identifier at a social network, as in
`deansher+github.com` or `deansher+twitter.com`.

A package name is a sequence of at least one Cy identifier, separated by slashes.

A module name is a Cy identifier, or empty for the top-level module of a package.

There is a top-level module associated with every package, which is public and has no further
module name. A package may have additional public and private modules, which have names. 

Cy source code is stored in a filesystem directory called a "Cy root directory". This directory
contains at least `node-out` and `browser-out`, preferably contains `src` and `README.md`, and
contains `contracts` if it does not contain `src`. The name `src` is reserved for this purpose, and
cannot be used as an organization, package, or module name.

The top-level module for (for example) a package with organization name "cy-platform.org" and
package name "examples/chat" is defined in a single file:

    $CY_ROOT_DIR/src/cy-platform.org/examples/chat.cy

This source file must begin with a package declaration.

An additional public or private module for the above example with module name "mockclient" is
defined in a single file:

    $CY_ROOT_DIR/src/cy-platform.org/examples/chat/mockclient.cy

This source file must begin with a module declaration.

Packages cannot be nested, even though their names and filesystem paths are organized
hierarchically.  It is an error to declare `foo.com/bar/baz` as a package (by placing a package
declaration in a file `foo.com/bar/baz.cy`) and to then also declare `foo.com/bar/baz/biff` as a
package (by placing another package declaration in a file `foo.com/bar/baz/biff.cy`.) The reason for
this restriction is to allow a simple system of scopes, versioning, and dependencies. The reason for
using hierarchical package names is to allow packages within an arganization to be logically
organized.

Modules within a package cannot be nested beyond the distinction between the package's top-level
module and its possible submodules.  It is an error to declare `foo.com/bar/baz` as a package (by
placing a package declaration in a file `foo.com/bar/baz.cy`) and to place any source code in any
subdirectory deeper than `foo.com/bar/baz`.

Given any single Cy source file, the compiler can examine the file's package or module declaration
and its filesystem path to find the corresponding "Cy root directory" as defined in the Dependency
Management documentation. It is an error for Cy source files to have conflicting notions of their Cy
root directories, or to be placed in the wrong location relative to their Cy root directories.