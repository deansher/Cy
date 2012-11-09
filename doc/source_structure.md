Cy Source Structure
===================

Cy source code is organized hierarchically into organizations, packages, and modules.

An organization name is an Internet domain name optionally prefixed by a user name within that
domain and a "+" or "@".  When the "@" separator is used, the user name must represent an email
address.  When the "+" separator is used, the user name must represent the primary non-email user
namespace recognized by the domain owner. Rather than exposing an email address to spam by using the
"@" syntax and thus putting the email address in source code, it is usually better to use a public
identifier at a social network, as in `deansher+github.com` or `deansher+twitter.com`.

A package name is a sequence of at least one Cy identifier, separated by slashes.

A module name is a Cy identifier, or empty for the top-level module of a package.

There is a top-level module associated with every package, which is public and has no further
module name.  A package may have additional public and private modules, which have names.

The top-level module for (for example) a package with organization name "cy-platform.org" and package name
"examples/chat" is defined in a file `cy-platform.org/examples/chat.stq`, which must begin with a package
declaration.

An additional public or private module for the above example with module name "mockclient" is
defined in a file `cy-platform.org/examples/chat/mockclient.stq`, which must begin with a module
declaration.