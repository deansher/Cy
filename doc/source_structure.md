Staq Source Structure
=====================

Staq source code is organized hierarchically into organizations, packages, and modules.

An organization name is an Internet domain name optionally prefixed by a user name within that
domain and a "+" or "@".  The user name should be one assigned and recognized by the owner of the
domain.  This could be the user name of an email address (as in `your_user_name@gmail.com`), but
rather than exposing an email address to spam by putting it in source code, it is usually better to
use a public identifier at a social network, as in `deansher+github.com` or `deansher+twitter.com`.

A package name is a sequence of at least one Staq identifier, separated by slashes.

A module name is a Staq identifier, or empty for the top-level module of a package.

There is a top-level module associated with every package, which is public and has no further
module name.  A package may have additional public and private modules, which have names.

The top-level module for (for example) a package with organization name "staq.us" and package name
"examples/chat" is defined in a file `staq.us/examples/chat.stq`, which must begin with a package
declaration.

An additional public or private module for the above example with module name "mockclient" is
defined in a file `staq.us/examples/chat/mockclient.stq`, which must begin with a module
declaration.