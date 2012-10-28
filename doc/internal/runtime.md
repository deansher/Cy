Cy Runtime
==========

Cy compiles to two different JavaScript environments: Node.js or browser.


Compiling to Node.js
--------------------

The Cy compiler generates Node JavaScript in a `node-out` subdirectory parallel to the root `src`
directory of a package. The `node-out` directory is then suitable for placing on your `NODE_PATH`.
Within `node-out`, the path elements are:
* `__cy`
* the organization name
* the package name (multiple path elements)
* the package version (as the leaf directory)
* a `.js` file for each module, called either `modulename.js` or the literal `__package.js` for the top-level module of a package

For example, if organization is "cy-platform.org", the package name is "examples/chat", and the version is 1.0.0, then the path of the top-level package output is 

  node-out/__cy/cy-platform.org/examples/chat/1.0.0/__package.js

The path of a module called "mockclient" within that package is

  node-out/__cy/cy-platform.org/examples/chat/1.0.0/mockclient.js

Within the directory corresponding to a package (such as the `chat` directory above), the Cy
compiler updates symbolic links for the version dependency specs for the new version itself and all
previous versions obsoleted by the new version. Continuing the example above, 1.2.3.js would become
the symbolic-link target for 1.2.3+.js, and would become the new target for symbolic links
1.2.2+.js, `1.2.1+.js`, `1.2.0+.js`, `1.1.97+.js`, etc., to the extent those previous versions
existed.  Note that a version with a later major version number never obsoletes any versions with
prior major version numbers > 0.  For example, version 1.0.0 obsoletes every version 0.y.z, but
version 2.0.0 doesn't obsolete any version 1.y.z.

Each module's `.js` output is a Node module with exactly the same exported names as
the Cy module, plus the following special names:

__cy_source_md5: The md5 hash of the source file that generated the module.

__cy_contract_md5: The md5 hash of the contract file that specifies the module.

The rest of the correspondence between JavaScript and Cy is specified independently of Node versus
browser, in the rest of this document.


JavaScript Representation of Simple Cy Types
--------------------------------------------

Cy String -> string

all Cy numeric types -> number


JavaScript Representation of Cy Object
--------------------------------------


JavaScript Representation of Cy Component
-----------------------------------------
