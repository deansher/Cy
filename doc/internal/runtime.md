Staq Runtime
============

Staq compiles to two different JavaScript environments: Node.js or browser.


Compiling to Node.js
--------------------

The Staq compiler generates Node JavaScript in a `node-out` subdirectory parallel to the root
`src` directory. The `node-out` directory is then suitable for placing on your `NODE_PATH`.
Each Staq module name is expanded at dot separators into a relative path in the `node-out`
subdirectory. The first element of the path is always `__staq`. The filename is the version number
in the format x.y.z plus ".js". For example, if the module is called `staq.lang.io` and has version
1.2.3, its compiled Node JavaScript output goes in `node-out/__staq/staq/lang/io/1.2.3.js`.

Within the leaf directory corresponding to each module, the Staq compiler updates symbolic links for
the version dependency specs for the new version itself and all previous versions obsoleted by the
new version. Continuing the example above, 1.2.3.js would become the symbolic-link target for
1.2.3+.js, and would become the new target for symbolic links 1.2.2+.js, `1.2.1+.js`, `1.2.0+.js`,
`1.1.97+.js`, etc., to the extent those previous versions existed.  Note that a version with
a later major version number never obsoletes any versions with prior major version numbers > 0.
For example, version 1.0.0 obsoletes every version 0.y.z, but version 2.0.0 doesn't obsolete
any version 1.y.z.

The generated `.js` file is a Node module with exactly the same exported names as the Staq module,
plus the following special names:

__staq_source_md5: The md5 hash of the source file that generated the module.

__staq_contract_md5: The md5 hash of the contract file that specifies the module.

The rest of the correspondence between JavaScript and Staq is specified independently of Node versus
browser, in the rest of this document.


JavaScript Representation of Simple Staq Types
----------------------------------------------

Staq String -> string

all Staq numeric types -> number


JavaScript Representation of Staq Object
----------------------------------------


JavaScript Representation of Staq Component
-------------------------------------------
