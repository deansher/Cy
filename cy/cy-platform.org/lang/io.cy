module cy-platform.org/lang:io

export (NodeStdio, LineIo)

type NodeStdio = component
  port io
    output fromStdin :: String
    
    input toStdout :: String >> () handles
      d -> [javascript d|
              process.stdout.write(d);
            |]

  daemon [javaScript fromStdin|
     process.stdin.resume();
     process.stdin.setEncoding('utf8');

     process.stdin.on('data', function (chunk) {
       fromStdin(chunk);
     });
    |]

type LineIo = component

  var partialLine := ""

  port rawIo
    output toStdout :: String

    input fromStdin :: String >> () handles
      d -> if not d.empty then
             -- Notice that split is a method on the delimiter, which reverses the typical split parameter order
             -- from an OO language.  This parameter order matches Haskell, where the motivation is that 
             -- split curries nicely.  E.g. "\n".split (or, in Hasekell, split "\n") is a function that splits on newlines.
             var lines := "\n".split d

             -- This is shorthand for 
             --    lines := lines.set 0 $ partialLine ++ lines[0]
             -- given a hypothetical (non-existent) set method.
             lines[0] := partialLine ++ lines[0]
             n = lines.length -- we could equivalently say length lines
             trailingNewline = d.endsWith "\n"
             partialLine := ""
             for i <- 0..n do
               if i < n-1 || trailingNewline then
                 send console.fromStdin lines[i]
               else
                 partialLine := lines[i]

  port lineIo
    output fromStdin :: String
    
    input toStdout :: String >> () handles
      line -> send rawIo.toStdout line

