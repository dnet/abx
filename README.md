Android Binary XML toolbox
==========================

Dependencies
------------

 - Erlang R16 or newer (R15 lacks at least `binary_to_integer`)

Installing and building
-----------------------

	$ git clone https://github.com/dnet/abx.git
	...
	$ cd abx
	$ erlc *.erl

Usage
-----

`abx.sh` can be invoked from the shell and can be used to transform XMLs, an
example can be seen below that enables on-device debugging.

	$ unzip foo.apk AndroidManifest.xml
	Archive:  foo.apk
	  inflating: AndroidManifest.xml
	$ ./abx.sh AndroidManifest.xml
	Self-test succeeded, proceed with injection
	$ ./abx.sh AndroidManifest.xml out.xml application.debuggable=true

The resulting XML can be repackaged into the APK, but then you'll need to
sign the package again, here's a [blogpost telling you how][1]. If you need
more flexibility and know some Erlang, the following modules can be used.

 - `abx_parser` transforms the binary XML to simple Erlang terms
   - `parse_file` parses a file specified by name
   - `parse_binary` does the same, but directly on an Erlang binary
 - `abx_serializer` serializes the Erlang terms to binary XML format
   - `serialize_to_file` writes the output to a file
   - `serialize_to_binary` returns the output as an Erlang binary
 - `abx_transform` compiles and applies transformations on Erlang terms

The Erlang term structure is a list, in which each item matches a chunk in
the Android Binary XML format. The list items are tuples, the first element
is always an atom that tags the contents. Currently, three such tags are
supported.

 - `{string_pool, [String1, String2, ...]}` represents a string pool, the
   second member is a list of binaries; the order matches the original chunk
 - `{element, LineNum, Comment, Namespace, Name, [{Ns, Name, Value}, ...]}`
   represents the start of an XML element with zero or more attributes as
   tuples of three elements. Namespaces and names are represented as binaries,
   values are represented as the following.
   - booleans follow Erlang convention by using `true` and `false` atoms
   - decimal numbers are Erlang integers
   - hexadecimal numbers are tagged tuples `{hex, Number}`
   - references are tagged tuples `{ref, Number}`
 - `{unknown, Type, HeaderSize, Payload}` represents every other chunk

License
-------

The whole project is available under MIT license, see `LICENSE.txt`.

  [1]: http://blog.silentsignal.eu/2014/04/04/quick-and-dirty-android-binary-xml-edits/
