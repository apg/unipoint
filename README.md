unipoint-mode is a simple way to insert common mathematical unicode codepoints
into an Emacs buffer.

To use:

`$ cp unipoint.el /path/to/somewhere/in/load-path`

From Emacs:

`M-: (require 'unipoint)`
`M-x unipoint-mode`

Or, add it to your .emacs

`(require 'unipoint)`

This will bind C-\ to `unipoint-insert` which allows you to type a TeX
symbol and get a codepoint. Or, change an already type TeX symbol name into
its unicode representation.

This idea was inspired by an email from a friend:

> DrRacket lets you enter common Unicode characters by typing their
> LaTeX name followed by control-\. For example, you type ∈ by typing
> "\", "i", "n", "control-\". It's easy enough that I use Unicode in my
> code all the time.

See TUTORIAL.txt for more information.
