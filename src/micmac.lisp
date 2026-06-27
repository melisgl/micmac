(in-package :micmac)

(defsection @micmac-manual (:title "Micmac Manual")
  (micmac asdf:system)
  (@micmac-introduction section)
  (@micmac-graph-search section)
  (micmac.metropolis-hastings::@micmac-metropolis-hastings section)
  (micmac.game-theory::@micmac-game-theory section))

(defsection @micmac-introduction (:title "Introduction")
  (@micmac-overview section)
  (@micmac-links section))

(defsection @micmac-overview (:title "Overview")
  "\\MICMAC is a Common Lisp library by [Gábor
  Melis](http://quotenil.com) focusing on [graph
  search](http://en.wikipedia.org/wiki/Graph_traversal) algorithms.")

(defsection @micmac-links (:title "Links")
  "The official repository is <https://github.com/melisgl/micmac>, and
  this document in available in various formats on
  <https://fixnum.com> for the latest version.")
