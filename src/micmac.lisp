(in-package :micmac)

(defsection @micmac-manual (:title "Micmac Manual")
  (micmac asdf:system)
  (@micmac-introduction section)
  (@micmac-graph-search section)
  (micmac.metropolis-hastings:@micmac-metropolis-hastings section)
  (micmac.game-theory:@micmac-game-theory section))

(defsection @micmac-introduction (:title "Introduction")
  (@micmac-overview section)
  (@micmac-links section))

(defsection @micmac-overview (:title "Overview")
  "\\MICMAC is a Common Lisp library by [Gábor
  Melis](http://quotenil.com) focusing on [graph
  search](http://en.wikipedia.org/wiki/Graph_traversal) algorithms.")

(defsection @micmac-links (:title "Links")
  "Here is the [official
  repository](https://github.com/melisgl/micmac) and the [HTML
  documentation](http://melisgl.github.io/mgl-pax-world/micmac-manual.html)
  for the latest version.")
