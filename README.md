<a name='x-28MICMAC-3A-40MICMAC-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# Micmac Manual

## Table of Contents

- [1 micmac ASDF System Details][be27]
- [2 Introduction][449f]
    - [2.1 Overview][ae93]
    - [2.2 Links][1a05]
- [3 Graph Search][a6cc]
    - [3.1 UCT][ca85]
- [4 Metropolis Hastings][670f]
- [5 Game Theory][724d]

###### \[in package MICMAC\]
<a name='x-28-22micmac-22-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 micmac ASDF System Details

- Version: 0.0.2
- Description: Micmac is mainly a library of graph search algorithms
  such as alpha-beta, UCT and beam search, but it also has some MCMC
  and other slightly unrelated stuff.
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)
- Homepage: [http://quotenil.com](http://quotenil.com)

<a name='x-28MICMAC-3A-40MICMAC-INTRODUCTION-20MGL-PAX-3ASECTION-29'></a>

## 2 Introduction

<a name='x-28MICMAC-3A-40MICMAC-OVERVIEW-20MGL-PAX-3ASECTION-29'></a>

### 2.1 Overview

MICMAC is a Common Lisp library by [Gábor
Melis](http://quotenil.com) focusing on [graph
search](http://en.wikipedia.org/wiki/Graph_traversal) algorithms.

<a name='x-28MICMAC-3A-40MICMAC-LINKS-20MGL-PAX-3ASECTION-29'></a>

### 2.2 Links

Here is the [official
repository](https://github.com/melisgl/micmac) and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/micmac-manual.html)
for the latest version.

<a name='x-28MICMAC-3A-40MICMAC-GRAPH-SEARCH-20MGL-PAX-3ASECTION-29'></a>

## 3 Graph Search

<a name='x-28MICMAC-3AALPHA-BETA-20FUNCTION-29'></a>

- [function] **ALPHA-BETA** *STATE &KEY (DEPTH 0) ALPHA BETA CALL-WITH-ACTION MAYBE-EVALUATE-STATE LIST-ACTIONS RECORD-BEST*

    Alpha-beta pruning for two player, zero-sum maximax (like minimax
    but both players maximize and the score is negated when passed
    between depths). Return the score of the game `STATE` from the point
    of view of the player to move at `DEPTH` and as the second value the
    list of actions of the principal variant.
    
    `CALL-WITH-ACTION` is a function of (`STATE` `DEPTH` `ACTION` FN). It
    carries out `ACTION` (returned by `LIST-ACTIONS` or `NIL`) to get the
    state corresponding to `DEPTH` and calls FN with that state. It may
    destructively modify `STATE` provided it undoes the damage after FN
    returns. `CALL-WITH-ACTION` is called with `NIL` as `ACTION` for the root
    of the tree, in this case `STATE` need not be changed. FN returns the
    same kinds of values as [`ALPHA-BETA`][d660]. They may be useful for logging.
    
    `MAYBE-EVALUATE-STATE` is a function of (`STATE` `DEPTH`). If `STATE` at
    `DEPTH` is a terminal node then it returns the score from the point of
    view of the player to move and as the second value a list of actions
    that lead from `STATE` to the position that was evaluated. The list of
    actions is typically empty. If we are not at a terminal node then
    `MAYBE-EVALUATE-STATE` returns `NIL`.
    
    `LIST-ACTIONS` is a function of (`STATE` `DEPTH`) and returns a non-empty
    list of legal candidate moves for non-terminal nodes. Actions are
    tried in the order `LIST-ACTIONS` returns them: stronger moves
    
    `CALL-WITH-ACTION`, `MAYBE-EVALUATE-STATE` and `LIST-ACTIONS` are
    mandatory.
    
    `RECORD-BEST`, if non-NIL, is a function of (`DEPTH` `SCORE` `ACTIONS`). It
    is called when at `DEPTH` a new best action is found. `ACTIONS` is a
    list of all the actions in the principle variant corresonding to the
    newly found best score. `RECORD-BEST` is useful for graceful
    degradation in case of timeout.
    
    `ALPHA` and `BETA` are typically `NIL` (equivalent to -infinity,
    +infinity) but any real number is allowed if the range of scores can
    be boxed.
    
    See `test/test-alpha-beta.lisp` for an example.

<a name='x-28MICMAC-3ABEAM-SEARCH-20FUNCTION-29'></a>

- [function] **BEAM-SEARCH** *START-NODES &KEY MAX-DEPTH (N-SOLUTIONS 1) (BEAM-WIDTH (LENGTH START-NODES)) EXPAND-NODE-FN EXPAND-BEAM-FN SCORE-FN UPPER-BOUND-FN SOLUTIONP-FN (FINISHEDP-FN SOLUTIONP-FN)*

    In a graph, search for nodes that with the best scores with [beam
    search](http://en.wikipedia.org/wiki/Beam_search). That is, starting
    from `START-NODES` perform a breadth-first search but at each depth
    only keep `BEAM-WIDTH` number of nodes with the best scores. Keep the
    best `N-SOLUTIONS` (at most) complete solutions. Discard nodes known
    to be unable to get into the best `N-SOLUTIONS` (due to
    `UPPER-BOUND-FN`). Finally, return the solutions and the active
    nodes (the *beam*) as adjustable arrays sorted by score in
    descending order.
    
    `START-NODES` (a sequence of elements of arbitrary type). `SCORE-FN`,
    `UPPER-BOUND-FN`, `SOLUTIONP-FN`, `FINISHEDP-FN` are all functions of one
    argument: the node. `SOLUTIONP-FN` checks whether a node represents a
    complete solution (i.e. some goal is reached). `SCORE-FN` returns a
    real number that's to be maximized, it's only called for node for
    which `SOLUTIONP-FN` returned true. `UPPER-BOUND-FN` (if not `NIL`)
    returns a real number that equal or greater than the score of all
    solutions reachable from that node. `FINISHEDP-FN` returns true iff
    there is nowhere to go from the node.
    
    `EXPAND-NODE-FN` is also a function of a single node argument. It
    returns a sequence of nodes to 'one step away' from its argument
    node. `EXPAND-BEAM-FN` is similar, but it takes a vector of nodes and
    returns all nodes one step away from any of them. It's enough
    provide either `EXPAND-NODE-FN` or `EXPAND-BEAM-FN`. The purpose of
    `EXPAND-BEAM-FN`. is to allow more efficient, batch-like operations.
    
    See `test/test-beam-search.lisp` for an example.

<a name='x-28MICMAC-3APARALLEL-BEAM-SEARCH-20FUNCTION-29'></a>

- [function] **PARALLEL-BEAM-SEARCH** *START-NODE-SEQS &KEY MAX-DEPTH (N-SOLUTIONS 1) BEAM-WIDTH EXPAND-NODE-FN EXPAND-BEAMS-FN SCORE-FN UPPER-BOUND-FN SOLUTIONP-FN (FINISHEDP-FN SOLUTIONP-FN)*

    This is very much like [`BEAM-SEARCH`][46a1] except it solves a number of
    instances of the same search problem starting from different sets of
    nodes. The sole purpose of [`PARALLEL-BEAM-SEARCH`][5d68] is to amortize the
    cost `EXPAND-BEAM-FN` if possible.
    
    `EXPAND-BEAMS-FN` is called with sequence of beams (i.e. it's a
    sequence of sequence of nodes) and it must return another sequence
    of sequences of nodes. Each element of the returned sequence is the
    reachable nodes of the nodes in the corresponding element of its
    argument sequence.
    
    [`PARALLEL-BEAM-SEARCH`][5d68] returns a sequence of solutions sequences, and
    a sequence of active node sequences.
    
    See `test/test-beam-search.lisp` for an example.

<a name='x-28MICMAC-2EUCT-3A-40MICMAC-UCT-20MGL-PAX-3ASECTION-29'></a>

### 3.1 UCT

###### \[in package MICMAC.UCT\]
[`UCT`][fb6d] Monte Carlo tree search. This is what makes current Go programs
tick. And Hex programs as well, for that matter. This is a cleanup
and generalization of code originally created in course of the
Google AI Challenge 2010.

For now, the documentation is just a reference. See
`test/test-uct.lisp` for an example.

<a name='x-28MICMAC-2EUCT-3AUCT-NODE-20CLASS-29'></a>

- [class] **UCT-NODE**

    A node in the [`UCT`][fb6d] tree. Roughly translates to a
    state in the search space. Note that the state itself is not stored
    explicity, but it can be recovered by \`replaying' the actions from
    the starting state or by customizing [`MAKE-UCT-NODE`][f36e].

<a name='x-28MICMAC-2EUCT-3ADEPTH-20-28MGL-PAX-3AREADER-20MICMAC-2EUCT-3AUCT-NODE-29-29'></a>

- [reader] **DEPTH** *UCT-NODE* *(:DEPTH = 0)*

<a name='x-28MICMAC-2EUCT-3AEDGES-20-28MGL-PAX-3AACCESSOR-20MICMAC-2EUCT-3AUCT-NODE-29-29'></a>

- [accessor] **EDGES** *UCT-NODE*

    Outgoing edges.

<a name='x-28MICMAC-2EUCT-3AAVERAGE-REWARD-20-28MGL-PAX-3AACCESSOR-20MICMAC-2EUCT-3AUCT-NODE-29-29'></a>

- [accessor] **AVERAGE-REWARD** *UCT-NODE* *(:AVERAGE-REWARD = 0)*

    Average reward over random playouts started from
    below this node. See [`UPDATE-UCT-STATISTICS`][8e5c] and REWARD.

<a name='x-28MICMAC-2EUCT-3AUCT-EDGE-20CLASS-29'></a>

- [class] **UCT-EDGE**

    An edge in the [`UCT`][fb6d] tree. Represents an action taken
    from a state. The value of an action is the value of its target
    state which is not quite as generic as it could be; feel free to
    specialize [`AVERAGE-REWARD`][1acd] for the edges if that's not the case.

<a name='x-28MICMAC-2EUCT-3AACTION-20-28MGL-PAX-3AREADER-20MICMAC-2EUCT-3AUCT-EDGE-29-29'></a>

- [reader] **ACTION** *UCT-EDGE* *(:ACTION)*

    The action represented by the edge.

<a name='x-28MICMAC-2EUCT-3AFROM-NODE-20-28MGL-PAX-3AACCESSOR-20MICMAC-2EUCT-3AUCT-EDGE-29-29'></a>

- [accessor] **FROM-NODE** *UCT-EDGE* *(:FROM-NODE)*

    The node this edge starts from.

<a name='x-28MICMAC-2EUCT-3ATO-NODE-20-28MGL-PAX-3AACCESSOR-20MICMAC-2EUCT-3AUCT-EDGE-29-29'></a>

- [accessor] **TO-NODE** *UCT-EDGE* *(= NIL)*

    The node this edge points to if the edge has been
    visited or `NIL`.

<a name='x-28MICMAC-2EUCT-3AVISITED-EDGES-20FUNCTION-29'></a>

- [function] **VISITED-EDGES** *NODE*

<a name='x-28MICMAC-2EUCT-3AUNVISITED-EDGES-20FUNCTION-29'></a>

- [function] **UNVISITED-EDGES** *NODE*

<a name='x-28MICMAC-2EUCT-3AEDGE-SCORE-20GENERIC-FUNCTION-29'></a>

- [generic-function] **EDGE-SCORE** *NODE EDGE EXPLORATION-BIAS*

<a name='x-28MICMAC-2EUCT-3ASELECT-EDGE-20GENERIC-FUNCTION-29'></a>

- [generic-function] **SELECT-EDGE** *NODE EXPLORATION-BIAS*

    Choose an action to take from a state, in other
    words an edge to follow from `NODE` in the tree. The default
    implementation chooses randomly from the yet unvisited edges or if
    there is none moves down the edge with the maximum [`EDGE-SCORE`][abee]. If
    you are thinking of customizing this, for example to make it choose
    the minimum at odd depths, the you may want to consider specializing
    REWARD or [`UPDATE-UCT-STATISTICS`][8e5c] instead.

<a name='x-28MICMAC-2EUCT-3AOUTCOME--3EREWARD-20GENERIC-FUNCTION-29'></a>

- [generic-function] **OUTCOME-\>REWARD** *NODE OUTCOME*

    Compute the reward for a node in the tree from
    `OUTCOME` that is the result of a playout. This is called by the
    default implementation of [`UPDATE-UCT-STATISTICS`][8e5c]. This is where one
    typically negates depending on the parity of [`DEPTH`][3361] in two player
    games.

<a name='x-28MICMAC-2EUCT-3AUPDATE-UCT-STATISTICS-20GENERIC-FUNCTION-29'></a>

- [generic-function] **UPDATE-UCT-STATISTICS** *ROOT PATH OUTCOME*

    Increment the number of visits and update the
    average reward in nodes and edges of `PATH`. By default, edges simply
    get their visit counter incremented while nodes also get an update
    to [`AVERAGE-REWARD`][1acd] based on what [`OUTCOME->REWARD`][4441] returns.

<a name='x-28MICMAC-2EUCT-3AMAKE-UCT-NODE-20GENERIC-FUNCTION-29'></a>

- [generic-function] **MAKE-UCT-NODE** *PARENT EDGE PARENT-STATE*

    Create a node representing the state of that `EDGE`
    leads to from `PARENT`. Specialize this if you want to keep track of
    the state which is not done by default as it can be expensive,
    especially in light of TAKE-ACTION mutating it. The default
    implementation simply creates an instance of the class of `PARENT` so
    that one can start from a subclass of [`UCT-NODE`][8ccb] and be sure that that
    class is going to be used for nodes below it.

<a name='x-28MICMAC-2EUCT-3ASTATE-20GENERIC-FUNCTION-29'></a>

- [generic-function] **STATE** *NODE PARENT EDGE PARENT-STATE*

    Return the state that corresponds to `NODE`. This is
    not a straightforward accessor unless `NODE` is customized to store
    it. The rest of the parameters are provided so that one can
    reconstruct the state by taking the action of `EDGE` in the
    `PARENT-STATE` of `PARENT`. It's okay to destroy `PARENT-STATE` in the
    process as long as it's not stored elsewhere. This function must be
    customized.

<a name='x-28MICMAC-2EUCT-3ALIST-EDGES-20GENERIC-FUNCTION-29'></a>

- [generic-function] **LIST-EDGES** *NODE STATE*

    Return a list of edges representing the possible
    actions from `NODE` with `STATE`. This function must be customized.

<a name='x-28MICMAC-2EUCT-3APLAY-OUT-20GENERIC-FUNCTION-29'></a>

- [generic-function] **PLAY-OUT** *NODE STATE REVERSE-PATH*

    Play a random game from `NODE` with `STATE` and return
    the outcome that's fed into [`UPDATE-UCT-STATISTICS`][8e5c]. The way the
    random game is played is referred to as \`default policy' and that's
    what makes or breaks [`UCT`][fb6d] search. This function must be
    customized.

<a name='x-28MICMAC-2EUCT-3AUCT-20FUNCTION-29'></a>

- [function] **UCT** *&KEY ROOT FRESH-ROOT-STATE EXPLORATION-BIAS MAX-N-PLAYOUTS*

    Starting from the `ROOT` node search the tree expanding it one node
    for each playout. Finally return the mutated `ROOT`. `ROOT` may be the
    root node of any tree, need not be a single node with no edges.
    `FRESH-ROOT-STATE` is a function that returns a fresh state
    corresponding to `ROOT`. This state will be destroyed unless special
    care is taken in [`STATE`][e109].

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3A-40MICMAC-METROPOLIS-HASTINGS-20MGL-PAX-3ASECTION-29'></a>

## 4 Metropolis Hastings

###### \[in package MICMAC.METROPOLIS-HASTINGS\]
Generic interface for the Metropolis-Hastings algorithm, also
Metropolis Coupled MCMC.

References:

- http://en.wikipedia.org/wiki/Metropolis–Hastings\_algorithm

- Markov Chain Monte Carlo and Gibbs Sampling
  Lecture Notes for EEB 581, version 26 April 2004 c B. Walsh 2004
  http://web.mit.edu/~wingated/www/introductions/mcmc-gibbs-intro.pdf

- Geyer, C.J. (1991) Markov chain Monte Carlo maximum likelihood

For now, the documentation is just a reference. See
`test/test-metropolis-hastings.lisp` for an example.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3AMC-CHAIN-20CLASS-29'></a>

- [class] **MC-CHAIN**

    A simple markov chain for Metropolis Hastings. With
    temperature it is suitable for `MC3`.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3ATEMPERATURE-20-28MGL-PAX-3AACCESSOR-20MICMAC-2EMETROPOLIS-HASTINGS-3AMC-CHAIN-29-29'></a>

- [accessor] **TEMPERATURE** *MC-CHAIN* *(:TEMPERATURE = 1.0d0)*

    The PROBABILITY-RATIO of samples is raised to the
    power of 1 / `TEMPERATURE` before calculating the acceptance
    probability. This effectively flattens the peaks if `TEMPERATURE` >
    1 which makes it easier for the chain to traverse deep valleys.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3ASTATE-20-28MGL-PAX-3AREADER-20MICMAC-2EMETROPOLIS-HASTINGS-3AMC-CHAIN-29-29'></a>

- [reader] **STATE** *MC-CHAIN* *(:STATE)*

    This is the current sample where the chain is.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3AJUMP-TO-SAMPLE-20FUNCTION-29'></a>

- [function] **JUMP-TO-SAMPLE** *CHAIN JUMP &KEY (RESULT-SAMPLE (STATE CHAIN))*

    From the current state of `CHAIN` make `JUMP` (from the current
    distribution of `CHAIN`) and return the sample where we landed. Reuse
    `RESULT-SAMPLE` when possible. 

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3AJUMP-TO-SAMPLE-2A-20GENERIC-FUNCTION-29'></a>

- [generic-function] **JUMP-TO-SAMPLE\*** *CHAIN JUMP RESULT-SAMPLE*

    This function is called by [`JUMP-TO-SAMPLE`][2acc]. It is
    where [`JUMP-TO-SAMPLE`][2acc] behaviour shall be customized.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3APREPARE-JUMP-DISTRIBUTION-20GENERIC-FUNCTION-29'></a>

- [generic-function] **PREPARE-JUMP-DISTRIBUTION** *CHAIN*

    Prepare for sampling from the F(X) = Q(SAMPLE->X)
    distribution. Called by [`RANDOM-JUMP`][499c]. The around method ensures that
    nothing is done unless there was a state change.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3ARANDOM-JUMP-20GENERIC-FUNCTION-29'></a>

- [generic-function] **RANDOM-JUMP** *CHAIN*

    Sample a jump from the current distribution of
    jumps that was computed by [`PREPARE-JUMP-DISTRIBUTION`][0ded].

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3ALOG-PROBABILITY-RATIO-20GENERIC-FUNCTION-29'></a>

- [generic-function] **LOG-PROBABILITY-RATIO** *CHAIN SAMPLE1 SAMPLE2*

    Return P(`SAMPLE1`)/P(`SAMPLE2`). It's in the log
    domain to avoid overflows and the ratio part is because that it may
    allow computational shortcuts as opposed to calculating unnormalized
    probabilities separately.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3ALOG-PROBABILITY-RATIO-TO-JUMP-TARGET-20GENERIC-FUNCTION-29'></a>

- [generic-function] **LOG-PROBABILITY-RATIO-TO-JUMP-TARGET** *CHAIN JUMP TARGET*

    Return P(`TARGET`)/P([`STATE`][39e2]) where `JUMP` is from the
    current state of `CHAIN` to `TARGET` sample. This can be specialized for
    speed. The default implementation just falls back on
    [`LOG-PROBABILITY-RATIO`][8adf].

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3ALOG-JUMP-PROBABILITY-RATIO-20GENERIC-FUNCTION-29'></a>

- [generic-function] **LOG-JUMP-PROBABILITY-RATIO** *CHAIN JUMP TARGET*

    Return Q(TARGET->STATE) / Q(STATE->TARGET) where Q
    is the jump distribution and `JUMP` is from the current [`STATE`][39e2] of `CHAIN`
    to `TARGET` sample.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3AACCEPTANCE-PROBABILITY-20GENERIC-FUNCTION-29'></a>

- [generic-function] **ACCEPTANCE-PROBABILITY** *CHAIN JUMP CANDIDATE*

    Calculate the acceptance probability of `CANDIDATE`
    to which `JUMP` leads from the current [`STATE`][39e2] of `CHAIN`.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3AACCEPT-JUMP-20GENERIC-FUNCTION-29'></a>

- [generic-function] **ACCEPT-JUMP** *CHAIN JUMP CANDIDATE*

    Called when `CHAIN` accepts `JUMP` to `CANDIDATE`.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3AREJECT-JUMP-20GENERIC-FUNCTION-29'></a>

- [generic-function] **REJECT-JUMP** *CHAIN JUMP CANDIDATE*

    Called when `CHAIN` rejects `JUMP` to `CANDIDATE`. It
    does nothing by default, it's just a convenience for debugging.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3AMAYBE-JUMP-20GENERIC-FUNCTION-29'></a>

- [generic-function] **MAYBE-JUMP** *CHAIN JUMP CANDIDATE ACCEPTANCE-PROBABILITY*

    Randomly accept or reject `JUMP` to `CANDIDATE` from
    the current state of `CHAIN` with `ACCEPTANCE-PROBABILITY`.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3AJUMP-20GENERIC-FUNCTION-29'></a>

- [generic-function] **JUMP** *CHAIN*

    Take a step on the markov chain. Return a boolean
    indicating whether the proposed jump was accepted.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3AMC3-CHAIN-20CLASS-29'></a>

- [class] **MC3-CHAIN** *MC-CHAIN*

    High probability island separated by low valley
    make the chain poorly mixing. `MC3-CHAIN` has a number of `HOT-CHAINS`
    that have state probabilities similar to that of the main chain but
    less jagged. Often it suffices to set the temperatures of the
    `HOT-CHAINS` higher use the very same base probability
    distribution.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3AACCEPT-SWAP-CHAIN-STATES-20GENERIC-FUNCTION-29'></a>

- [generic-function] **ACCEPT-SWAP-CHAIN-STATES** *MC3 CHAIN1 CHAIN2*

    Swap the states of `CHAIN1` and `CHAIN2` of `MC3`.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3AREJECT-SWAP-CHAIN-STATES-20GENERIC-FUNCTION-29'></a>

- [generic-function] **REJECT-SWAP-CHAIN-STATES** *MC3 CHAIN1 CHAIN2*

    Called when the swap of states of `CHAIN1` and `CHAIN2`
    is rejected. It does nothing by default, it's just a convenience for
    debugging.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3AMAYBE-SWAP-CHAIN-STATES-20GENERIC-FUNCTION-29'></a>

- [generic-function] **MAYBE-SWAP-CHAIN-STATES** *MC3 CHAIN1 CHAIN2 ACCEPTANCE-PROBABILITY*

    Swap of states of `CHAIN1` and `CHAIN2` of `MC3` with
    `ACCEPTANCE-PROBABILITY`.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3AJUMP-BETWEEN-CHAINS-20GENERIC-FUNCTION-29'></a>

- [generic-function] **JUMP-BETWEEN-CHAINS** *MC3*

    Choose two chains randomly and swap their states
    with `MC3` acceptance probability.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3AENUMERATING-CHAIN-20CLASS-29'></a>

- [class] **ENUMERATING-CHAIN** *MC-CHAIN*

    A simple abstract chain subclass that explicitly
    enumerates the probabilities of the distribution.

<a name='x-28MICMAC-2EMETROPOLIS-HASTINGS-3ATRACING-CHAIN-20CLASS-29'></a>

- [class] **TRACING-CHAIN**

    Mix this in with your chain to have it print trace
    of acceptances/rejections.

<a name='x-28MICMAC-2EGAME-THEORY-3A-40MICMAC-GAME-THEORY-20MGL-PAX-3ASECTION-29'></a>

## 5 Game Theory

###### \[in package MICMAC.GAME-THEORY\]
<a name='x-28MICMAC-2EGAME-THEORY-3AFIND-NASH-EQUILIBRIUM-20FUNCTION-29'></a>

- [function] **FIND-NASH-EQUILIBRIUM** *PAYOFF &KEY (N-ITERATIONS 100)*

    Find a Nash equilibrium of a zero-sum game represented by `PAYOFF`
    matrix (a 2d matrix or a nested list). `PAYOFF` is from the point of
    view of the row player: the player who choses column wants to
    minimize, the row player wants to maximize. The first value returned
    is a vector of unnormalized probabilities assigned to each action of
    the row player, the second value is the same for the column player
    and the third is the expected payoff of the row player in the nash
    equilibrium represented by the oddment vectors.

  [0ded]: #x-28MICMAC-2EMETROPOLIS-HASTINGS-3APREPARE-JUMP-DISTRIBUTION-20GENERIC-FUNCTION-29 "(MICMAC.METROPOLIS-HASTINGS:PREPARE-JUMP-DISTRIBUTION GENERIC-FUNCTION)"
  [1a05]: #x-28MICMAC-3A-40MICMAC-LINKS-20MGL-PAX-3ASECTION-29 "(MICMAC:@MICMAC-LINKS MGL-PAX:SECTION)"
  [1acd]: #x-28MICMAC-2EUCT-3AAVERAGE-REWARD-20-28MGL-PAX-3AACCESSOR-20MICMAC-2EUCT-3AUCT-NODE-29-29 "(MICMAC.UCT:AVERAGE-REWARD (MGL-PAX:ACCESSOR MICMAC.UCT:UCT-NODE))"
  [2acc]: #x-28MICMAC-2EMETROPOLIS-HASTINGS-3AJUMP-TO-SAMPLE-20FUNCTION-29 "(MICMAC.METROPOLIS-HASTINGS:JUMP-TO-SAMPLE FUNCTION)"
  [3361]: #x-28MICMAC-2EUCT-3ADEPTH-20-28MGL-PAX-3AREADER-20MICMAC-2EUCT-3AUCT-NODE-29-29 "(MICMAC.UCT:DEPTH (MGL-PAX:READER MICMAC.UCT:UCT-NODE))"
  [39e2]: #x-28MICMAC-2EMETROPOLIS-HASTINGS-3ASTATE-20-28MGL-PAX-3AREADER-20MICMAC-2EMETROPOLIS-HASTINGS-3AMC-CHAIN-29-29 "(MICMAC.METROPOLIS-HASTINGS:STATE (MGL-PAX:READER MICMAC.METROPOLIS-HASTINGS:MC-CHAIN))"
  [4441]: #x-28MICMAC-2EUCT-3AOUTCOME--3EREWARD-20GENERIC-FUNCTION-29 "(MICMAC.UCT:OUTCOME->REWARD GENERIC-FUNCTION)"
  [449f]: #x-28MICMAC-3A-40MICMAC-INTRODUCTION-20MGL-PAX-3ASECTION-29 "(MICMAC:@MICMAC-INTRODUCTION MGL-PAX:SECTION)"
  [46a1]: #x-28MICMAC-3ABEAM-SEARCH-20FUNCTION-29 "(MICMAC:BEAM-SEARCH FUNCTION)"
  [499c]: #x-28MICMAC-2EMETROPOLIS-HASTINGS-3ARANDOM-JUMP-20GENERIC-FUNCTION-29 "(MICMAC.METROPOLIS-HASTINGS:RANDOM-JUMP GENERIC-FUNCTION)"
  [5d68]: #x-28MICMAC-3APARALLEL-BEAM-SEARCH-20FUNCTION-29 "(MICMAC:PARALLEL-BEAM-SEARCH FUNCTION)"
  [670f]: #x-28MICMAC-2EMETROPOLIS-HASTINGS-3A-40MICMAC-METROPOLIS-HASTINGS-20MGL-PAX-3ASECTION-29 "(MICMAC.METROPOLIS-HASTINGS:@MICMAC-METROPOLIS-HASTINGS MGL-PAX:SECTION)"
  [724d]: #x-28MICMAC-2EGAME-THEORY-3A-40MICMAC-GAME-THEORY-20MGL-PAX-3ASECTION-29 "(MICMAC.GAME-THEORY:@MICMAC-GAME-THEORY MGL-PAX:SECTION)"
  [8adf]: #x-28MICMAC-2EMETROPOLIS-HASTINGS-3ALOG-PROBABILITY-RATIO-20GENERIC-FUNCTION-29 "(MICMAC.METROPOLIS-HASTINGS:LOG-PROBABILITY-RATIO GENERIC-FUNCTION)"
  [8ccb]: #x-28MICMAC-2EUCT-3AUCT-NODE-20CLASS-29 "(MICMAC.UCT:UCT-NODE CLASS)"
  [8e5c]: #x-28MICMAC-2EUCT-3AUPDATE-UCT-STATISTICS-20GENERIC-FUNCTION-29 "(MICMAC.UCT:UPDATE-UCT-STATISTICS GENERIC-FUNCTION)"
  [a6cc]: #x-28MICMAC-3A-40MICMAC-GRAPH-SEARCH-20MGL-PAX-3ASECTION-29 "(MICMAC:@MICMAC-GRAPH-SEARCH MGL-PAX:SECTION)"
  [abee]: #x-28MICMAC-2EUCT-3AEDGE-SCORE-20GENERIC-FUNCTION-29 "(MICMAC.UCT:EDGE-SCORE GENERIC-FUNCTION)"
  [ae93]: #x-28MICMAC-3A-40MICMAC-OVERVIEW-20MGL-PAX-3ASECTION-29 "(MICMAC:@MICMAC-OVERVIEW MGL-PAX:SECTION)"
  [be27]: #x-28-22micmac-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "(\"micmac\" ASDF/SYSTEM:SYSTEM)"
  [ca85]: #x-28MICMAC-2EUCT-3A-40MICMAC-UCT-20MGL-PAX-3ASECTION-29 "(MICMAC.UCT:@MICMAC-UCT MGL-PAX:SECTION)"
  [d660]: #x-28MICMAC-3AALPHA-BETA-20FUNCTION-29 "(MICMAC:ALPHA-BETA FUNCTION)"
  [e109]: #x-28MICMAC-2EUCT-3ASTATE-20GENERIC-FUNCTION-29 "(MICMAC.UCT:STATE GENERIC-FUNCTION)"
  [f36e]: #x-28MICMAC-2EUCT-3AMAKE-UCT-NODE-20GENERIC-FUNCTION-29 "(MICMAC.UCT:MAKE-UCT-NODE GENERIC-FUNCTION)"
  [fb6d]: #x-28MICMAC-2EUCT-3AUCT-20FUNCTION-29 "(MICMAC.UCT:UCT FUNCTION)"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
