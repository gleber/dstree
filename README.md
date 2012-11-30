*DRAFT*

# Distributed spanning tree #

This simple implementation is based on token-passing paradigm with
optimistic approach to failure handling. 

# Building #

To build everything:
- `rebar compile`
or
- `make`

To run tests:
- `rebar eunit`
or
- `make tests`

To run shell:
- `make sh`

# Reliability #

System has been tested using Proper/Quickcheck tests based on the
following approach:

1. randomly create a graph with few cycles
2. start up as many processes as there are nodes in the graph
3. establish connections between processes according to edges in the
graph
4. (optional) kill some of processes
5. start distributed spanning tree search procedure
6. check resulting spanning tree to be
   - consistent between all processes in the graph
   - done in limited amount of time

System is able to handle around 800 tests before starting to fail when
there are around 30+ nodes in the graph. Looks like that more
sophisticated testing procedure (McErlang, PULSE or Concuerror) is
needed to find the bug.

# Implementation #

System uses four message types:

1. forward
2. return
3. finished
4. working

## Messages ##

### Forward ###

Forward message denotes a token which traverses the graph in forward
direction in the algorithm. It carries the following information:
- origin - root of the tree
- visited - list of visited nodes
- sender - last sender of the token

### Return ###

Return message denotes a token which traverses the graph backwards in
the algorithm. It carries the following information:
- origin - root of the tree
- visited - list of visited nodes
- sender - last sender of the token
- subtree - a subtree of the constructed spanning tree which is
  defined by the route of the token

### Forward ###

Finished message is a message which confirms that a spanning tree
algorithm has finished it's work. It carries the following
information:
- origin - root of the tree
- sender - last sender of the token
- tree - a final spanning tree constructed by the algorithm

### Working ###

Working message is used to inform parent that currently some node is
taking long time to answer, hence increasing timeout time on parent node.

## Algorithm ##

Each node in the system can be in two states:
- idle
- working
- finishing

Idle state means that this node is not participating in any process of
building of spanning tree.

Working state means that this node does participate in building of a
spanning tree.

Spanning tree building process is uniquely identified with "origin",
which is identifier of the node which started given search process (it
is assumed that given node can not initiate two search processes
concurrently).

We assume that set of node identifiers has a total ordering. Node
identifier with smaller order has higher priority.

Each node stores a "neighbor list", which is a list of nodes which
are known to this node (discovery of such nodes is out of the scope of
the project).

### Node state ###

1. If received message is FORWARD and we are in IDLE state
   - enter working state
   - save sender of the message as parent
   - add myself to visited list
   - initiate timeout procedure
   - send FORWARD token to first un-visited neighbor

2. If received message is FORWARD and we are in WORKING state
   - check if `origin` of the message is the same as in our state
   - if yes, ignore
   - if no and if `origin` of the message has higher priority,
   overwrite our own state to switch to higher priority `origin`

3. If received message is RETURN and we are in WORKING state
   - add sender to set of children
   - cancel timeout
   - send FORWARD token to first un-visited neighbor if there are
   un-visited neighbors left
   - else send RETURN message to own parent

4. If received message is TIMEOUT and we are in WORKING state
   - it means that some of the children is not responding
   - add dead child to list of visited nodes (to prevent other nodes
   from trying this child again)
   - send FORWARD token to first un-visited neighbor if there are
   un-visited neighbors left
   - else send RETURN message to own parent

### Timeout procedure ###

Whenever node sends forward message it will start timeout procedure,
which will wait for T milliseconds before treating a child as
dead. But since node's parent is waiting for the node as well, we need
to ensure that the parent won't treat node as dead. To solve this
whenever a child takes more than T/2 time to respond, we will send
`working` message to a parent. Parent will route the message to his
parent. This will avoid "timeout failure cascade" scenario.
