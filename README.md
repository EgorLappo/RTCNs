# RTCNs

Code to work with rankings of phylogenetic networks. Contact me (see [egorlappo.me](https://egorlappo.me)) if you want to use this code and need my help.

### Useful features

* Generation one-component tree-child networks (TCNs) and normal networks following [doi.org10.1016/j.jcss.2020.06.001](https://doi.org10.1016/j.jcss.2020.06.001).
* Generation all non-isomorphic TCNs and normal networks.
* Backwards-construction generation of ranked tree-child networks (RTCNs), following [onlinelibrary.wiley.com/doi/abs/10.1002/rsa.21048](https://onlinelibrary.wiley.com/doi/abs/10.1002/rsa.21048).
* Generation of all rankable TCNs as DAGs. This is essentially the same as generation of RTCNs, but we forget the order and encode the resulting graph as a DAG with nodes corresponding to branching and reticulation events.
* Converting rankable TCNs to `fgl` graphs.
