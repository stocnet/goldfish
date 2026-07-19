# List of symbols {#list-of-symbols .unnumbered}

+:--------------------------------+:-----------------------------------+
| **Network elements**                                                 |
+---------------------------------+------------------------------------+
| $X$                             | Network (multiplex or multilayer), |
|                                 | random variable                    |
+---------------------------------+------------------------------------+
| $X_i$                           | Each layer that comprises the      |
|                                 | multiplex/multilayer network       |
+---------------------------------+------------------------------------+
| $x(t)$                          | Adjacency matrix of network $X$ at |
|                                 | time $t$                           |
+---------------------------------+------------------------------------+
|                                 |                                    |
+---------------------------------+------------------------------------+
| **Relational modelling elements**                                    |
+---------------------------------+------------------------------------+
| $\boldsymbol{\omega}$           | Relational event with elements     |
|                                 | (sender, receiver, time, flavour)  |
|                                 | (weight is an optional parameter), |
|                                 | random variable                    |
+---------------------------------+------------------------------------+
| $\boldsymbol{\Omega}$           | Sequence of events, random         |
|                                 | variable                           |
+---------------------------------+------------------------------------+
| $\omega$                        | Observed event                     |
+---------------------------------+------------------------------------+
| $\Omega$                        | Observed sequence of events        |
+---------------------------------+------------------------------------+
| $\omega^*$                      | Sampled panel data event, an event |
|                                 | with one of the elements (sender,  |
|                                 | receiver, time or flavour) sampled |
+---------------------------------+------------------------------------+
| $\Omega^*$                      | Sampled sequence of of panel data  |
|                                 | events                             |
+---------------------------------+------------------------------------+
| $\omega'$                       | Event with a time proposed by the  |
|                                 | MCMC routine                       |
+---------------------------------+------------------------------------+
| $\Omega'$                       | Sequence of events proposed in the |
|                                 | MCMC routine                       |
+---------------------------------+------------------------------------+
| $\varphi$                       | Flavour of an event                |
+---------------------------------+------------------------------------+
| $y_t$                           | Process state at time $t$          |
+---------------------------------+------------------------------------+
| $f(\boldsymbol{\Omega},\theta)$ | Total probability given a sequence |
|                                 | of events (parameters omitted if   |
|                                 | there is no risk of confusion)     |
+---------------------------------+------------------------------------+
| $\tau_{i,\varphi}$              | Probability of an actor to get     |
|                                 | activated for flavour $\varphi$    |
+---------------------------------+------------------------------------+
| $p_{i,j,\varphi}$               | Probability of choosing actor $j$  |
|                                 | to receive an event of flavour     |
|                                 | $\varphi$ from actor $i$           |
+---------------------------------+------------------------------------+
|                                 |                                    |
+---------------------------------+------------------------------------+
| **Sets**                                                             |
+---------------------------------+------------------------------------+
| $\mathcal{T}$                   | Time period of observation of the  |
|                                 | network                            |
+---------------------------------+------------------------------------+
| $\mathcal{A}$                   | Set of actors                      |
+---------------------------------+------------------------------------+
| $\mathcal{S}(X,y_t)$            | Set of actors at risk to be        |
|                                 | senders for network $X$ given the  |
|                                 | process state at time $t$          |
+---------------------------------+------------------------------------+
| $\mathcal{S}_\varphi(X,y_t)$    | Set of actors at risk to be        |
|                                 | senders for network $X$ given the  |
|                                 | process state at time $t$ for      |
|                                 | flavour $\varphi$                  |
+---------------------------------+------------------------------------+
| $\mathcal{R}_\varphi(y_t)$      | Set of actors at risk to be        |
|                                 | receivers for network $X$ given    |
|                                 | the process state at time $t$ for  |
|                                 | flavour $\varphi$                  |
+---------------------------------+------------------------------------+
| $\mathcal{F}$                   | Set of all event flavours          |
+---------------------------------+------------------------------------+
| $\mathcal{F}(X)$                | Set of event flavours present in   |
|                                 | layer $X$                          |
+---------------------------------+------------------------------------+
| $\Upsilon(X_i)$                 | Set of observed changes in layer   |
|                                 | $X_i$ in the period $\mathcal{T}$  |
|                                 | (events without time)              |
+---------------------------------+------------------------------------+
|                                 |                                    |
+---------------------------------+------------------------------------+
| **MCMC elements**                                                    |
+---------------------------------+------------------------------------+
| $t^*$                           | Time sampled or time from the      |
|                                 | previous step in the MCMC routine  |
+---------------------------------+------------------------------------+
| $t'$                            | Time proposed in the MCMC routine  |
+---------------------------------+------------------------------------+

# Set-up
For simplicity, we will assume that the multiplex network is conformed by two layers, 
one collected as relational event (RE) data $X_1$, and the other observed via surveys, $X_2$, with unobserved changes as panel data events (PE).
This is generalizable any amount of layers. We also assume, for simplicity that only one
of $X_2$ is observed.

# Augmentation routines: 
The simulated/sampled PE sequences will be conditioned to start and end
in the observed networks. Therefore, only the observed events will be
considered in the sampling, constraining the risk set at each step of
the routine. 

## Random sampling of panel data events
Simulate random times between the limits of $\mathcal{T}$ for each of the PE.

## Simulations from the model[]{#sec:simulations label="sec:simulations"}
Following the actor-oriented logic of the algorithm, the
simulation process is divided into two steps: sampling of the sender of
the event and selection of the receiver.

At time t, the senders at risk are, for the example:

- From $X_1$, the corresponding pair sender-flavour for the only one
  event $\omega$ that satisfies
  $t_{\omega} = \min_{\omega'}\{t_{\omega'}>t\}$.

- From $X_2$, the set
  $\mathcal{S}(X_2,t)=\cup_\varphi \mathcal{S}_\varphi(X_2,t)$ of
  the observed sender-flavour pairs present in the panel data and for
  which not all events (i.e., not all tuples sender-receiver-time) have
  yet being sampled (given a simulated time).


The conditional probabilities (rates) of being the next sender are
computed for the actors at risk. One pair sender-flavour is chosen (sampled), two
possibilities arise:

- If the sender-flavour pair corresponds to the RE, then the relational
  event is selected to be the next one in the complete sequence of
  events. -> Nothing need to be done, update the process state and time of simulation.

- If the sender-flavour corresponds to a PE, then:

  1.  Sample a receiver from the set of observed ones that are not yet
      sampled. Only actors from
      events in the observed data, not sampled at time $t$ (not given a sampled/simulated time), will be at risk. Compute the conditional probability of the receivers.
  2.  Sample the time of the event from the model specification and
      conditioned to be before the next recorded RE: $t^*<t_{h}$,
      $h = \min_k{t_k\geq t}$ (see Figure
      [1](#fig1){reference-type="ref" reference="fig1"}).

The routine is repeated until all observed events have been sampled,
i.e., until all panel events have been assigned a simulated time $t^*$.

<figure id="fig1" data-latex-placement="h">

<figcaption>Timeline representing observed and sampled times. Green
times correspond to relational events and are observed. Purple time is
simulated for a panel data event verifying <span
class="math inline"><em>t</em> ≤ <em>t</em><sup>*</sup> &lt; <em>t</em><sub><em>h</em></sub></span>.<span
id="fig1" data-label="fig1"></span></figcaption>

\begin{tikzpicture}[scale=1]
  % Timeline
  \draw[thick] (0,0) -- (6,0);
  \fill[black] (2,0) circle (2pt);
  \node[below=6pt] at (2,0) {$t$};

% Points as vertical lines
  \draw[green, thick] (1,-0.2) -- (1,0.2);
  \draw[green, thick] (5,-0.2) -- (5,0.2);
  \draw[purple, thick]  (3,-0.2) -- (3,0.2);

  % Labels in math mode, spaced further
  \node[above=6pt] at (1,0) {$t_{h-1}$};
  \node[above=6pt] at (5,0) {$t_{h}$};
  \node[below=6pt] at (3,0) {$t^*$};
\end{tikzpicture}
</figure>

::: {#ex:1 .example}
**Example 1**. *Consider a multiplex network conformed by*

- *Set of actors $\mathcal{A} = \{1,2,3,4\}$.*

- *One relational event layer $X_1$, with two flavours
  $\{\varphi_1,\varphi_2\}$.*

- *One panel data layer $X_2$, with two flavours
  $\{\varphi_3,\varphi_4\}$.*

*Moreover, consider that the events observed in $X_1$ are
($(sender,receiver,flavour,time)$):
$$\{ (1,2,\varphi_1,3), (1,2,\varphi_2,5), 
(3,4,\varphi_1,10) ,(2,3,\varphi_1,12)\}.$$ Consider that the changes
observed in $X_2$ are ($(sender,receiver,flavour)$):*

*$$\{(2,4,\varphi_3), 
(3,1,\varphi_3), 
(3,2,\varphi_3), 
(4,2,\varphi_4) ,(4,1,\varphi_4)\}.$$*

*Consider that the simulation time $t = 4.5$, and that three events have
already been sampled: $(2,4,\varphi_3,t_1^*=2)$, $(1,2,\varphi_1,3)$,
and $(4,2,\varphi_4,t_2^*=4.5)$. Then, the pairs of sender-flavour at
risk to be the next sender are: $(1,\varphi_2)$, $(3,\varphi_3)$, and
$(4,\varphi_4)$ (see Figure [2](#fig2){reference-type="ref"
reference="fig2"}).*

<figure id="fig2" data-latex-placement="h">

<figcaption><em>Illustration of the simulations from the model
constrained to the observed data.<span id="fig2"
data-label="fig2"></span></em></figcaption>

\begin{tikzpicture}[scale=1, every node/.style={font=\small}]
  % Timeline
  \draw[thick] (0,0) -- (13,0);

% Relational events as vertical lines
  \draw[green, thick] (3,-0.2) -- (3,0.2);
  \draw[green!50, thick] (5,-0.2) -- (5,0.2);
  \draw[green!50, thick]  (10,-0.2) -- (10,0.2);
  \draw[green!50, thick]  (12,-0.2) -- (12,0.2);

  % Relational event labels
  \node[above=6pt] at (3,0) {$(1,2,\varphi_1,3)$};
  \node[above=6pt,text opacity=0.5] at (5,0) {$(1,2,\varphi_2,5)$};
  \node[above=6pt,text opacity=0.5] at (10,0) {$(3,4,\varphi_1,10)$};
  \node[above=6pt,text opacity=0.5] at (12,0) {$(2,3,\varphi_1,12)$};
  
 \fill[black] (4.5,0) circle (2pt);
  \node[below=6pt] at (4.5,0) {$t$};

% Panel data events as vertical lines
  \draw[purple, thick] (2,-0.2) -- (2,0.2);
  \draw[purple, thick] (4.5,-0.2) -- (4.5,0.2);
  \draw[purple!50, thick]  (6.5,-0.2) -- (6.5,0.2);
  \draw[purple!50, thick]  (8.2,-0.2) -- (8.2,0.2);
  \draw[purple!50, thick]  (11,-0.2) -- (11,0.2);

  % Panel data event labels
  \node[below=6pt] at (2,0) {$(2,4,\varphi_3,t_1^*=2)$};
  \node[below=20pt] at (4.5,0) {$(4,2,\varphi_4,t_2^*=4.5)$};
  \node[below=6pt,text opacity=0.5] at (6.5,0) {$(3,2,\varphi_3,t_3^*=6.5)$};
  \node[below=20pt,text opacity=0.5] at (8.2,0) {$(3,1,\varphi_3,t_4^*=8.2)$};
  \node[below=6pt,text opacity=0.5] at (11,0) {$(4,1,\varphi_4,t_5^*=11)$};

\end{tikzpicture}
</figure>

*In the example, the pair $(1,\varphi_2)$ is selected, which corresponds
to a relational event. The sender $1$ only sends one event with flavour
$\varphi_2$, so the receiver and time are also determined.*

*The next pair sender-flavour sampled in the example is $(3,\varphi_3)$.
In this case, there are still two possible receivers, actors $1$ and
$2$. The conditional probabilities are then computed, and for the
example, sender $2$ is chosen. The time $t^*_3$ is sampled to be $6.5$.*

*The process is repeated until all events have been sampled.*
:::

## MCMC routine

From a previously generated sequence, new samples can
be generated via permutations. The permutations only affect the panel
events. The following MCMC proposal will allow for changes in the
relative positions of the PE events with respect to the relational
events. This generates sequences with a different number of PEs
occurring between the observed REs. Start from a sequence of events (PE and RE)
$\Omega_C$. Perform an MCMC step, represented in Figure
[3](#fig3){reference-type="ref" reference="fig3"}, by transforming the
sequence:

1.  Choose a pair of PE to permute: $\omega^*_h$ and $\omega^*_k$, $h<k$
    (recall that the ${}^*$ notation implies that the time is sampled).

2.  Sample a new time for $\omega^*_k$, $t'_k$, such that (conditional
    on): $$\begin{equation*}
          t^*_{h-1} < t'_k < \left\{\begin{array}{cc}
              t^*_{h+1}, & k>h+1, \\
               t^*_{h+2}, & k=h+1.
           \end{array}\right.
    \end{equation*}$$ 
    This sampling is done conditional on
    respecting the order among PE events, and allows for changes in the
    relative positions between PE and RE. For example, it could happen
    that $\omega^*_h$ occurred before $\omega_l$ (relational event), but
    after the permutation $\omega'_k$ occurs after $\omega_l$.

3.  Sample a new time for $\omega^*_h$, $t'_h$ following the equivalent
    rules as for $t'_k$. $$\begin{equation*}
          \max\{t'_k,t^*_{k-1}\} < t'_h < t^*_{k+1}.
    \end{equation*}$$

<figure id="fig3" data-latex-placement="h">

<figcaption>Timeline representing observed and sampled times. Green
times correspond to relational events and are observed. Purple times
with a prime are obtained through the MCMC routine. Purple times with an
asterisk are times not modified in the MCMC step, and obtained through a
simulation or a sampling process (not observed). The example shows that
the proposed times can occupy different relative positions with respect
to the RE, thereby allowing for a richer sample pool.<span id="fig3"
data-label="fig3"></span></figcaption>

\begin{tikzpicture}[scale=1]
  % Timeline
  \draw[thick] (0,0) -- (10,0);
  %\fill[black] (2,0) circle (2pt);
  %\node[below=6pt] at (2,0) {$t$};

% Points as vertical lines
  \draw[purple, thick] (1,-0.2) -- (1,0.2);
  \draw[purple, thick] (4,-0.2) -- (4,0.2);
  \draw[purple, thick]  (2,-0.2) -- (2,0.2);

  \draw[purple, thick] (6,-0.2) -- (6,0.2);
  \draw[purple, thick] (9,-0.2) -- (9,0.2);
  \draw[purple, thick]  (7,-0.2) -- (7,0.2);
  
  \draw[green, thick]  (3.2,-0.2) -- (3.2,0.2);
  \draw[green, thick]  (7.5,-0.2) -- (7.5,0.2);


  % Labels in math mode, spaced further
  \node[below=6pt] at (1,0) {$t^*_{h-1}$};
  \node[below=6pt] at (4,0) {$t^*_{h+1}$};
  \node[below=6pt] at (2,0) {$t'_k$};
  \node[below=6pt] at (6,0) {$t^*_{k-1}$};
  \node[below=6pt] at (9,0) {$t^*_{k+1}$};
  \node[below=6pt] at (7,0) {$t'_h$};

  \node[above=6pt] at (3.2,0) {$t_u$};
  \node[above=6pt] at (7.5,0) {$t_v$};

  \node[below=6pt] at (5,0) {...};
  
  \draw[purple!50, thick] (2.5,-0.2) -- (2.5,0.2);
  \node[below=12pt, text opacity=0.5] at (2.5,0) {$t^*_h$};

  \draw[purple!50, thick] (8.2,-0.2) -- (8.2,0.2);
  \node[below=12pt, text opacity=0.5] at (8.2,0) {$t^*_k$};

\end{tikzpicture}
</figure>

The proposed sequence on the MCMC is then
$\Omega'_c=\Omega^{RE}\cup\{\omega^*_1,\dots,\omega'_k,\dots,\omega'_h,\dots,\omega^*_n\} =\Omega^{RE}\cup\Omega'$.

Finally, in order to define the MCMC routine, we need to specify the
proposal distribution. In this case, the proposal corresponds only to
the probability of randomly selecting two events from all events.
This results in a simple acceptance
rule for the Metropolis-Hastings MCMC: $$\begin{equation*}
    \alpha(\Omega'_c,\Omega^*_c) = \frac{f(\Omega'_c)}{f(\Omega^*_c)}.
\end{equation*}$$ The step will be accepted if, for a random uniform
number between 0 and 1, $u$, $u\leq \alpha$. If the new sequence is more
likely than the previous one, the probability of being accepted is
higher. If the step is accepted, then "rename" the new events, with new
times with the star notation:
$\omega^*_h (i_h,j_h,t^*_h,\varphi_h)\leftarrow \omega'_k (i_k,j_k,t'_k,\varphi_k)$
and
$\omega^*_k(i_k,j_k,t^*_k,\varphi_k)\leftarrow ~\omega'_h (i_h,j_h,t'_h,\varphi_h)$
.

::: example
**Example 2** (Continuation of Example [1](#ex:1){reference-type="ref"
reference="ex:1"}). *Consider that the two events chosen to be permuted
are $\omega_1^*=(4,2,\varphi_4,t^*_2)$ and
$\omega_2^*=(3,1,\varphi_3,t^*_4)$. The proposed new times must verify
$2<t'_4<6.5$ and $6.5<t'_2<11$. Therefore, the new event $\omega'_2$ can
be before $\omega_1$, in between $\omega_1$ and $\omega_2$ (like it was
$\omega^*_3$), or after $\omega_2$. On the other hand, $\omega'_1$ can
be before the relational event $\omega_3$ (like $\omega^*_2$) or after
it. The relative positions between PE and RE can change, and the
proposed changes are more likely to be accepted if they increase the
likelihood. See Figure [4](#fig4){reference-type="ref" reference="fig4"}
for an illustration of this example.*

<figure id="fig4" data-latex-placement="h">

<figcaption><em>Illustration of the permutations proposed during the
MCMC routine. <span id="fig4"
data-label="fig4"></span></em></figcaption>
\begin{tikzpicture}[scale=1, every node/.style={font=\small}]
  % Timeline
  \draw[thick] (0,0) -- (13,0);

% Relational events as vertical lines
  \draw[green, thick] (3,-0.2) -- (3,0.2);
  \draw[green, thick] (5,-0.2) -- (5,0.2);
  \draw[green, thick]  (10,-0.2) -- (10,0.2);
  \draw[green, thick]  (12,-0.2) -- (12,0.2);

  % Relational event labels
  \node[above=6pt] at (3,0) {$\omega_1 = (1,2,\varphi_1,3)$};
  \node[above=16pt] at (5,0) {$\omega_2 =(1,2,\varphi_2,5)$};
  \node[above=6pt] at (10,0) {$\omega_3 = (3,4,\varphi_1,10)$};
  \node[above=16pt] at (12,0) {$\omega_4 = (2,3,\varphi_1,12)$};
  

% Panel data events as vertical lines
  \draw[purple, thick] (2,-0.2) -- (2,0.2);
  \draw[purple!50, thick] (4.5,-0.2) -- (4.5,0.2);

 \draw[purple, ultra thick] (5.4,-0.2) -- (5.4,0.2);

 \draw[purple, ultra thick] (8,-0.2) -- (8,0.2);
  
  \draw[purple, thick]  (6.5,-0.2) -- (6.5,0.2);
  \draw[purple!50, thick]  (8.2,-0.2) -- (8.2,0.2);
  \draw[purple, thick]  (11,-0.2) -- (11,0.2);

  % Panel data event labels
  \node[below=20pt] at (2,0) {$(2,4,\varphi_3,t_1^*=2)$};
  \node[below=32pt,text opacity=0.5] at (4.5,0) {$(4,2,\varphi_4,t_2^*=4.5)$};

\node[below=6pt, font=\boldmath] at (5,0) {$(3,1,\varphi_3,t'_4=5.4)$};
  
  \node[below=20pt] at (6.5,0) {$(3,2,\varphi_3,t_3^*=6.5)$};

  \node[below=6pt, font=\boldmath] at (8.2,0) {$(4,2,\varphi_4,t'_2=8)$};
  
  \node[below=32pt,text opacity=0.5] at (8.2,0) {$(3,1,\varphi_3,t_4^*=8.2)$};
  \node[below=20pt] at (11,0) {$(4,1,\varphi_4,t_5^*=11)$};

\end{tikzpicture}
</figure>
:::

