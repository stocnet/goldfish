* Fixed the rate risk set of a constrained or flavored one-mode model: a
  sender whose only allowed receiver was itself stayed at risk.
  * A creation flavor allows the self-dyad forever, since no actor holds a
    tie to itself, so an actor already tied to everyone kept contributing
    rate exposure for events it could not produce.
  * Estimates of such a model change. On a four-actor fixture where one
    actor is tied to all others, the intercept moved from -2.67 to -2.09
    and the degree effect from 0.66 to 0.36.
  * Re-preprocess a constrained or flavored one-mode rate model saved with
    `preprocessed =`; the format is unchanged, but a stored risk set is not.
  * `simulate()` no longer draws such a sender and then finds no receiver
    for it.
