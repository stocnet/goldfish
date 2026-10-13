* Improved the DyNES sequence augmenters (`augment_seq_*()`):
  * Draws, legality checks and rate queries read tied stamps as the
    likelihood does;
  * legality checks and simulated draws walk the preprocessing control the
    likelihood is scored with, a non-default one included, as rate queries
    already did;
  * simulated draws evaluate fewer rates and replay no unused walks.
* Fixed `augment_seq_sim()` hanging on tied observed events.
* Fixed `augment_seq_sim()` hanging on an observed event at a wave time.
