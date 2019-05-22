# ToDo

## Bugfix
- Don't kill when it's done.
- Lwt'ization of loading data from files. Potentially fail.

## New features
- report: add memory usage instrument by Gc module
  - memory/execution time profiling tools: just interested to briefly discuss what kind of tools being used for profiling. Current I'm using valgrind but not very handy.
- add support of dynamic population of workers
  - Depends on barrier method used. E.g:
    - the management of workers’ information in the book.
      - if it’s the BSP, then removing a worker will stop all the others;
      - if it’s ASP, then dynamically removing one or adding it back should be no problem.
- evaluation setup: use multiple rpi devices, or one large server, to scale up.
- github: add Travis-CI for D-MNIST with Unix && MirageOS, make sure to not break with further commits
- lwae: fix transparent parallelism behind LwAE from consistent training interface as the original AE does
- lwae: add other Barrier support, Jianxin?
  - for the barrier support, I've added some initial implementation here: https://github.com/jzstark/light_actor/blob/lda/src/core/actor_barrier_ssp.ml

## Unsorted
- besides mnist training, find several IoT-based use cases where multiple instances interact with each other
- difficulties in wrapping up an Owl script into Unikernel; possible automation of this process and deployment to edge devices

## Pending
- owl: build Owl without Unix dependency for MirageOS https://github.com/owlbarn/owl/pull/372 No good idea for now
  - ex: gettimeofday -> ptime???

## Done
- ~~mnist: add inference/test after training~~
- ~~mnist: add Random'ization of get_next, currently it's incremented sequentially, but should be random'ized.~~
- ~~report: add support arbitral number of clients automatically 128~~
