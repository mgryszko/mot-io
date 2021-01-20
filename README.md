# My Own Toy IO Monad

Do you wonder how IO monad implementation works under the hood? I did, so, as a learning exercise, I implemented the very basic IO monad
from scratch.

I took inspiration from
the [Demystifying functional effect systems in Scala](https://medium.com/wix-engineering/demystifying-functional-effect-systems-in-scala-14419039a423)
article by [Dmitry Karlinsky](https://twitter.com/__sapien) and
the [How do Fibers Work? A Peek Under the Hood](https://www.youtube.com/watch?v=x5_MmZVLiSM) talk by Fabio Labella.

## Features

- stack safety
- exception handling
- error recovery 
- support for parallelism and concurrency by shifting the execution to a different execution context and by forking the run loop aka 
  creating a _fiber_

