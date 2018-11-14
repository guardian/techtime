
In order to simulate and observe the gravitational slingshot effect, we just need two bodies, with a certain type of relative motion. In our case we are going to use a single planet and a single probe. 

### Datatypes

Let's start with the types. Let's have two fundamental types `Position` and `Speed`. 

```
type Position = (Double, Double)
type Speed = (Double, Double)
```

We use vectors with two coordinates because we are limiting ourselves to the 2D plan, but we could also model in 3D in which case we would use vectors of length 3. Then we only need one case class to represent celestial bodies:

```
case class Body(position: Position, speed: Speed)
```

### Modelisation strategy

There are essentially two ways to model the evolution of systems bounds by gravitation. 

The first is to solve complex differential equations [1] and hope we can solve them. We sometimes can but most of the time we can't. When we can the evolution of the system, and in particular the motion of its elements, are given by nice predictible functions of a time variable. This is the ideal case to be in. 

[1] The reason why we end up having to solve those equations is that the laws of nature are given as differential equalities. For instance (as we are going to see in a minute), Newton's second law is an equality that involves a force, a mass and an accelaration (the second derivative of a position).

The second is to perform incremential updates of the state of the system. For instance given the state of the system at time `T`, we can easily compute the state of the system at time `T+epsilon`, where `epsilon` is small. If we then keep computing the next step over and over again, we have a view of the evolution of the system. The idea being that the smaller the `epsilon` the more computations steps are needed, but also the more accurate (ie: closer to Nature) the simulation is. 

Here we are going to work and implement the second way: incremental updates. 

### Newton's Second Law

The Second Law of Newton, is a fundamental law of Nature that tells us the way objects trajectories evolve when being subject to forces, for instance, the force of gravitation. 

The law says:

```
F = m a
```

Here, the force `F` is assumed to be a vector (will have two cartesian components in our simulation, but has three cartesian components when working in the 3D space). The acceleration is also a vector. The mass is just a non null positive number. 

When considering the force and acceleration as scalars, the force is in **newton**, the mass in **kilograms** and the acceleration in **m/s^2**. 

For our study today, we need to refactor this equality to be `a = F / m`, which reads: "the acceleration experienced by an object is the force applied to it divided by its mass", where, again, `F` and `a` are vectors.

We need types to represent forces and accelerations 

```
type Force = (Double, Double)
type Acceleration = (Double, Double)
```

The function to compute an acceleration given a force is 

```
def acceleration(force: Force, mass: Double): Acceleration = {
	(force._1/mass, force._2/mass)
}
```

Now, given a `Boby` at a given instant and an acceleration, we will be computing the new `Body` `x` seconds later, as follows:

1. Compute the new position of the body according to its current position and current speed.

2. Compute the new speed of the body according to its current speed and the acceleration.

Note that we could do the opposite: updating the speed first and updating the position using the updated speed. Using one method instead of the other would lead to different simulations, but as the time increment `x` becomes smaller and smaller, both simulations converge to Nature's behavior. 

The function to compute a new body given an acceleration and a time increment is 

```
def incBody(body: Body, acc: Acceleration, x: Double): Body = ???
```

### Gavitation Law

The gravitation law says

```
F = G m1 m2 / r^2
```

Where `F` is the force that the two bodies exercise on each other. `G` is the gravitational constant, whose value doesn't matter right now, `m1` and `m2` the masses of the two objects and `r` the distance between them. All values given in international standard units. 

The Force here is a number and not a vector. What happens is that the intensity of the force applied to both objects is the same, but converted into two vectors (one for each object) the two vectors points to opposite directions. The force applied to object A by object B, points towards B and vice versa. This means that the two objects attract each other (the effect of gravity).

We need a function to compute vector forces. 

```
def forceAppliedToFirstArgument(a: Body, b: Body): Force = ???
```

We are almost done. We now define a system as a collection of `Body`s. 

```
type System = List[Body] 
```

And given a system we can compute its state `x` seconds later

```
incSystem(system: System, x: Double): System = ???
```

All the function `incSystem` does is: 
for each body of a `System`: compute all the forces applied to the body, use vector algebra to compute the resulting force applied to the body, use `acceleration` to compute the acceleration of the body, compute the next status of the body; all leading to the next `System`.

Now, posing system as: 

1. A huge mass moving slowly (but not immobile) and 
2. A smaller mass moving faster

and positioning them correctly at the start of the simulation, you should be able to see the effect in action. 

### Simplifications

There is no need to make the system to scale, and you can play around with the gravitation constant: the smaller the less bodies interact with each other, the bigger the more the bodies interact (and sometimes fall on each other no matter what). You need to find the right value for gravitation to affect their trajectory but without pathology :)

 