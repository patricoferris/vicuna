# Vicuña 🦙
------

**Note this is not supposed to be a fast, memory-optimised implementation of a ray-marching algorithm. Instead it was done for fun and educational purposes. There are lots of ways I know to make it better or faster, but that's not really the point at the moment.** 

The code generates a scene and places it in `image.ppm` with the following header: 

```
P3
<width> <height>
255
```

To run the code, the following should work: 

```
cd src 
dune exec -- ./main.exe
```

------

Some todos: 
  - Finish the `scene` making tool so a large number of shapes can be drawn
  - Double check some of the maths (almost sure some of the `x` or `y` coordinates are the wrong way around 🐫)
  - Finish writing the inverse matrix function for proper transformations 