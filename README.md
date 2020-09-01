# traviz
## Google Summer of Code 2020
traviz is my Google Summer of Code 2020 project. To see the project abstract, use the following link https://summerofcode.withgoogle.com/projects/#5517386007969792

## A general purpose trajectory analytics toolbox 
traviz contains two classes to store trajectories:
* `sfTrack`- store an individual track/trajectory
* `sfTracks`- store a collection of sfTrack objects (trajectories)

`sfTrack` and `sfTracks` generic methods include:
* `sf::`- st_transform, st_coordinates, st_intersection, st_distance, st_bbox, st_length (https://github.com/r-spatial/sf)
* `pv_stcube`- point value space time cube 
* `vscube`- value space cube 
* `cluster`- clustering of Tracks

traviz also contains convertor methods:
* `geodata_to_sf`- converts geographic point data to a trajectory in sf format
* `sf_to_rasterize`- converts sf data to raster point data
* `sfTrack` and `sfTracks` coercion methods: coerce classes to `data.frame`, `sf`, `Track`, `geojson`

The main methods of traviz perform the following:
* Aggregation and subsetting of trajectories or trajectory point values
* Plotting of individual trajectories or a collection of trajectories
* Rasterization of point values in trajectories 
* Visualization of trajectories through space time cubes, animations, and options for multiple projections
* Heatmap, hotspot, and density visualization 

## Blog posts:
[Introductory blog post](https://blog.52north.org/2020/05/29/trajectory-analytics-toolbox-in-r/) 

[Midterm blog post](https://blog.52north.org/2020/07/10/trajectory-analytics-toolbox-midterm-post/)

## Weekly progress status
[Weekly progress status for Google Summer of Code 2020](https://wiki.52north.org/Projects/GSoC2020TrajectoryAnalyticsToolbox)

## Installation
To install the most current version:
```R
devtools::install_github("JamMurz/traviz")
```