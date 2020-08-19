# traviz
## Google Summer of Code 2020
[traviz](https://summerofcode.withgoogle.com/projects/#5517386007969792) is my Google Summer of Code 2020 project. 

## A general purpose trajectory analytics toolbox 
traviz contains two classes to store trajectories:
* `sfTrack`- store an individual trajectory
* `sfTracks`- store a collection of trajectories

The main functionality of traviz include the following:
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
To install the most current development version:
```R
devtools::install_github("JamMurz/traviz")
```