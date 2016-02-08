#Description of (planned) classifier module

# Introduction #

OpenSim/SL is an object oriented system, where assets are immutable and identified by UUID's.

Bots need a way to classify assets -  "this is brick", "this is concrete", "this is a fire particle". Classification is, largely, recognition.  I go into a room and want to sit down, I classify that brownish smudge as a seat, I am ready to use an ontology and a planner to form a plan to sit on the seat.


# Architecture #

Classification is a two step process.

First the actual image data is analyzed by a SalientFeatureExtractor and a SalientVector produced. This is a representation of a set of features of the asset. There can be one SalientVector per asset per SalientFeatureExtractor subclass.  It's expected that for a while there will only be one SalientFeatureExtractor subclass.

The SalientVector is then cached in a database, so it never need be computed again.  The database can be common to many users of cogbot.
A small server provides a REST interface for querying and adding to the SalientVector tables.

At runtime the SalientVector is then classified by a Classifier, which returns the final classification. This classification occurs on much less data, so should be quick. The final Classification object is returned to the external system.

![http://cogbot.logicmoo.com/assetclassifiers.png](http://cogbot.logicmoo.com/assetclassifiers.png)

# SalientVector by type #

## Image ##

Colors are 8 bit, 0-255, as is alpha

'transparent' is alpha < 0.5

  * mean color, rgba
  * modal color, rgba, where the modal buckets are per-pixel, not per-channel
  * % (0-255 mapped) of pixels that are transparent
  * % (0-255 mapped) of edge pixels that are transparent
  * RMS error of color channels across the horizontal wrap boundary
  * RMS error of color channels across the vertical wrap boundary
  * known names
  * float proportion of pixels that are adjacent to an edge between transparent and opaque
  * Number of horizontal lines by hough --> masking --> blob analysis
  * Number of vertical lines by hough --> masking --> blob analysis
  * Number of diagonal lines by hough --> masking --> blob analysis
  * greatest number of transitions of color of > n tracing along the Hough line
  * circular hough score (float)
  * float proportion of pixels that are edges by Canny
  * topology tree of transparent/nontransparent areas
  * integrated frequency power of image.
  * length of longest single edge by transparency.
  * size of largest blob of edge pixels.

# The Server #

# Interaction with the rest of Cogbot #

Because this is a streaming system, all calls are async.

TODO - decide how that works with, for example, AIML matching.

TODO - decide the syntax of how this stuff gets into botcmd/AIML/whatever


This is a copy of the old machinevision wiki page


# Machine Vision #

Cogbot would benefit from being able to characterize surfaces.

# Features #

  * ratio of edges between transparent and nontransparent areas - eg. plants wil have high values for this.
  * Topology of transparent areas - Solid on the edges withmany holes in the center is probably a window with mullions
  * Hough transform - this finds lines
  * FFT - this can probably be used to sort out 'textures' from 'images'



# Packages #

This is notes about available machine vision/image processing packages that might be suitable for doing texture analysis



  * AForge - a nice, easy to use package in C#  The Hough transform is slow, we need to run some performance tests

  * cIMg  - C++ library with nice support.
> > Name is really "CoolImage", but many places refer to it as cImg (even on the project website).
> > It appears to be quite simple to use, and the demo program seems more performant.
> > 5/5/2012 - AO - I've written hough/inverse hough and it runs at video rates.

  * eyepatch - this is what Kino originally was going to use. It seems more an application than a library, and seems really oriented towards camera analysis.

  * ImageJ - seems quite slow, and more of a toy app than a library

  * Neatvision - I've used this for a project. Sadly, it's dead, because it was exactly what we needed. It had a GUI dataflow / visual programming pipeline builder, and a very wide variety of transforms. Unofrtunately it's closed source, dead, and depends on Java 1.4.  The offered replacement depends on matlab

  * OpenCV - this seems to be the closest there is to a standard machine vision package. However, it is complex, extremely poorly documented, and seems to be in disarray - I couldn't even find coherent build instructions or get it to build.