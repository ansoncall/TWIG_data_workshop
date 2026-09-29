# TWIG Data Workshop
This is the source code for the 2026 TWIG Data Workshop activities. 

The TWIG (Treatment and Wildfire Interagency Geodatabase) is a spatial database
of forest management activities relating to wildfire and fuel reduction. 
With the recent addition of state agency and NGO data from the [National Fuels
Treatment initiative](https://nft.garphub.org/), it is now the most
comprehensive nationwide database of fuel treatments across all lands.

> Note: State agency data is not downloadable at this time, and will not be used
> in these exercises. 

The TWIG data workshop is designed to introduce users to TWIG and the ReSHAPE
program, build familiarity and confidence in working with TWIG data, and prepare
participants to use TWIG data in their own projects. In the workshop exercises,
users will have the chance to:
- download and examine TWIG data up-close
- identify and avoid common pitfall when working with fuel treatment data
- combine the Treatment Index with external data sources, including U.S. Census
  Burea income data and wildfire perimeter data.

## Requirements
This exercise is designed for participants who already have some experience
using R. Participants should have already installed:
- R
- A suitable IDE, such as [RStudio](https://posit.co/downloads) or
  [Positron](https://positron.posit.co/download.html)

## Contents

This repository includes two activities, each with two scripts: 
```
.
├───README.md                // this file
├───LICENSE                  // MIT License: all code is open-source
├───.gitignore               // controls which files are tracked by git
├───analysis1.R              // participants will write code for Activity 1 here
├───analysis1_complete.R     // Activity 1 answer key
├───analysis2.R              // participants will write code for Activity 2 here
├───analysis2_complete.R     // Activity 2 answer key

```

For each activity, the ```analysis*.R ``` script is where participants should
write their code. The ```analysis*_complete.R``` script is the "answer key"
(it's not a test, feel free to use the answer keys as needed). 

In addition to these files, we will create a "data" folder at the beginning of
Activity 1 to store data downloaded for analysis (don't do this until prompted).
When both activities are finished, the data folder should look like this:

```
.
│   
└───data
    ├───income_data.zip                         // Data downloaded in Activity 1 
    ├───treatment_index.zip                     // Data downloaded in Activity 1 
    ├───treatment_index_flagstaff_area.zip      // Data downloaded in Activity 2 
    ├───Perimeters_flagstaff_area.zip           // Data downloaded in Activity 2 
    ├───twig_co.rdata                           // Data object from Activity 1         
    ├───income_data.gdb                         // Data used in Activity 1
    ├───treatment_index_co.gdb                  // Data used in Activity 1
    ├───treatment_index_flagstaff_area.gdb      // Data used in Activity 2
    └───Perimeters_flagstaff_area.gdb           // Accidental "double-nesting"
        └───Perimeters_flagstaff_area.gdb       // Data used in Activity 2

```

## Getting started
If you are a GitHub user, you can start by cloning this repository:
```
git clone git@github.com:ansoncall/TWIG_data_workshop.git
```

If you are not familiar with Git or GitHub, just open up your IDE and copy/paste
the contents of the ```analysis*.R``` files into a blank R script as needed. 
