# Overview 

This repository contains code required to produce Settlement Type and
Road Connectivity (STARC) codes, estimate canine populations, and
identify hotspots of free-roaming dog populations. Methods and examples
of its application are outlined in Corbett et al. 2024 <link>.

The main functionality of this code is established, but supporting
documentation is still under development. Novel uses of the STARC
methodology, including identifying expected transmission zones for
canine rabies, are still in development. Additional documentation and
clarity regarding these new applications are forthcoming.

# Running the code 

Currently there are two ways to interact with this code:

1.  Run the app.R file to run a local R Shiny application. This
    application will allow users to select parameters and then run
    individual sections of the STARC Toolkit

2.  Individually run the Rmd files within the R folder. The user will
    have to manually specify parameters at the top of each file. A
    letter prefix for each Rmd file notates the order the files should
    be run in (for example "A_Set_up_Input_Files.Rmd" precedes
    "B_Create_STARC_Map_SHP.Rmd")

The Rmd files are organized such that they can be run from the
interactive functions within the R shiny app, or individually run within
Rstudio.

# Data sources referenced 

The project uses several publicly-available data sources

**geoBoundaries**

National and subnational borders

[www.geoboundaries.org](www.geoboundaries.org)


**OpenStreetMap (OSM)**

Open source mapping including street network data. 
Data accessed at: [geofabrik](download.geofabrik.de)

more information here:
[OpenStreetMap](https://www.openstreetmap.org/about)

**Uber's hexagonal hierarchical geospatial indexing system (H3)**

Scalable hexagon based global grid system.

more information here: [Uber H3](https://www.uber.com/blog/h3/)

**Meta's High Resolution Settlement Layer (HRSL)**

High spatial resolution human population density data

more information here:
[HRSL](https://dataforgood.facebook.com/dfg/tools/high-resolution-population-density-maps)







# General disclaimer  
This repository was created for use by CDC programs to collaborate on public health related projects in support of the [CDC mission](https://www.cdc.gov/about/organization/mission.htm).  GitHub is not hosted by the CDC, but is a third party website used by CDC and its partners to share information and collaborate on software. CDC use of GitHub does not imply an endorsement of any one particular service, product, or enterprise. 

## Public Domain Standard Notice
This repository constitutes a work of the United States Government and is not
subject to domestic copyright protection under 17 USC § 105. This repository is in
the public domain within the United States, and copyright and related rights in
the work worldwide are waived through the [CC0 1.0 Universal public domain dedication](https://creativecommons.org/publicdomain/zero/1.0/).
All contributions to this repository will be released under the CC0 dedication. By
submitting a pull request you are agreeing to comply with this waiver of
copyright interest.

## License Standard Notice
The repository utilizes code licensed under the terms of the Apache Software
License and therefore is licensed under ASL v2 or later.

This source code in this repository is free: you can redistribute it and/or modify it under
the terms of the Apache Software License version 2, or (at your option) any
later version.

This source code in this repository is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the Apache Software License for more details.

You should have received a copy of the Apache Software License along with this
program. If not, see http://www.apache.org/licenses/LICENSE-2.0.html

The source code forked from other open source projects will inherit its license.

## Privacy Standard Notice
This repository contains only non-sensitive, publicly available data and
information. All material and community participation is covered by the
[Disclaimer](DISCLAIMER.md)
and [Code of Conduct](code-of-conduct.md).
For more information about CDC's privacy policy, please visit [http://www.cdc.gov/other/privacy.html](https://www.cdc.gov/other/privacy.html).

## Contributing Standard Notice
Anyone is encouraged to contribute to the repository by [forking](https://help.github.com/articles/fork-a-repo)
and submitting a pull request. (If you are new to GitHub, you might start with a
[basic tutorial](https://help.github.com/articles/set-up-git).) By contributing
to this project, you grant a world-wide, royalty-free, perpetual, irrevocable,
non-exclusive, transferable license to all users under the terms of the
[Apache Software License v2](http://www.apache.org/licenses/LICENSE-2.0.html) or
later.

All comments, messages, pull requests, and other submissions received through
CDC including this GitHub page may be subject to applicable federal law, including but not limited to the Federal Records Act, and may be archived. Learn more at [http://www.cdc.gov/other/privacy.html](http://www.cdc.gov/other/privacy.html).

## Records Management Standard Notice
This repository is not a source of government records, but is a copy to increase
collaboration and collaborative potential. All government records will be
published through the [CDC web site](http://www.cdc.gov).

## Additional Standard Notices
Please refer to [CDC's Template Repository](https://github.com/CDCgov/template) for more information about [contributing to this repository](https://github.com/CDCgov/template/blob/main/CONTRIBUTING.md), [public domain notices and disclaimers](https://github.com/CDCgov/template/blob/main/DISCLAIMER.md), and [code of conduct](https://github.com/CDCgov/template/blob/main/code-of-conduct.md).
