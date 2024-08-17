
[![tic](https://github.com/mlampros/CopernicusDEM/workflows/tic/badge.svg?branch=master)](https://github.com/mlampros/CopernicusDEM/actions)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/CopernicusDEM)](http://cran.r-project.org/package=CopernicusDEM)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/CopernicusDEM?color=blue)](http://www.r-pkg.org/pkg/CopernicusDEM)
<a href="https://www.buymeacoffee.com/VY0x8snyh" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png" alt="Buy Me A Coffee" height="21px" ></a>
[![Dependencies](https://tinyverse.netlify.com/badge/CopernicusDEM)](https://cran.r-project.org/package=CopernicusDEM)


## CopernicusDEM

<br>

Copernicus Digital Elevation Model datasets (DEM) of 90 and 30 meters resolution using the 'awscli' command line tool. The Copernicus (DEM) is included in the Registry of Open Data on AWS. Details on how to use the R package can be found both in the Vignette and in the [blog post](http://mlampros.github.io/2021/05/21/copernicusDEM_package/).

<br>

**System Requirements**:

This R package uses the command line **awscli** internally, which has to be installed first in the Operating System. 

<br>

On **Ubuntu** this can be done using:

```R
sudo apt install -y awscli

```

<br>

On **Macintosh** use,

```R
brew install awscli

```

<br>

and on **Windows 10** (tested) open the command line (cmd) and type,

```R
msiexec.exe /i https://awscli.amazonaws.com/AWSCLIV2.msi

```

which will open a new window ('AWS command line interface v2 Setup'), then click twice on 'next' and then 'install' and 'allow the device to make changes' and once another window pop-ups click 'finish'

<br>

Then verify the installation using,

```R
aws --version

```

<br>

and proceed to **configure aws** using either 

```R
aws configure

```

or

```R
export AWS_ACCESS_KEY_ID=xxxxxxxxx
export AWS_SECRET_ACCESS_KEY=xxxxxxxxx
export AWS_DEFAULT_REGION=eu-central-1

```

<br>

To check that **awscli** was installed and configured successfully run the following line in the command line. It returns the first 10 files of the 90 meter DEM product (for verification purposes),

```R
aws s3 ls s3://copernicus-dem-90m | head -n 10

```

<br>

To install the package from CRAN use, 

```R
install.packages("CopernicusDEM")

```
<br>

and to download the latest version of the package from Github,

```R
remotes::install_github('mlampros/CopernicusDEM')

```

<br>

### Citation:

<br>

If you use the **CopernicusDEM** R package in your paper or research please cite:

<br>

```R
@Manual{,
  title = {{CopernicusDEM}: Copernicus Digital Elevation Models},
  author = {Lampros Mouselimis},
  year = {2024},
  doi = 10.32614/CRAN.package.CopernicusDEM,
  note = {R package version 1.0.4 produced using Copernicus
    WorldDEMTM-90 DLR e.V. 2010-2014 and Airbus Defence and Space
    GmbH 2014-2018 provided under COPERNICUS by the European Union
    and ESA; all rights reserved},
  url = {https://CRAN.R-project.org/package=CopernicusDEM},
}
```

<br>
