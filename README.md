# ggseabubble
<img src="./.img/ggseabubble.png" alt="ggseabubble" width="200">

R package for creating publication-ready bubble heatmaps.

## Introduction
**ggseabubble** exploits gpplot2 and patchwork to create a heatmap-like visualization inspired by Bubble GUM ([Spinelli, L. et al., 2015](https://doi.org/10.1186/s12864-015-2012-4)) that allows to **sumarize several GSEA comparisons in a single plot**. 

A bubble heatmap represents comparisons as columns and signatures as rows. The colour of the bubble is proportional to the NES and the size of the bubble to the FDR-adjusted p-value. Empty bubbles represent non-significant results. Rows and columns can be clustered according to the distance between NES.

<!-- ## Tutorials -->

* Plotting [GSEA](https://www.gsea-msigdb.org/gsea/index.jsp) results.
* Plotting [fgsea](https://bioconductor.org/packages/release/bioc/html/fgsea.html) results.

## Installation
The ggseabubble package is implemented in R >= 4.0.0. We recommend 
running the installation via mamba: 

```r
# Create a conda environment.
conda create -n ggseabubble 
# Activate the environment.
conda activate ggseabubble
# Install beyondcell package and dependencies.
mamba install -c mjjimenez r-ggseabubble
```

## Authors
María José Jiménez-Santos

## Citation
If you use `ggseabubble` in your work, please cite us:

```
Jimenez-Santos MJ. ggseabubble: R package for creating publication-ready bubble heatmaps. Version 1.0.0. 2023. doi:10.5281/zenodo.10692491.
```
## Support
If you have any questions regarding the use of ggseabubble, feel free to submit an [issue](https://github.com/mj-jimenez/ggseabubble/issues).

## Publications that use ggseabubble
* Ciscar, M., Trinidad, E. M., Perez-Chacon, G. *et al.* (2023). RANK is a poor prognosis marker and a therapeutic target in ER-negative postmenopausal breast cancer. *EMBO molecular medicine*, 15(4), e16715. [https://doi.org/10.15252/emmm.202216715.](https://doi.org/10.15252/emmm.202216715)
* Rocha, A. S., Collado-Solé, A., Graña-Castro, O. *et al.* (2023). Luminal Rank loss decreases cell fitness leading to basal cell bipotency in parous mammary glands. *Nature communications*, 14(1), 6213. [https://doi.org/10.1038/s41467-023-41741-5.](https://doi.org/10.1038/s41467-023-41741-5)
