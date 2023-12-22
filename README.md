# ggseabubble
<img src="./.img/ggseabubble.png" alt="ggseabubble" width="200">

R package for creating publication-ready bubble heatmaps.

## Introduction
**ggseabubble** exploits gpplot2 and patchwork to create a heatmap-like visualization inspired by Bubble GUM ([Spinelli, L. et al., 2015](https://doi.org/10.1186/s12864-015-2012-4)) that allows to **sumarize several GSEA comparisons in a single plot**. 

A bubble heatmap represents comparisons as columns and signatures as rows. The colour of the bubble is proportional to the NES and the size of the bubble to the FDR-adjusted p-value. Empty bubbles represent non-significant results. Rows and columns can be clustered according to the distance between NES.



<!-- ## Tutorials

* Plotting GSEA results.
* Plotting fGSEA results.

## Installation -->

## Authors
María José Jiménez-Santos

<!-- ## Citation -->

## Support
If you have any questions regarding the use of ggseabubble, feel free to submit an [issue](https://github.com/mj-jimenez/ggseabubble/issues).

## Publications that use ggseabubble
* Ciscar, M., Trinidad, E. M., Perez-Chacon, G. *et al.* (2023). RANK is a poor prognosis marker and a therapeutic target in ER-negative postmenopausal breast cancer. *EMBO molecular medicine*, 15(4), e16715. [https://doi.org/10.15252/emmm.202216715.](https://doi.org/10.15252/emmm.202216715)
* Rocha, A. S., Collado-Solé, A., Graña-Castro, O. *et al.* (2023). Luminal Rank loss decreases cell fitness leading to basal cell bipotency in parous mammary glands. *Nature communications*, 14(1), 6213. [https://doi.org/10.1038/s41467-023-41741-5.](https://doi.org/10.1038/s41467-023-41741-5)
