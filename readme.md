## Preliminary tests on the authorship of HDI fragments (Fonds Vandeul)
  
This repository contains some attribution tests done in order to check the authorship of selected fragments from *Histoire de deux Indes*. The corpus was compiled by Julian Csapo, the tests done by Antonina Martynenko; pipeline for the tests by Artjoms Šeļa & AM.  
  
[Corpus](##Corpus)  
[Methodology](##Methodology)  
[FP-1772](##FP-1772)  
[FV-Results](##FV-Results)  

## Corpus
### Reference set
The corpus as well as the metadata were prepared by Julian Csapo. Corpus metadata is stored as `metadata/corpus_metadata.csv`. The files in the corpus are presented as one file = one book.  
All together the corpus comprises 175 books by 17 authors:  
Baudeau, Chastellux, Condorcet, Deleyre, d'Holbach, Diderot (two sets), Guibert, Jaucourt, Jussieu, La Grange, Meister, Morellet, Naigeon, Pechméja, Raynal, Rivière, Saint-Lambert.  

### Test set
The fragments from the HDI are collected from ARTFL project, the third edition of the HDI (1780). **Only** fragments from *Fonds Vandeul* were taken into consideration for the current study.  
Each fragment is supplied with the metadata on its appearance in different editions and inclusion in *Pensées détachées* and/or *Mélanges*. The metadata for fragments is stored as `fragments_attributions_upd.csv`.

#### Groups of segments tested
- Segments from different editions:  
  - appeared 1770 - not changed 1774 - not changed 1780  
  - appeared 1770 - changed 1774 - not changed 1780  
  - appeared 1770 - not changed 1774 - changed 1780  
  - appeared 1770 - changed 1774 - changed 1780  
  - appeared 1774 - not changed 1780  
  - appeared 1774 - changed 1780  
  - appeared 1780  
  
- Segments from Pénsées Détachées and Mélanges:
  - segments in pencil (PD)
  - segments in ink (M)
  - segments included in ink, but not in pencil (M without PD)
  
- Separate fragments:
  - Fragments on Russia (68, 310) and China (12) combined together
  - Fragment 'Les avantages de la vie sauvage'
    
The analysis of the fragments in full can be found in `scr/03_1_analysis.md`, the last three fragments from the list are in `scr/03_2_analysis_additiona.md`.  

The steps on corpus creation and cleaning as well as segments manipulation are shown in `01_preprocessing.qmd` and `02_1_datawriting.qmd` respectively.

## Methodology
The analysis is fully done in R using `stylo()` package and `tidyverse` and `quatro` environment. As the corpus is unbalanced, the main addition to the basic `stylo()` analysis lies in repetitive sampling (independent samples).  
  
**Preprocessing**
- OCR sources of the books differ, so we tried to clean most frequent errors (long-s issues, -oit => -ait endings, etc.);
- Overlong books by each author were sampled down to 60k random tokens;  
  
**Preliminary analysis**  
- Books by each author were combined together in one set, two samples taken from each author; 
  - Sample size depends on the size of the document in question, e.g., if the text in question is only 8,000 words, only 2 samples of 4,000 words from each author were taken.
- To see the positioning of texts in question basic dendrogramms (200MFW, cosine delta, Ward's method) and bootstrap consensus trees (50-250 MFW) were built on random samples.   
- **'Diderot's question'**: Diderot's writings were separated in two sets - Diderot II (Correspondences littéraires) and Diderot I (the rest of the books). In all trials (incl. GI) these two sets by Diderot shows great closeness, proving the overall method to work on our data and with current sampling & processing methods.   
  
Main part of the analysis: *General Imposters*  
- For the main part of the analysis GI method was used (stylo implementation, cosine delta);
  - 100 trials of each GI test result in a coefficient between 0 and 1. This shows how many times an author was the closest to the text in question: 0 means 'in none of the trials', 1 means 'in all of the trials';  
- 100 iterations of GI test was performed, for each a new set of random independent samples was taken from the corpus;  
- The distribution of the 100 GI distributions (from 0 to 1) presented as box plots.  

## FP-1772
For the beginning the method's work on 'a ground truth' can be demonstrated.  
Here we use the text of *Fragments politiques* as it was published in Grimm’s *Correspondance littéraire* in 1772. This text ought to be detected as Diderot's one as the authorship is known from traditional sources. As this text with edits was included in the HDI as well, it is important if the actual method will show the similarity with Diderot's writings in this case.  
Below are the results from the GI analysis, the full analysis can be found at `03_0_FP`.  
  
![plot-FP-main](https://github.com/tonyamart/hdi/blob/main/scr/03_0_FP.markdown_strict_files/figure-markdown_strict/unnamed-chunk-14-1.png?raw=true)


## FV-Results
### Editions
#### Segments appeared in ed. 1770
Based on the current corpus, in all tests (both BTC and GI) these segments demonstrate closeness to d'Holbach's writings. This is the most robust result in all tests done so far.

![plot-1770-nch](https://github.com/tonyamart/hdi/blob/main/scr/03_1_analysis.markdown_strict_files/figure-markdown_strict/unnamed-chunk-25-1.png?raw=true)

The results for the fragments appeared in the 1770 and changed twice show a similar picture: it is highly likely that d'Holbach might be the author. Though the algorithm is slightly less sure about the fragments which appeared in 1774 but have been changed in the next editions.  

![plot-1770-ch-ch](https://github.com/tonyamart/hdi/blob/main/scr/03_1_analysis.markdown_strict_files/figure-markdown_strict/unnamed-chunk-31-1.png?raw=true)
![plot-1770-ch-nch](https://github.com/tonyamart/hdi/blob/main/scr/03_1_analysis.markdown_strict_files/figure-markdown_strict/unnamed-chunk-34-1.png?raw=true)

#### Segments appeared in ed. 1774
The situation with the fragments appeared in 1774 (which have not been edited later) is more intriguing as in this case the algorithm is not sure about the authorship of any author. The plot below shows that no author in the set is close enough in their word usage to the fragment in question. As some authors (as Pechmeja, Diderot, Baudeau and Riviere) sometimes have results higher than 40-50%, there is not enough evidence to say if any of them is a likely candidate. One of the explanation here could be that the fragments combined as "edition 1774 - not changed" are in fact belong to multiple authors from the set or outside it.  

![plot-1774-nch](https://github.com/tonyamart/hdi/blob/main/scr/03_1_analysis.markdown_strict_files/figure-markdown_strict/unnamed-chunk-38-1.png?raw=true)

#### Segments appeared in ed. 1780
In the case of the latest added fragments, the situation is mixed: on the one hand, there is some signs of similarity with d'Holbach's writings. However, the author's signal is much less clear in comparison with ed. 1770.

![plot-1780](https://github.com/tonyamart/hdi/blob/main/scr/03_1_analysis.markdown_strict_files/figure-markdown_strict/unnamed-chunk-42-1.png?raw=true)

### PD vs M
In the case of red pencil marks our results are quite similar to that of the edition of 1770. There is quite a strong signal of d'Holbach authorship, although here the algorithm is less sure than in case of fragments from the earlier editions. 

![plot-PD](https://github.com/tonyamart/hdi/blob/main/scr/03_1_analysis.markdown_strict_files/figure-markdown_strict/unnamed-chunk-48-1.png?raw=true)

The observations on the word usage in *Mélanges* are more questionable, as this selection leads us to the mixed-authorship: sporadically some text fragments  resemble some of the authors from our corpus. Nevertheless, we do not have an evidence strong enough to select one candidate in this case as well.

![plot-M](https://github.com/tonyamart/hdi/blob/main/scr/03_1_analysis.markdown_strict_files/figure-markdown_strict/unnamed-chunk-50-1.png?raw=true)
![plot-M-without-PD](https://github.com/tonyamart/hdi/blob/main/scr/03_2_analyasis_additional.markdown_strict_files/figure-markdown_strict/unnamed-chunk-30-1.png?raw=true)

### Separate fragments
The combined set of fragments about Russia and China shows no clear authorship signal. Nevertheless, the fragment 'Les avantages de la vie sauvage' hints to be close do Diderot (athought the result is not robust enough).  

![plot-sauvage](https://github.com/tonyamart/hdi/blob/main/scr/03_2_analyasis_additional.markdown_strict_files/figure-markdown_strict/unnamed-chunk-22-1.png?raw=true)
