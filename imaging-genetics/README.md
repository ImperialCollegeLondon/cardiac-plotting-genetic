3D Associations
===============

The visualisation of cardiac imaging phenotypes derived from UK Biobank
cardiovascular magnetic resonance (CMR) images across 50,000 subjects
and to enable the exploration of their links to other non-imaging
phenotypes that characterise the structure and function for the the left
ventricle (LV).  
Wall thickness (WT) and surface to surface (S2S) were calculated at over
40,000 points in the 3D model at end-diastole and were measured as the
distance between the endocardial and epicardial surfaces perpendicular
to the midwall plane and as the differences in the endocardial and
epicardial surfaces relative to an average cardiac shape, respectively.

The associations between one imaging phenotype (WT, S2S) and one
non-imaging phenotype for each point in the 3D datasets were assessed
using a linear regression model adjusted for Age, Sex, Race, BSA, SBP,
DBP.  
Multiple testing with Benjamini-Hochberg (BH) procedure to control from
false discovery rate (FDR) and the threshold-free cluster enhancement
(TFCE) technique to boost the areas of signal with spatial contiguity,
was used in the LV points with significant associations ([Biffi et al.,
2018](https://academic.oup.com/bioinformatics/article/34/1/97/4103396)).

From the table summarytable\_meta.txt,  
Step 1: 2709 people any rare PAV (syndromic or not)  
Step 2A: 774 people have a rare PAV in the 8 sarcomere genes  
Step 2B: 424 people have a rare PAV (FAF for HCM) variant in the 8
sarcomere genes  
Step 3A: 24 people have a P/LP (CC) rare (FAF) variant in the 8
sarcomere genes  
Step 3B: 18 people have a ClinVar HCM rare (FAF) P/LP variant in the 8
sarcomere genes  
Step 3A\_3B: 30 people have a P/LP (CC) rare (FAF) variant a ClinVar HCM
rare (FAF) P/LP variant in the 8 sarcomere genes  
Step 4: 12 people have a P/LP (CC), ClinVar, rare (FAF) variant in the 8
sarc genes  
were taken against  
Step 0 and hasanyCM: 9204 people have no rare PAV = 1 and has any CM
flag by Kathryn’s analysis |FALSE,  
and their associations with imaging phenotypes in the 3D LV model, were
explored from a subset of 12k subjects.  

PAV = protein altering variant  
FAF = filtering allele frequency  
P/LP = flagged as pathogenic or likely pathogenic  
CC = CardioClassifier  

    summarytable_meta <- read.table("summarytable_meta.txt", header = TRUE)
    head(summarytable_meta)

    ##   eid_47602 eid_18545 eid_40616 hasWithdrawn hasExomeData50k hasCMR isBritCauc
    ## 1   5283938   1001106   2975510        FALSE            TRUE   TRUE         NA
    ## 2   5866425   1001139   2777450        FALSE            TRUE   TRUE          1
    ## 3   1604555   1002696   5674019        FALSE            TRUE   TRUE          1
    ## 4   4971535   1003636   2645738        FALSE            TRUE   TRUE          1
    ## 5   3076504   1004066   1485039        FALSE            TRUE   TRUE          1
    ## 6   1839471   1004487   3114006        FALSE            TRUE   TRUE          1
    ##   hasKinship sexgenetic  yob autoSysBP     bmi agerecruit hasHCM_sx hasDCM_sx
    ## 1          0          1 1953       125 29.7036         54     FALSE     FALSE
    ## 2          1          0 1949       134 24.9479         58     FALSE     FALSE
    ## 3          0          0 1941       181 20.5235         66     FALSE     FALSE
    ## 4          0          1 1949       152 29.8659         60     FALSE     FALSE
    ## 5          0          0 1949       153 34.6499         60     FALSE     FALSE
    ## 6          0          1 1966       118 22.8571         43     FALSE     FALSE
    ##   hasHCM_k hasDCM_k hasanyCM_k passedQC_k flagstep0 flagstep1 flagstep2A
    ## 1    FALSE    FALSE      FALSE      FALSE         0         1          0
    ## 2    FALSE    FALSE      FALSE       TRUE         1         0          0
    ## 3    FALSE    FALSE      FALSE       TRUE         1         0          0
    ## 4    FALSE    FALSE      FALSE       TRUE         0         1          1
    ## 5    FALSE    FALSE      FALSE       TRUE         1         0          0
    ## 6    FALSE    FALSE      FALSE       TRUE         1         0          0
    ##   flagstep2B flagstep3A flagstep3B flagstep4 flagstep3A_3B
    ## 1          0          0          0         0             0
    ## 2          0          0          0         0             0
    ## 3          0          0          0         0             0
    ## 4          1          0          0         0             0
    ## 5          0          0          0         0             0
    ## 6          0          0          0         0             0

Step 1:
-------

2,523 people any rare PAV (syndromic or not) vs 8,696 people have no
rare PAV and no CM flag by Kathryn’s analysis (clean set of non imaging
phenotypes).

<style>
    iframe{
        border: 2px solid #ccc;
    }
</style>
<body>
    <iframe src="README_htmls/plot1.html" id="plot1" style="border:none;"
    scrolling="auto"></iframe>
    
    <script>
    var iframe = document.getElementById("plot1");
    iframe.onload = function(){
        iframe.style.width = iframe.contentWindow.document.body.scrollWidth + 'px';
        iframe.style.height = iframe.contentWindow.document.body.scrollHeight + 'px';
    }
    </script>
</body>

Step 2A:
--------

713 people have a rare PAV in the 8 sarcomere genes vs 8,696 people have
no rare PAV and no CM flag by Kathryn’s analysis (clean set of non
imaging phenotypes).
<style>
    iframe{
        border: 2px solid #ccc;
    }
</style>
<body>
    <iframe src="README_htmls/plot2A.html" id="plot2a" style="border:none;"
    scrolling="auto"></iframe>
    
    <script>
    var iframe = document.getElementById("plot2a");
    iframe.onload = function(){
        iframe.style.width = iframe.contentWindow.document.body.scrollWidth + 'px';
        iframe.style.height = iframe.contentWindow.document.body.scrollHeight + 'px';
    }
    </script>
</body>

Step 2B:
--------

395 people have a rare PAV (FAF for HCM) variant in the 8 sarcomere
genes vs 8,696 people have no rare PAV and no CM flag by Kathryn’s
analysis (clean set of non imaging phenotypes).


<style>
    iframe{
        border: 2px solid #ccc;
    }
</style>
<body>
    <iframe src="README_htmls/plot2B.html" id="plot2b" style="border:none;"
    scrolling="auto"></iframe>
    
    <script>
    var iframe = document.getElementById("plot2b");
    iframe.onload = function(){
        iframe.style.width = iframe.contentWindow.document.body.scrollWidth + 'px';
        iframe.style.height = iframe.contentWindow.document.body.scrollHeight + 'px';
    }
    </script>
</body>

Step 3A:
--------

23 people have a P/LP (CC) rare (FAF) variant in the 8 sarcomere genes
vs 8,696 people have no rare PAV and no CM flag by Kathryn’s analysis
(clean set of non imaging phenotypes).

<style>
    iframe{
        border: 2px solid #ccc;
    }
</style>
<body>
    <iframe src="README_htmls/plot3A.html" id="plot3a" style="border:none;"
    scrolling="auto"></iframe>
    
    <script>
    var iframe = document.getElementById("plot3a");
    iframe.onload = function(){
        iframe.style.width = iframe.contentWindow.document.body.scrollWidth + 'px';
        iframe.style.height = iframe.contentWindow.document.body.scrollHeight + 'px';
    }
    </script>
</body>

Step 3B:
--------

17 people have a ClinVar HCM rare (FAF) P/LP variant in the 8 sarcomere
genes vs 8,696 people have no rare PAV and no CM flag by Kathryn’s
analysis (clean set of non imaging phenotypes).


<style>
    iframe{
        border: 2px solid #ccc;
    }
</style>
<body>
    <iframe src="README_htmls/plot3B.html" id="plot3B" style="border:none;"
    scrolling="auto"></iframe>
    
    <script>
    var iframe = document.getElementById("plot3B");
    iframe.onload = function(){
        iframe.style.width = iframe.contentWindow.document.body.scrollWidth + 'px';
        iframe.style.height = iframe.contentWindow.document.body.scrollHeight + 'px';
    }
    </script>
</body>

Step 3A\_3B:
------------

28 people have a P/LP (CC) rare (FAF) variant a ClinVar HCM rare (FAF)
P/LP variant in the 8 sarcomere genes vs 8,696 people have no rare PAV
and no CM flag by Kathryn’s analysis (clean set of non imaging
phenotypes).


<style>
    iframe{
        border: 2px solid #ccc;
    }
</style>
<body>
    <iframe src="README_htmls/plot3AB.html" id="plot3ab" style="border:none;"
    scrolling="auto"></iframe>
    
    <script>
    var iframe = document.getElementById("plot3ab");
    iframe.onload = function(){
        iframe.style.width = iframe.contentWindow.document.body.scrollWidth + 'px';
        iframe.style.height = iframe.contentWindow.document.body.scrollHeight + 'px';
    }
    </script>
</body>

Step 4:
-------

12 people have a P/LP (CC), ClinVar, rare (FAF) variant in the 8 sarc
genes vs 8,696 people have no rare PAV and no CM flag by Kathryn’s
analysis (clean set of non imaging phenotypes).


<style>
    iframe{
        border: 2px solid #ccc;
    }
</style>
<body>
    <iframe src="README_htmls/plot4.html" id="plot4" style="border:none;"
    scrolling="auto"></iframe>
    
    <script>
    var iframe = document.getElementById("plot4");
    iframe.onload = function(){
        iframe.style.width = iframe.contentWindow.document.body.scrollWidth + 'px';
        iframe.style.height = iframe.contentWindow.document.body.scrollHeight + 'px';
    }
    </script>
</body>


Link to site: https://marjola89.github.io/imaging-genetics/

