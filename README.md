# Pubpeer Comments on Replications and Reproductions

## Aim
RepNote is the PubPeer integration of the FORRT Library of Replication Attempts (FLoRA). It is a proactive method to make researchers aware of replications of their own and others' research. For original studies for which there is a replication attempt in FLoRA, a publicly visible comment is posted on PubPeer.

## Funding
This project is funded by UK Research and Innovation (UKRI) as part of the Making Replications Count project (https://forrt.org/marco/). 

## FAQs
<details>
<summary>What is RepNote?</summary>

<br>RepNote is a tool that makes replications more visible and linked to the original
research. We link the replications from the FORRT Library of Replication Attempts
(FLoRA) to original studies and create post-publication comments on [pubpeer.com](http://pubpeer.com)
containing information on replication and reproduction attempts (i.e., number of
replications, description of replication outcome, and success of replications).<br><br> 
</details>

<details>
<summary>Why notify original authors?</summary>

<br>To make it transparent that a replication has been attempted and more information about the original study’s claims have been tested, we think a public approach to notification is important.<br>

Replications are a prerequisite for a robust research claim, and if these attempts to check original work are not rewarded or seen, unreliable findings will be built on. We think that notifying readers and authors of replication attempts makes relevant information more accessible and is useful for the scientific landscape.<br><br>

</details>

<details>
<summary>Why use PubPeer?</summary>
<br>PubPeer is a public platform, accessible without barriers, where anonymous and
signed comments can be made on research articles. We are not affiliated with PubPeer but we use this platform because it
is the most transparent tool we have to link original and replication articles. There is
no current infrastructure that links original articles to their replication, as citations on
articles can only point backwards to previously completed work.<br><br> 

</details>

<details>
<summary>How are the replication and reproduction articles found?</summary>

<br>RepNote uses the FORRT Library of Reproduction and Replication Attempts.<br>

One part of FLoRA is made up of the FORRT Replication Database (FReD), which includes the work of hundreds of people over many years. The FORRT Replication Database is a crowdsourced effort, which aimed to gather unpublished and published replication results to estimate and track replicability in social sciences. Studies were manually found or submitted and then double-coded by humans. For more information and to explore the database, click [here](https://forrt-replications.shinyapps.io/fred_explorer/#).

Additional studies are included in FLoRA that are not in the FORRT Replication Database. A systematic search of OpenAlex has been conducted, using the keyword “replication” with automated extraction with R code and manual validation by a human of extracted variables. This is continuing over time. Reproductions are taken from the [Replication Network list](https://replicationnetwork.com/), and from the [Institute for Replication reports](https://i4replication.org/reports/?cpt=replication-report).<br><br> 
</details>

<details>
<summary>Which replications and reproductions does RepNote detect?<br></summary>

<br>We consider an article to be a replication if it has been termed as such by the authors and includes a test of prior claim using (at least partially) new data. Replication articles were collated partly by hand and partly in an automated way, by the FORRT extraction team. 

Replication results are coded from the text of replication articles,
found either by submission (the FORRT Replication Database data) or by
systematic searching of OpenAlex for the term “replication”. Reproductions are taken from [Replication Network list](https://replicationnetwork.com/), and from the [Institute for Replication reports](https://i4replication.org/reports/?cpt=replication-report).<br><br>
</details>

<details>
<summary>How does RepNote create comments?</summary>

<br>Using R code, which will be made available on the [project GitHub](https://github.com/forrtproject/flora-pubpeer/tree/main).<br><br>

</details>

<details>
<summary>Why does RepNote say my outcome is successful/unsuccessful/mixed when I think it's not?</summary>

<br>We rely on what replication authors say in the abstract or the report, subject to our interpretation of this. If studies are part of a meta-paper, we still rely on the individual reports where available, and otherwise on the main criterion used by the paper authors. 

For more information on the dataset, see the about the dataset section. For more information on coding, see [the info page](https://forrt.org/replication-hub/flora/).<br><br>

</details>

<details>
<summary>Do you maintain a copy of analysed papers or results?</summary>

<br>The FORRT Replication Database database (FReD)is openly available [here](https://forrt.org/explorer/).  
The FORRT Library of Reproduction and Replication Attempts (FLoRA) is openly available [here](https://forrt.org/replication-hub/flora/) and can be browsed in the [FLoRA Explorer](https://forrt.org/flora-explorer/). The data can be downloaded under a CC-BY 4.0 license [here](https://github.com/forrtproject/FReD-data/blob/main/output/flora.csv).<br><br>

</details>

<details>
<summary>Something is broken. Who can help me?</summary>

<br>
Please contact the RepNote Team on <a href="https://docs.google.com/forms/d/e/1FAIpQLSdLePtIlPxwiKQuEZv-LIsgn77oMRlFdEwyAoKm0F1qRwaRCA/viewform?usp=header">this Google form</a>.<br><br>

</details>

<details>
<summary>Where can I find more information about RepNote?</summary>

<br>
The GitHub page <a href="https://github.com/forrtproject/flora-pubpeer/tree/main">here</a> is where you can find RepNote's latest developments.<br><br>

</details>

## About the dataset
RepNote uses the FORRT Library of Reproduction and Replication Attempts (FLoRA), which contains replications and reproductions of studies from many different areas of science.

Replications are studies that intentionally repeat prior research to test whether the original findings hold. To be included in FLoRA, a study must:

* *Self-identify as a replication* (e.g., “replication of Author (Year)”) before reporting results — replication must be an aim, not just a result. Identify specific target study/studies that it replicates. Replicate a study or experiment, not just a single association or finding.
* *Replications can range from close/direct (same methods, same population) to conceptual (testing the same hypothesis with different methods)*, as long as the above criteria are met. The plugin tags replication outcomes as Successful, Failed, or Mixed, based on how the replication authors characterise their results, usually in the abstract. 

**Replication outcome coding relies on what replication authors say in the abstract. If this information was not available in the abstract, coders were directed to check the results section and supplementary materials.** If there is no abstract or summary, coders have to summarise themselves. As coders cannot know all topics in depth and weigh claims, we have to do it formally. That means, 9/10 successfully replicated claims would be coded as “mixed” since the one failed claim could be crucial for the overall conclusion. If nothing is in abstract, discussion, or conclusion, then the study is coded as "descriptive only", indicating that the study explicitly did not aim to test the veracity of the original claim. This is very rare, and applies to e.g. qualitative replications, repeated reviews or a study that "replicated" an analysis of the composition of 911 calls in a different place and time. 

Reproductions are attempts to computationally verify whether reported results can be obtained from the original study’s data and methods. Reproductions are coded along two dimensions:

* *Computational success:* Were the original results obtained? (Computationally Successful vs Computational Issues)
Robustness: Do results hold under reasonable alternative specifications? (Robust, Robustness Challenges, or Robustness Not Checked)
* *Key distinction:* **If new data are collected or used (e.g., an additional decade of data), it is a replication. If the same data are re-analysed to verify the original results, it is a reproduction.**

## Team (alphabetically)
RepNote was made by Prasad Chandrashekar, Ze Freeman, Lukas Röseler, Lukas Wallrich, and Josefina Weinerova. The full list of FORRT community and contributors to FReD can be found [here](https://forrt.org/contributors/?project=fred-forrt-replication-database&collapse-filter) and FLoRA can be found [here](https://forrt.org/contributors/?project=flora-forrt-library-of-replication-attempts).

## Cite as
Chandrashekar, P., Freeman, Z., Weinerova, J., Röseler, L., & Wallrich, L. (2026). RepNote: Pubpeer Comments on Replications and Reproductions. Zenodo. https://doi.org/10.5281/zenodo.20069918

## Contact
Please contact the RepNote Team on [this Google form if you find issues in the data](https://docs.google.com/forms/d/e/1FAIpQLSeH8s--VyLYdwhER_pdO_37gpwHDp5hyWF3mGUolOOBKdc7_w/viewform) and [this Google form for general feedback](https://docs.google.com/forms/d/e/1FAIpQLSc3Qinv4xwp01JT3P9_6wyz4Hd5bqBYJr6RcvyMvWrP5hScZQ/viewform?usp=dialog).
