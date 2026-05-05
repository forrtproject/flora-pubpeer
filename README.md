# ReadMe

## Aim
RepNote is the PubPeer integration of the FORRT Library of Replication Attempts (FLoRA). It is a proactive method to make researchers aware of replications of their own and others' research. For original studies for which there is a replication attempt in FLoRA, a publicly visible comment is posted on PubPeer.

## Funding
This project is funded by UK Research and Innovation (UKRI) as part of the Making Replications Count project (https://forrt.org/marco/). 

## FAQs
#### What is RepNote?
RepNote is a tool that makes replications more visible and linked to the original
research. We link the replications from the FORRT Library of Replication Attempts
(FLoRA) to original studies and create post-publication comments on [pubpeer.com](http://pubpeer.com)
containing information on replication and reproduction attempts (i.e., number of
replications, description of replication outcome, and success of replications). 

#### Why notify original authors?
When scientists discover something, we want to check the findings to make sure the results bear up. It benefits the scientific community and the public to make attempts to repeat studies easily visible. To make it transparent that a replication has been attempted and more information about the original study’s claims have been tested, we think a public approach to notification is important. Replications are a prerequisite for a robust research claim, and if these attempts to check original work are not rewarded or seen, unreliable findings will be built on. We think that notifying readers and authors of replication attempts makes relevant information more accessible and is useful for the scientific landscape.

#### Why did you email me about my article?
We emailed 100 original study authors before making the first round of comments to 
gather responses to the tool before it was widely implemented.

#### Why use PubPeer?
PubPeer is a public platform, accessible without barriers, where anonymous and
signed comments can be made on research articles. We use this platform because it
is the most transparent tool we have to link original and replication articles. 

#### How are the replication and reproduction articles found?
RepNote uses the FLoRA database of replications and reproductions.
 

One part of FLoRA is made up of the FORRT Replication Database (FReD), which includes the work of hundreds of people over many years. The FORRT Replication Database is a crowdsourced effort, which aimed to gather unpublished and published replication results to estimate and track replicability in social sciences. Studies were manually found or submitted and then double-coded by humans. For more information and to explore the database, click [here](https://forrt-replications.shinyapps.io/fred_explorer/#).

Additional studies are included in FLoRA that are not in the FORRT Replication Database. A systematic search of OpenAlex has been conducted, using the keyword “replication” with automated extraction with R code and manual validation by a human of extracted variables. This is continuing over time. The code used for automatic extraction can be found [here]. 

#### Which replications and reproductions does RepNote detect?
We consider an article to be a replication if it has been termed as such by the authors and includes a test of prior claim using (at least partially) new data. Replication articles were collated partly by hand and partly in an automated way, by the FORRT extraction team. 

Replication results are coded from the text of replication articles,
found either by submission (the FORRT Replication Database data) or by
systematic searching of OpenAlex for the term “replication”. Reproductions are taken from Replication Network list, and from the Institute for Replication reports.

#### How does RepNote create comments?
Using R code, which will be made available on the [project GitHub](https://github.com/forrtproject/flora-pubpeer/tree/main).

#### Why does RepNote say my outcome is successful/unsuccessful/mixed when I think it's not?
Replication outcome coding relies on what replication authors say in the abstract. If there is no abstract or summary, coders have to summarise themselves. As coders cannot know all topics in depth and weigh claims, we have to do it formally. That means, 9/10 successfully replicated claims would be coded as “mixed” since the one failed claim could be crucial for the overall conclusion. For more information on the dataset, see the about the dataset section. For more information on coding, see [“What is FLoRA?”](https://docs.google.com/document/d/1WhrXKNIoa3Y1ERpLmT5jowZHVEIGJTp3-FBlAYfGkQE/edit?tab=t.0).

#### Do you maintain a copy of analysed papers or results?
The FORRT Replication Database (FReD) database is openly available [here](https://forrt-replications.shinyapps.io/fred_explorer/#). The
FLoRA dataset is not openly available, but is maintained by the FORRT team.

#### Something is broken. Who can help me?
Please contact the RepNote Team on [this Google form](https://docs.google.com/forms/d/e/1FAIpQLSc3Qinv4xwp01JT3P9_6wyz4Hd5bqBYJr6RcvyMvWrP5hScZQ/viewform?usp=dialog).

#### Where can I find more information about RepNote?
The GitHub page [here](https://github.com/forrtproject/flora-pubpeer/tree/main) is where you can find RepNote's latest developments.

## About the dataset
RepNote uses the FORRT Library of Reproduction and Replication Attempts (FLoRA), which contains replications and reproductions of studies from many different areas of science.

Replications are studies that intentionally repeat prior research to test whether the original findings hold. To be included in FLoRA, a study must:

* *Self-identify as a replication* (e.g., “replication of Author (Year)”) before reporting results — replication must be an aim, not just a result. Identify specific target study/studies that it replicates. Replicate a study or experiment, not just a single association or finding.
* *Replications can range from close/direct (same methods, same population) to conceptual (testing the same hypothesis with different methods)*, as long as the above criteria are met. The plugin tags replication outcomes as Successful, Failed, or Mixed, based on how the replication authors characterise their results, usually in the abstract. 

**Replication outcome coding relies on what replication authors say in the abstract. If this information was not available in the abstract, coders were directed to check the results section and supplementary materials.** If there is no abstract or summary, coders have to summarise themselves. As coders cannot know all topics in depth and weigh claims, we have to do it formally. That means, 9/10 successfully replicated claims would be coded as “mixed” since the one failed claim could be crucial for the overall conclusion. If nothing is in abstract, discussion, or conclusion, then the study is coded as "descriptive only".

Reproductions are attempts to computationally verify whether reported results can be obtained from the original study’s data and methods. Reproductions are coded along two dimensions:

* *Computational success:* Were the original results obtained? (Computationally Successful vs Computational Issues)
Robustness: Do results hold under reasonable alternative specifications? (Robust, Robustness Challenges, or Robustness Not Checked)
* *Key distinction:* **If new data are collected or used (e.g., an additional decade of data), it is a replication. If the same data are re-analysed to verify the original results, it is a reproduction.**

## Team (alphabetically)
Prasad Chandrashekar, Ze Freeman, Lukas Wallrich, and Josefina Weinerova created the tool. RepNote is maintained by the FORRT community and collaborators, including Lukas Röseler, Lukas Wallrich, and Josefina Weinerova.

## Contact
Please contact the RepNote Team on [this Google form](https://docs.google.com/forms/d/e/1FAIpQLSc3Qinv4xwp01JT3P9_6wyz4Hd5bqBYJr6RcvyMvWrP5hScZQ/viewform?usp=dialog).
