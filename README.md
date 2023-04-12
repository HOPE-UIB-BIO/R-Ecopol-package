# REcopol package:package:<img src="man/figures/REcopol_logo.png" align="right" width="400" />

<!-- badges: start -->
<!-- badges: end -->

The goal of {`REcopol`} is to make an easy-to-use function to analyse fossil pollen data

## Installation

You can install the development version of REcopol:package: from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("HOPE-UIB-BIO/R-Ecopol-package")
```

## Usage

The main purpose of {REcopol} is to provide tools for easy-to-use analyses of fossil pollen data. All functions can be grouped into 5 categories:

1. **Diversity estimations** - Functions to estimates diversity from community data using various methods, including taxonomic, functional. and phylogenetic diversity.
    - [diversity_estimate()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/diversity_estimate.html)
    - [diversity_estimate_taxonomic()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/diversity_estimate_taxonomic.html)
    - [diversity_estimate_functional()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/diversity_estimate_functional.html)
    - [diversity_estimate_phylogenetic()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/diversity_estimate_phylogenetic.html)
2. **Ordinations** - Functions to fit Detrended (Canonical)
    Correspondence Analysis.
    - [fit_ordination()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/fit_ordination.html)
    - [fit_ordination_dca()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/fit_ordination_dca.html)
    - [fit_ordination_dcca()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/fit_ordination_dcca.html)
3. **GAM modeling** - Functions to fit, predict, and visualise generalized additive models (GAM) models.
    - [fit_custom_gam()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/fit_custom_gam.html)
    - [fit_hgam()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/fit_hgam.html)
    - [fit_multiple_gams()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/fit_multiple_gams.html)
    - [predic_model()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/predic_model.html)
    - [plot_temporal_trend()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/plot_temporal_trend.html)
4. **Change-point analyses and regression partition** - Functions for detection of change points in temporal patterns
    - [regression_partition()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/regression_partition.html)
    - [mv_regression_partition()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/mv_regression_partition.html)
    - [get_change_points()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/get_change_points.html)
    - [get_change_points_all()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/get_change_points_all.html)
    - [add_data_partition()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/add_data_partition.html)
5. **Utility** - Functions for operation with (not only) fossil pollen data
    - [add_age_bin()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/add_age_bin.html)
    - [transfer_into_proportions()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/transfer_into_proportions.html)
    - [tranform_percentage_data()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/tranform_percentage_data.html)
    - [get_density()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/get_density.html)

## Get in touch

We would love to build a community around R-Ecopol and there are various ways to get in touch and grow together.

We have set up [GitHub Discussions](https://github.com/HOPE-UIB-BIO/R-Ecopol-package/discussions) as a main hub for communication. Do you have a question? Have you thought about a cool new feature? Let's write us a message. We hope that the **Discussions** page will serve as a line of communication to the developers as well as between various users.

### Package future updates

The R-Ecopol package is envisioned as software that will undergo updates for further improvement.

We are aware of the functions and features we would like to implement in the future.

See the planned future updates in the **[package future updates](https://github.com/orgs/HOPE-UIB-BIO/projects/4/views/1)** The three stages of request are:

- "next version" - a feature that will be implemented in the next R-Ecopol release

- "future" - a feature that will be probably implemented in one of the future R-Ecopol releases

- "in consideration" - the feature might be implemented but it is not a priority

If there is a feature you would like to implement, please first check the [Issue Tracker](https://github.com/HOPE-UIB-BIO/R-Ecopol-package/issues) and look if someone already suggested it and ***upvote*** it, if it is already there. Before each version release, we will implement the most upvoted feature.

We aim to regularly update the list.

### "It does not work!"

No software is without problems and if you find a nasty bug while using this workflow, please use the [Issues page](https://github.com/HOPE-UIB-BIO/R-Ecopol-package/issues) to report it.

Consider the following steps before and when opening a new Issue:

1. Have you or someone else asked about it at the [GitHub Discussions](https://github.com/HOPE-UIB-BIO/R-Ecopol-package/discussions)? The "Q&A" section is perfect for that!

2. Have you checked that similar issues may have been already reported? The issue tracker has a filter function to search for keywords in open Issues. You can narrow down the search using `labels`:label: as filters. See [Labels](https://docs.github.com/en/issues/using-labels-and-milestones-to-track-work/managing-labels) for more information. As a general rule, we don't assign issues to anyone.

To open a new Issue:

1. Click on the green **New issue** button in the upper right corner, and select **Bug report**.

2. Describe your problem in as much detail as possible. The issue should state what the problem is, what the expected behaviour should be, and, maybe, suggest a solution. Note that you can also attach files or images to the issue.

3. Select a suitable `label`:label: from the drop-down menu called **Labels**.

4. Click on the green **Submit new issue** button and wait for a reply.

### "Can we add this?"

We also use the [Issues page](https://github.com/HOPE-UIB-BIO/R-Ecopol-package/issues) for serious feature requests. If some discussion [GitHub Discussions](https://github.com/HOPE-UIB-BIO/R-Ecopol-package/discussions) portal leads to a flashed-out feature, you would like to implement, you can submit it as a *feature request*:

1. Go to the [Issues page](https://github.com/HOPE-UIB-BIO/R-Ecopol-package/issues)

2. Click on the green **New issue** button in the upper right corner, and select **Feature request**.

3. Describe your feature as detailed as possible. What is the expected behaviour? What packages we should use? Note that you can also attach files or images to the Issue.

4. Select a suitable `label`:label: from the drop-down menu called **Labels**.

5. Click on the green **Submit new issue** button and wait for a reply.

### Contribute

The R-Ecopol package is envisioned as software that will undergo updates for further improvement.

We appreciate the help :sparkling_heart: and thank you just for considering contributing to R-Ecopol.

To make sure that we maintain the highest quality of code, we do have to adhere to some strict guidelines though. Please read through this document to help you get up and running.

If you would like to report a bug, suggest enhancements, or request a new feature, jump to the [Issues section](#it-does-not-work).

#### Git + GitHub

We use the [Git](https://git-scm.com/) version control system to manage the development with the repository hosted on [GitHub](https://github.com). If you are new to Git or GitHub, please read through the [GitHub Bootcamp](https://help.github.com/categories/bootcamp/) to get up to speed.

If you're already familiar with Git and GitHub, please read [Submitting Pull Requests](#submitting-pull-requests).

#### Coding Style Guidelines

While we do have our style in coding and haven't followed any standards available
on the web, we do maintain some uniformity.

If we missed mentioning a particular case, you should always follow the below
procedure:

- See how it's done in the codebase.
- See what [Advanced R by Hadley Wickham](http://adv-r.had.co.nz/Style.html) convention says and choose something that is close to the codebase.
- If all else fails, ask on [GitHub Discussions](https://github.com/HOPE-UIB-BIO/R-Ecopol-package/discussions) :)

#### Submitting Pull Requests

All changes to R-Ecopol must be in the form of a **pull request** (also known as a PR). If you are unfamiliar with pull requests, please read [this](https://git-scm.com/book/en/v2/GitHub-Contributing-to-a-package>).

Here is the recommended process:

1. Fork the repo so that you can make your changes without affecting the original package until you're ready to merge them. Check out the [Guide to forking](https://docs.github.com/en/get-started/quickstart/fork-a-repo#fork-an-example-repository)

2. Check out the branch (named the next version; if there is one).

3. Commit your updates once you are happy with them. See contributing [guide](https://github.com/atom/atom/blob/master/CONTRIBUTING.md#git-commit-messages) for commit messages.

4. When you're finished with the changes, create a PR
   - Click the "Ready for review" so that we can review your PR. This template helps reviewers understand your changes as well as the purpose of your pull request.
   - Don't forget to [link PR to the Issue](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue) if you are solving one.
   - Enable the checkbox to [allow maintainer edits](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks/allowing-changes-to-a-pull-request-branch-created-from-a-fork) so the branch can be updated for a merge. Once you submit your PR, a HOPE team member will review your proposal. We may ask questions or request additional information.
   - We may ask for changes to be made before a PR can be merged, either using suggested changes or pull request comments. You can apply suggested changes directly through the UI. You can make any other changes in your fork, and then commit them to your branch.
As you update your PR and apply changes, mark each conversation as [resolved](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/reviewing-changes-in-pull-requests/commenting-on-a-pull-request#resolving-conversations)
   - If you run into any merge issues, check out this [git tutorial](https://lab.github.com/githubtraining/managing-merge-conflicts) to help you resolve merge conflicts and other issues.

Before submitting a pull request, please make sure you follow all the guidelines
below while working on your changes:

- Each pull request should try to accomplish one general task.
- All work should be done on a branch with a descriptive name relating to the
  general task (eg. `fix_bug_x` or `add_feature_y`). Each commit should accomplish one small sub-task and should be explainable in a sentence or two.
- Each commit should have a descriptive commit message.
- You should make sure your code passes all tests before committing.
